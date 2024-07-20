-- |
-- Module      : Main
-- Description : Precdabal implementation
-- Copyright   : Copyright (C) 2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Main (main) where

import Control.Applicative qualified as A
import Control.Exception
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable (fold)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Options.Applicative qualified as O
import System.Exit
import System.File.OsPath
import System.IO (stderr)
import System.IO.Error (isDoesNotExistError)
import System.OsPath
import Text.Megaparsec as MP

-- | Command-line options.
data CliOptions = CliOptions
    { packageBoundsFile :: Maybe FilePath,
      inputFile :: FilePath,
      outputFile :: Maybe FilePath,
      includeDirs :: [FilePath]
    }

-- | Mapping from variable names to their expansion strings
type VariableMap = HM.HashMap T.Text T.Text

-- | Program configuration is otherwise identical to 'CliOptions' except that OsPaths are used instead of FilePaths and all Maybe values have been resolved.
data Config = Config
    { includeDirs :: [OsPath],
      -- | Stack of included files
      includedFiles :: NE.NonEmpty OsPath,
      -- | Mapping from variable names to their expansion strings
      variables :: VariableMap
    }
    deriving (Show)

data ErrorMessage = ErrorMessage T.Text
    deriving (Show)

instance Exception ErrorMessage

main :: IO ()
main = do
    let action = precabalMain
        handler = \(ErrorMessage msg) -> do
            T.hPutStrLn stderr msg
            exitWith $! ExitFailure 1
     in action `catch` handler

precabalMain :: IO ()
precabalMain = do
    cli <- O.execParser cliOptionsInfo

    inputFile <- encodeFS cli.inputFile
    outputFile <- case cli.outputFile of
        Just out -> encodeFS out
        Nothing -> do
            if isExtensionOf [osp|cabal.in|] inputFile
                then pure $! dropExtension inputFile
                else signalError $ "Input file name does not end in .cabal.in, you need to specify output file name"
    packageBoundsFile <- case cli.packageBoundsFile of
        Just bounds -> encodeFS bounds
        Nothing -> pure $! dropFileName inputFile </> [osp|package-bounds.txt|]
    includeDirs <- traverse (fmap normalise . encodeFS) cli.includeDirs

    vars <- withUtf8File parsePackageBounds packageBoundsFile
    let config =
            Config
                { includeDirs = includeDirs,
                  includedFiles = NE.singleton $! normalise inputFile,
                  variables = vars
                }
    contents <- withUtf8File (parsePrecabal config) inputFile
    writeFile' outputFile (T.encodeUtf8 contents)

signalError :: MonadIO m => T.Text -> m a
signalError msg = liftIO $! throwIO $! ErrorMessage msg

cliOptionsInfo :: O.ParserInfo CliOptions
cliOptionsInfo =
    O.info
        (O.helper <*> programOptions)
        ( O.fullDesc
            <> O.progDesc "Pre-process cabal files"
            <> O.header "Expand #include directives and package version bound macros in Cabal files."
        )

programOptions :: O.Parser CliOptions
programOptions = do
    inputFile <-
        O.strArgument
            ( O.metavar "INPUT-FILE"
                <> O.help "Input .precabal file"
            )
    outputFile <-
        optional $
            O.strOption
                ( O.short 'o'
                    <> O.long "utput-file"
                    <> O.metavar "OUTPUT-FILE"
                    <> O.help "Output .cabal file to be generated"
                )
    packageBoundsFile <-
        optional $
            O.strOption
                ( O.short 'b'
                    <> O.long "bounds-file"
                    <> O.metavar "FILE"
                    <> O.help "Package version bounds definition file"
                )
    includeDirs <-
        A.many $
            O.strOption
                ( O.short 'I'
                    <> O.long "include-dir"
                    <> O.metavar "DIRECTORY"
                    <> O.help "A directory to look for #included files (may be specified multiple times)"
                )
    pure CliOptions {..}

withUtf8File :: (FilePath -> T.Text -> IO a) -> OsPath -> IO a
withUtf8File f fp = do
    contents <- (readFile' fp >>= pure . T.decodeUtf8)
    fpString <- decodeFS fp
    f fpString contents

handleParseError :: MonadIO m => Either (ParseErrorBundle T.Text Void) a -> m a
handleParseError = \case
    Left err -> signalError $! T.pack . errorBundlePretty $! err
    Right r -> pure $! r

parsePackageBounds :: FilePath -> T.Text -> IO (VariableMap)
parsePackageBounds fp text =
    handleParseError $! runParser (variableMap HM.empty) fp text
  where
    variableMap :: VariableMap -> Parsec Void T.Text VariableMap
    variableMap !vars = do
        skipMany (skipSpaceLF1 <|> skipComment)
        (variableBinding vars >>= variableMap) <|> (eof *> pure vars)

    variableBinding :: VariableMap -> Parsec Void T.Text VariableMap
    variableBinding vars =
        label "variable binding" $! do
            name <- fmap fold $! some (parseUnquotedString <|> parseQuotedString)
            when (HM.member name vars) $ failParser $ "Attempt to redefine `" <> name <> "`"
            value <- skipSpace *> parseWord <* skipSpace
            (void $! single '\n') <|> (single '$' *> skipComment)
            pure $! HM.insert name (name <> " " <> value) vars

    parseWord :: Parsec Void T.Text T.Text
    parseWord = label "compound word" $! fmap fold $! some (unquotedText <|> parseQuotedString)

    unquotedText :: Parsec Void T.Text T.Text
    unquotedText = do
        txt <- takeWhile1P (Just "unquoted text") (\ch -> ch >= ' ' && ch /= '"' && ch /= '\'')
        eol <- (lookAhead (single '\n') *> pure True) <|> pure False
        pure $!
            if eol
                then T.dropWhileEnd (== ' ') txt
                else txt

-- | Type for the template parser
type Parser a = ReaderT Config (ParsecT Void T.Text IO) a

-- | Parse precabal text and expand directives and variables
-- The String argument is used only as the source name – data is not read from the file.
parsePrecabal :: Config -> FilePath -> T.Text -> IO T.Text
parsePrecabal config fp text =
    runParserT (runReaderT parseTopLevel config) fp text >>= handleParseError

-- | Top-level precabal parser
parseTopLevel :: Parser T.Text
parseTopLevel = do
    ts <- someTill parseTextLine eof
    pure $! fold ts

parseTextLine :: Parser T.Text
parseTextLine = do
    -- We should now be at the beginning of a line. Check if we have a line
    -- consisting of just whitespace and a comment, and if so, skip the line.
    spaces <- takeWhileP (Just "text") (\ch -> ch == '\t' || ch == ' ')
    commentLine <-
        (MP.try $! single '$' *> skipComment *> (optional $! single '\n') *> pure True)
            <|> pure False
    if commentLine
        then pure ""
        else do
            txt <- fmap fold $! many $! textChunk <|> parseExpansion
            void $! optional (single '\n')
            pure $! spaces <> txt <> "\n"
  where
    textChunk :: Parser T.Text
    textChunk = takeWhile1P (Just "text") (\ch -> ch /= '$' && ch /= '\n')

parseExpansion :: Parser T.Text
parseExpansion =
    label "expansion" $!
        single '$'
            *> choice
                [ parseCommand,
                  parseVariable,
                  parseEscapedChar,
                  skipComment *> pure ""
                ]

parseCommand :: Parser T.Text
parseCommand =
    label "command" $!
        between
            (single '(')
            (single ')')
            (sepBy parseName skipSpaceLF1)
            >>= \case
                command : args -> dispatchCommand command args
                [] -> pure ""

dispatchCommand :: T.Text -> [T.Text] -> Parser T.Text
dispatchCommand command args =
    case command of
        "include-file" -> parseIncludeFile args
        _ -> failParser $ "Unknown command " <> command <> "`"

parseIncludeFile :: [T.Text] -> Parser T.Text
parseIncludeFile [fileName] = do
    config <- ask
    let recursionLimit = 32
    when (length config.includedFiles >= recursionLimit) $
        failParser $
            "Maximum recursion depth of " <> (T.pack . show) recursionLimit <> " reached"
    fp <- liftIO $! encodeFS (T.unpack fileName)
    let currentDir = takeDirectory . NE.head $! config.includedFiles
    result <- liftIO $! tryInclude config $ (normalise $! currentDir </> fp) : fmap (</> fp) config.includeDirs
    either failParser pure result
  where
    tryInclude :: Config -> [OsPath] -> IO (Either T.Text T.Text)
    tryInclude config (file : files) =
        let includedFiles' = NE.cons file config.includedFiles
            config' = config {includedFiles = includedFiles'}
            action = fmap Right $! withUtf8File (parsePrecabal config') file
            handler (exc :: IOException) =
                if isDoesNotExistError exc
                    then tryInclude config files
                    else throwIO exc
        in  if file `notElem` config.includedFiles
                then action `catch` handler
                else
                    pure . Left $
                        "Recursive includes: "
                            <> T.intercalate ", " (fmap (T.pack . show) $ NE.toList includedFiles')
    tryInclude _ [] = fail $! "Could not find include file `" <> T.unpack fileName <> "`"
parseIncludeFile _ = failParser $ "include-file command takes exactly one argument"

skipSpaceLF1 :: MonadParsec Void T.Text m => m ()
skipSpaceLF1 = void $ takeWhile1P (Just "space") (\ch -> ch == '\n' || ch == ' ')

skipSpace :: MonadParsec Void T.Text m => m ()
skipSpace = void $! takeWhileP (Just "space") (\ch -> ch == ' ')

parseVariable :: Parser T.Text
parseVariable = do
    name <- label "variable expansion" $! between (single '{') (single '}') parseName
    vars <- asks (.variables)
    case HM.lookup name vars of
        Just expn -> pure $! expn
        Nothing -> failParser $ "Undefined variable `" <> name <> "`"

parseName :: Parser T.Text
parseName =
    fmap fold $!
        some $!
            choice
                [ parseQuotedString,
                  parseUnquotedString,
                  parseExpansion
                ]

parseQuotedString :: forall m. MonadParsec Void T.Text m => m T.Text
parseQuotedString = do
    quote <- oneOf ['"', '\'']
    contents <- fmap fold $! many (rawText quote <|> escape)
    void $! single quote
    pure $! contents
  where
    rawText :: Char -> m T.Text
    rawText quote = takeWhile1P (Just "character sequence") (\ch -> ch >= ' ' && ch /= '\\' && ch /= quote)

    escape :: m T.Text
    escape =
        label "escape sequence" $!
            single '\\' *> anySingle
                >>= \case
                    '"' -> pure "\""
                    '\'' -> pure "'"
                    'n' -> pure "\n"
                    '\\' -> pure "\\"
                    ch -> failParser $ "Unsupported escape sequence: \\" <> T.singleton ch

parseUnquotedString :: MonadParsec Void T.Text m => m T.Text
parseUnquotedString =
    takeWhile1P
        (Just "identifier character sequence")
        (\ch -> ch > ' ' && ch `notElem` ['"', '$', '\'', '(', ')', '[', '\\', ']', '{', '|', '}'])

parseEscapedChar :: Parser T.Text
parseEscapedChar =
    label "escaped character" $!
        ( (T.singleton <$> single '$')
            <|> (single '\n' *> pure "")
        )

skipComment :: MonadParsec Void T.Text m => m ()
skipComment =
    void $! label "comment" $! chunk "--" *> takeWhileP Nothing (\ch -> ch /= '\n')

failParser :: MonadParsec Void T.Text m => T.Text -> m a
failParser msg = fancyFailure $! Set.singleton (ErrorFail $! T.unpack msg)
