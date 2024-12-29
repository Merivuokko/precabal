# Precabal

*Precabal* is a simple macro preprocessor intended to be used to automatically generate `.cabal` files from 

- cabal file fragments, and
- package dependency bound specification file.

*Precabal* aims to address the following issues with .cabal files:

1) Repeating package version bounds for multi-unit packages.
2) Split common sections into separate files, allowing them to be shared across projects.
3) Facilitate cabal file management in multi-package projects.

## Usage

See the included cabal file fragments (in `cabal/`subdirectory) for overview of the macro expansion syntax.
Use the supplied `autogen` script to generate all `.cabal` files of your project from the `.cabal.in` templates.
Use `precabal --help` to see the command-line syntax.
