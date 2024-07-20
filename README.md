# Precabal

*Precabal* is a simple macro preprocessor intended to be used to automatically generate `.cabal` files from 

- cabal file fragments, and
- package dependency bound specification file.

*Precabal* aims to address the following issues in .cabal files:

1) Repeating package version bounds for multi-unit packages.
2) Split common sections into a separate files, which allows sharing them across projects.
3) Facilitate cabal file management in multi-package projects.

Use `precabal --help` to see the command-line syntax.
Use the supplied `autogen` script to generate all `.cabal` files of your project from the `.cabal.in` templates.

See the included cabal file fragments for overview of the macro expansion syntax.
