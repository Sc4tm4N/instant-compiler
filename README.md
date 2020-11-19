# Instant Compiler

It's simple compiler written in Haskell.

Better description (in depth can be found in [PL]README but only in polish)

## Prerequisites

`cabal` is required

## Usage

In main catalogue:
`make`

2 executables (`jvm or insc_jvm` and `llvm or insc_llvm`) should be created, and copied to main catalog.
 If they're not present try to search in dist.

These executables require as first param text file with code in Instant language.

Execution of `jvm` ends up with some `.class` file. Simply type `java ${filename}` to test.

Execution of `llvm` ends up with some `.bc` file. Simply type `lli ${filename}.bc` to test.
