module Main where

import Options.Applicative

import CmdLine (writeOutput)
import CmdLineArgs (argParser)


main :: IO ()
main = execParser parserInfo >>= writeOutput
  where
    parserInfo = info (argParser <**> helper)
      ( fullDesc
      <> header "tlc - Compiler for a tiny language"
      )
