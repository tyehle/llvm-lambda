{-# LANGUAGE LambdaCase #-}

module CmdLineArgs where

import Options.Applicative


data OutputFormat
  = Parsed     -- ^ Expr
  | Typed      -- ^ Types
  | Compiled   -- ^ LowLevel
  | Normalized -- ^ ANorm
  | LLVM       -- ^ LLVM IR
  | ASM        -- ^ Assembly file
  | OBJ        -- ^ Object file
  | EXE        -- ^ Linked executable file
  | Run        -- ^ Output of executable
  deriving (Eq, Show)


data Args = Args
  { debugRuntime :: Bool
  , outputFile :: Maybe String
  , outputFormat :: OutputFormat
  , optimizationFlag :: Maybe String
  , inputFile :: Maybe String
  } deriving(Eq, Show)


defaultArgs :: Args
defaultArgs = Args
  { debugRuntime = False
  , outputFile = Nothing
  , outputFormat = Run
  , optimizationFlag = Nothing
  , inputFile = Nothing
  }


argParser :: Parser Args
argParser = Args
  <$> switch
      ( long "debug-runtime"
      <> help "Print debug output from the runtime"
      )
  <*> optional (strOption
      ( long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Output filename"
      ))
  <*> option outputFormatReader
      ( long "format"
      <> short 'f'
      <> metavar "FORMAT"
      <> value (outputFormat defaultArgs)
      <> help "The output format. One of [parsed,typed,compiled,normalized,llvm,\
              \asm,obj,exe,run]. Default is run."
      )
  <*> optional optFlagParser
  <*> optional (argument str
      ( metavar "FILE"
      <> help "File to compile. If not specified code is read from stdin"
      ))


outputFormatReader :: ReadM OutputFormat
outputFormatReader = maybeReader $ \case
  "parsed" -> Just Parsed
  "typed" -> Just Typed
  "compiled" -> Just Compiled
  "normalized" -> Just Normalized
  "llvm" -> Just LLVM
  "asm" -> Just ASM
  "obj" -> Just OBJ
  "exe" -> Just EXE
  "run" -> Just Run
  _ -> Nothing


optFlagParser :: Parser String
optFlagParser = option levelReader
  ( long "optimization"
  <> short 'O'
  <> metavar "LEVEL"
  <> help "Level of optimization to use. Passed to clang and opt. One of [0,1,2,3,s,z]"
  )
  where
    levelReader = maybeReader $ \case
      "0" -> Just "-O0"
      "1" -> Just "-O1"
      "2" -> Just "-O2"
      "3" -> Just "-O3"
      "s" -> Just "-Os"
      "z" -> Just "-Oz"
      _ -> Nothing