{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module CmdLine where

import Control.Monad ( (>=>) )
import Control.Monad.Except (ExceptT, Except, liftEither, mapExceptT, runExcept)
import Control.Monad.Identity (Identity(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory (removeFile)
import System.Exit
import System.FilePath (replaceExtension)
import System.IO
import System.Process

import ANorm (aNormalizeProg)
import CmdLineArgs
import CodegenMonad (genModule, serialize)
import Fresh (Fresh, evalFresh)
import Infer (infer)
import LowLevel (runConvert)
import Parsing (parse)
import Pretty


evalInnerFresh :: ExceptT e Fresh b -> Map String Integer -> Except e b
evalInnerFresh comp state = mapExceptT (Identity . flip evalFresh state) comp


doEitherIO :: Either e a -> (a -> IO b) -> IO (Either e b)
doEitherIO a fn = either (return . Left) (fmap Right . fn) a


readBProcess :: String -> [String] -> ByteString -> IO ByteString
readBProcess cmd args input = do
  let process = (proc cmd args){std_in=CreatePipe, std_out=CreatePipe}
  (Just hInput, Just hOutput, _, processHandle) <- createProcess process
  hSetBinaryMode hInput True
  hSetBinaryMode hOutput True
  hPutStr hInput $ BSC.unpack input
  exitCode <- waitForProcess processHandle
  result <- BSC.pack <$> hGetContents hOutput
  case exitCode of
    ExitSuccess   -> return result
    ExitFailure code -> error $ cmd ++ concatMap ((' ':) . show) args ++ " (exit " ++ show code ++ ")"


writeOutput :: Args -> IO ()
writeOutput args@Args{outputFile, outputFormat, inputFile} = do
  result <- produceOutput args
  either printError writeOutput result
  where
    printError :: String -> IO ()
    printError err = putStrLn $ "Error: " ++ err

    writeOutput :: ByteString -> IO ()
    writeOutput = case outputPath of
      Just path -> BS.writeFile path
      Nothing -> putStr . BSU.toString

    outputPath :: Maybe String
    outputPath = case (outputFile, outputFormat) of
      (Just _, EXE)  -> Nothing
      (Just _, _)    -> outputFile
      (Nothing, OBJ) -> Just $ replaceExtension (fromMaybe "gen" inputFile) ".o"
      (Nothing, _)   -> Nothing


produceOutput :: Args -> IO (Either String ByteString)
produceOutput args@Args{outputFile, outputFormat, optimizationFlag} = do
  (filename, input) <- getInput args
  let parsed = parse filename $ BSU.toString input
      inferred = parsed >>= infer
      compiled = do { expr <- parsed; _ <- infer expr; return $ runConvert expr Set.empty }
      freshNormalized = liftEither compiled >>= aNormalizeProg
      normalized = runExcept $ evalInnerFresh freshNormalized Map.empty
      llvmModule = runExcept $ evalInnerFresh (freshNormalized >>= genModule) Map.empty
      mayabeOptLL = if null optimizationFlag then return else optimize args
      llvm = doEitherIO llvmModule $ (fmap BSU.fromString . serialize) >=> optimize args
      obj = llvm >>= flip doEitherIO (assemble "obj" args)
      defaultExeFile = replaceExtension filename ".out"
  case outputFormat of
    Parsed     -> return $ BSU.fromString . pretty <$> parsed
    Typed      -> return $ BSU.fromString . pretty <$> inferred
    Compiled   -> return $ BSU.fromString . pretty <$> compiled
    Normalized -> return $ BSU.fromString . pretty <$> normalized
    LLVM       -> doEitherIO llvmModule $ (fmap BSU.fromString . serialize) >=> mayabeOptLL
    ASM        -> llvm >>= flip doEitherIO (assemble "asm" args)
    OBJ        -> obj
    EXE        -> obj >>= flip doEitherIO (fmap (const "") . link args (fromMaybe defaultExeFile outputFile))
    Run        -> obj >>= flip doEitherIO (link args defaultExeFile >=> execute)


getInput :: Args -> IO (String, ByteString)
getInput Args{inputFile} = do
  input <- maybe BS.getContents BS.readFile inputFile
  return (fromMaybe "stdin" inputFile, input)


optimize :: Args -> ByteString -> IO ByteString
optimize Args{optimizationFlag} = readBProcess "opt-9" args
  where
    baseArgs = ["-S"]
    args = maybe baseArgs (:baseArgs) optimizationFlag


assemble :: String -> Args -> ByteString -> IO ByteString
assemble filetype Args{optimizationFlag} = readBProcess "llc-9" args
  where
    baseArgs = ["-filetype", filetype]
    args = case optimizationFlag of
      Just f | f `elem` ["-O0", "-O1", "-O2", "-O3"] -> f : baseArgs
      _ -> baseArgs


-- | Compile the runtime C component. Returns the name of the output object file
compileRuntimeC :: Args -> IO String
compileRuntimeC Args{debugRuntime} = readProcess "clang" args "" >> return output
  where
    input = "runtime.c"
    output = "runtime.o"
    baseArgs = ["-c", "-O3", "-flto", "-o", output, input]
    debugFlag = "-DDEBUG"
    args = if debugRuntime then debugFlag : baseArgs else baseArgs


link :: Args -> String -> ByteString -> IO String
link args@Args{optimizationFlag} output input = do
  runtimeFileName <- compileRuntimeC args
  let objFileName = "gen.o"
  BS.writeFile objFileName input
  let baseArgs = ["-flto", "-o", output, runtimeFileName, objFileName]
      args = maybe baseArgs (:baseArgs) optimizationFlag
  _ <- readProcess "clang" args ""
  removeFile runtimeFileName
  removeFile objFileName
  return output


execute :: String -> IO ByteString
execute filename = readBProcess ("./" ++ filename) [] "" <* removeFile filename
