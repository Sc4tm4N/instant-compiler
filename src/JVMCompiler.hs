module Main where

import System.Process (callProcess)
import System.FilePath.Posix (takeDirectory, dropExtension)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Data.Map (empty)
import Control.Monad.State (runStateT)

import Backend.ParGrammar (pProgram, myLexer)
import Backend.AbsGrammar (Program)
import Backend.ErrM (Err(Bad, Ok))

import Frontend.JVM.JVMConstants (wrongUsage)
import Frontend.JVM.JVMDefinitions (ParserFunction)
import Frontend.JVM.JVMAnalyzer (compileProgram)

import Utils.Constants (parsingError)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn wrongUsage
    filenames -> do
      mapM_ (processFile pProgram) filenames
      callProcess "java" ["-jar", "libs/jasmin.jar", (++) (dropExtension $ head filenames) ".j", "-d", takeDirectory $ head filenames]

processFile :: ParserFunction Program -> FilePath -> IO ()
processFile parser filePath = readFile filePath >>= processContent parser filePath

processContent :: ParserFunction Program -> FilePath -> String -> IO ()
processContent parser filePath fileContent =
 let ts = myLexer fileContent in
   case parser ts of
     Bad fileContent -> do
       putStrLn parsingError
       putStrLn fileContent
       exitFailure
     Ok parsingTree -> do
       let outputFilePath = (++) (dropExtension filePath) ".j"
       runStateT (compileProgram parsingTree outputFilePath) (1, 1, [],  empty, 1)
       return ()
