module Main where

import Data.Map (empty)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)

import System.Process (callProcess)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix (dropExtension)

import Backend.ParGrammar (pProgram, myLexer)
import Backend.AbsGrammar (Program)
import Backend.ErrM (Err(Bad, Ok))

import Frontend.LLVM.LLVMConstants (wrongUsage, preambule, postambule)
import Frontend.LLVM.LLVMDefinitions (ParserFunction, InstantValue(RegistryNumber))
import Frontend.LLVM.LLVMAnalyzer (compileProgram)
import Frontend.Utils.Formatter (indentation)
import Utils.Constants (parsingError)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn wrongUsage
    filenames -> do
      mapM_ (processFile pProgram) filenames
      callProcess "llvm-as" ["-o",  "toRemove.bc", (++) (dropExtension $ head filenames) ".ll"]
      callProcess "llvm-link" ["-o", (++) (dropExtension $ head filenames) ".bc", "toRemove.bc", "libs/runtime.bc"]
      callProcess "rm" ["toRemove.bc"]

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
        let outputFilePath = (++) (dropExtension filePath) ".ll"
        writeFile outputFilePath preambule
        runStateT (runReaderT (compileProgram parsingTree) (appendFile outputFilePath)) (RegistryNumber 1, empty)
        appendFile outputFilePath (indentation postambule 1)
        return ()
