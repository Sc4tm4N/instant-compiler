module Frontend.JVM.JVMAnalyzer (compileProgram) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, insert, (!))
import Control.Monad.State (put, get, liftIO)
import System.FilePath.Posix (takeBaseName)

import Backend.AbsGrammar (Ident, Exp(ExpLit, ExpVar, ExpAdd, ExpSub, ExpMul, ExpDiv), Program(Prog), Stmt(SAss, SExp))

import Frontend.Utils.Formatter (indentation)
import Frontend.JVM.JVMUtils (push, store)
import Frontend.JVM.JVMConstants (filePattern, invokePrintInstruction, swapInstruction, getPrintInstruction)
import Frontend.JVM.JVMDefinitions (Instant)


compileProgram :: Program -> String -> Instant ()
compileProgram programStructure outputFilePath = do
  compile programStructure
  (maxStack, maxLocal, instructions, _, _) <- get
  let body = foldr (\x y -> y ++ "\n" ++ x) "" instructions
  liftIO $ writeFile outputFilePath $ filePattern (takeBaseName outputFilePath) (show maxStack) (show maxLocal) body
  return ()

compile :: Program -> Instant ()
compile (Prog []) = return ()
compile (Prog (s:ss)) = do
  compileStatement s
  compile $ (Prog ss)

compileStatement :: Stmt -> Instant ()
compileStatement (SAss id e) = do
  (currentStackHeight, newInstructions) <- processExpression e
  (maxStack, maxLocal, savedInstructions, store, indentationLevel) <- get
  case (lookup id store) of
    Just n -> do
      let s = (resolveStore n indentationLevel):(newInstructions ++ savedInstructions)
      put (max currentStackHeight maxStack, maxLocal, s, insert id n store, indentationLevel)
      return ()
    Nothing -> do
      let s = (resolveStore maxLocal indentationLevel):(newInstructions ++ savedInstructions)
      put (max currentStackHeight maxStack, maxLocal + 1, s, insert id maxLocal store, indentationLevel)
      return ()
compileStatement (SExp e) = do
  (currentStackHeight, newInstructions) <- processExpression e
  (maxStack, maxLocal, savedInstructions, store, indentationLevel) <- get
  let allInstructions = ((indentation invokePrintInstruction indentationLevel):(indentation swapInstruction indentationLevel):(indentation getPrintInstruction indentationLevel):(newInstructions ++ savedInstructions))
  put (max (currentStackHeight + 1) maxStack, maxLocal, allInstructions, store, indentationLevel)
  return ()

processExpression :: Exp -> Instant (Integer, [String])
processExpression (ExpAdd e1 e2) = processExpressionInternally "iadd" True e1 e2
processExpression (ExpMul e1 e2) = processExpressionInternally "imul" True e1 e2
processExpression (ExpSub e1 e2) = processExpressionInternally "isub" False e1 e2
processExpression (ExpDiv e1 e2) = processExpressionInternally "idiv" False e1 e2
processExpression (ExpLit i) = do
  (_, _, _, _, indentationLevel) <- get
  return (1, [resolvePush i indentationLevel])
processExpression (ExpVar id) = do
  (_, _, _, variablesStore, indentationLevel) <- get
  return (1, [resolveLoad variablesStore id indentationLevel])

processExpressionInternally :: String -> Bool -> Exp -> Exp -> Instant (Integer, [String])
processExpressionInternally operation isCommutative e1 e2 = do
  (stackHeight1, calculationInstructions1) <- processExpression e1
  (stackHeight2, calculationInstructions2) <- processExpression e2
  (maxStack, maxLocal, _, _, indentationLevel) <- get
  let currentInstruction = indentation operation indentationLevel
  return (if stackHeight1 == stackHeight2 then
        (stackHeight1 + 1, currentInstruction:(calculationInstructions2 ++ calculationInstructions1))
    else if stackHeight1 > stackHeight2 then
        (stackHeight1, currentInstruction:(calculationInstructions2 ++ calculationInstructions1))
    else (stackHeight2, currentInstruction:(
            (if isCommutative then [] else [indentation swapInstruction indentationLevel])
             ++ (calculationInstructions1 ++ calculationInstructions2))))

resolvePush :: Integer -> Int -> String
resolvePush var indentationLevel = indentation ((push var) ++ (show var)) indentationLevel

resolveStore :: Integer -> Int -> String
resolveStore var indentationLevel = indentation ((store var) ++ (show var)) indentationLevel

resolveLoad :: Map Ident Integer -> Ident -> Int -> String
resolveLoad variablesStore id indentationLevel = indentation ("iload " ++ (show $ variablesStore ! id)) indentationLevel