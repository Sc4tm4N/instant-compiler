module Frontend.LLVM.LLVMAnalyzer (compileProgram) where

import Data.Map ((!), insert)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put, liftIO)

import Backend.AbsGrammar (Program(Prog), Stmt(SAss, SExp), Exp(ExpAdd, ExpSub, ExpMul, ExpDiv, ExpLit, ExpVar))

import Frontend.LLVM.LLVMDefinitions (Instant, InstantValue(RegistryNumber, PureNumber), incr)
import Frontend.Utils.Formatter (indentation)

compileProgram :: Program -> Instant ()
compileProgram (Prog []) = return ()
compileProgram (Prog (s:ss)) = do
  compileStatement s
  compileProgram $ Prog ss

compileStatement :: Stmt -> Instant ()
compileStatement (SAss id e) = do
  val <- processExpression e
  (n, store) <- get
  case val of
    (RegistryNumber r) -> do
      put (n, insert id val store)
      return ()
    (PureNumber i) -> do
      put (incr n, insert id n store)
      fileAppender <- ask
      liftIO $ fileAppender $ resolveInit n i
      return ()
compileStatement (SExp e) = do
  n <- processExpression e
  fileAppender <- ask
  liftIO $ fileAppender $ resolvePrint n
  return ()

processExpression :: Exp -> Instant (InstantValue)
processExpression (ExpAdd e1 e2) = processExpressionInternally "add" e1 e2
processExpression (ExpSub e1 e2) = processExpressionInternally "sub" e1 e2
processExpression (ExpMul e1 e2) = processExpressionInternally "mul" e1 e2
processExpression (ExpDiv e1 e2) = processExpressionInternally "sdiv" e1 e2
processExpression (ExpLit i) = return (PureNumber i)
processExpression (ExpVar id) = do
  (_, store) <- get
  return (store ! id)

processExpressionInternally :: String -> Exp -> Exp -> Instant (InstantValue)
processExpressionInternally operation e1 e2 = do
  val1 <- processExpression e1
  val2 <- processExpression e2
  (n, store) <- get
  fileAppender <- ask
  liftIO $ fileAppender $ resolveOperation n operation val1 val2
  put (incr n, store)
  return n

resolveInit :: InstantValue -> Integer -> String
resolveInit reg num = indentation ((show reg) ++ " = add i32 0, " ++ (show num) ++ "\n") 1

resolvePrint :: InstantValue -> String
resolvePrint val = indentation ("call void @printInt(i32 " ++ (show val) ++ ")\n") 1

resolveOperation :: InstantValue -> String -> InstantValue -> InstantValue -> String
resolveOperation reg operation val1 val2 =
  indentation ((show reg) ++ " = " ++ operation ++ " i32 " ++ (show val1) ++ ", " ++ (show val2) ++ "\n") 1
