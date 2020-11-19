module Frontend.LLVM.LLVMDefinitions (ParserFunction,
  InstantValue(RegistryNumber, PureNumber), Store, Env, Instant, incr) where

import Data.Map (Map)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)

import Backend.LexGrammar (Token)
import Backend.AbsGrammar (Ident)
import Backend.ErrM (Err)

type ParserFunction a = [Token] -> Err a

data InstantValue = RegistryNumber Integer | PureNumber Integer
type Store = (InstantValue, Map Ident InstantValue)
type Env = String -> IO()

type Instant = ReaderT Env (StateT Store IO)

instance Show InstantValue where
  show (RegistryNumber i) = "%" ++ (show i)
  show (PureNumber i) = show i

incr  :: InstantValue -> InstantValue
incr (RegistryNumber i) = RegistryNumber (i + 1)
incr (PureNumber i) = PureNumber (i + 1)