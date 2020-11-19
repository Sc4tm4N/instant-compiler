module Frontend.JVM.JVMDefinitions (ParserFunction, Store, Instant) where

import Data.Map (Map)
import Control.Monad.State (StateT)

import Backend.LexGrammar (Token)
import Backend.AbsGrammar (Ident)
import Backend.ErrM (Err)

type ParserFunction a = [Token] -> Err a

-- maxStack, maxLocal, operations, {variables}, indentation_level
type Store = (Integer, Integer, [String], Map Ident Integer, Int)
type Instant = StateT Store IO