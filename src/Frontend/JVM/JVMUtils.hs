module Frontend.JVM.JVMUtils (push, store) where

push :: Integer -> String
push var
  | var > 32767 = "ldc "
  | var > 127   = "sipush "
  | var > 3     = "bipush "
  | otherwise   = "iconst_"

store :: Integer -> String
store var
  | var > 3   = "istore "
  | otherwise = "istore_"