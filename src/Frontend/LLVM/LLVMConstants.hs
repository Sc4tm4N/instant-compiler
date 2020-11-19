module Frontend.LLVM.LLVMConstants (wrongUsage, preambule, postambule) where

wrongUsage :: String
wrongUsage = "Usage: \"./insc_llvm filename\""

preambule :: String
preambule = "declare void @printInt(i32)\n"
  ++ "\n"
  ++ "define i32 @main(){\n"

postambule :: String
postambule = "ret i32 0\n"
  ++ "}\n"
