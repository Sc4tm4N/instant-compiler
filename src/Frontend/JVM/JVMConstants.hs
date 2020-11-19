module Frontend.JVM.JVMConstants (wrongUsage, filePattern, invokePrintInstruction,
 swapInstruction, getPrintInstruction) where

wrongUsage :: String
wrongUsage = "Usage: \"./insc_jvm filename\""

invokePrintInstruction :: String
invokePrintInstruction = "invokevirtual java/io/PrintStream/println(I)V"

swapInstruction :: String
swapInstruction = "swap"

getPrintInstruction :: String
getPrintInstruction = "getstatic java/lang/System/out Ljava/io/PrintStream;"

filePattern :: String -> String -> String -> String -> String
filePattern className maxStack maxLocal body = (++) (preambule className) (postambule maxStack maxLocal body)

preambule :: String -> String
preambule className =
  ".class public " ++ className ++ "\n"
  ++ ".super java/lang/Object\n"
  ++ "\n"
  ++ ".method public <init>()V\n"
  ++ "    aload_0\n"
  ++ "    invokespecial java/lang/Object/<init>()V\n"
  ++ "    return\n"
  ++ ".end method\n"
  ++ "\n"
  ++ ".method public static main([Ljava/lang/String;)V\n"

postambule :: String -> String -> String -> String
postambule maxStack maxLocal body =
  "    .limit stack " ++ maxStack ++ "\n"
  ++ "    .limit locals " ++ maxLocal ++ "\n"
  ++ body ++ "\n"
  ++ "    return\n"
  ++ ".end method\n"