module Frontend.Utils.Formatter (indentation) where

baseIndentation :: Int
baseIndentation = 4

-- line, level of indentation
indentation :: String -> Int -> String
indentation s level = (concat (replicate (level * baseIndentation) " ")) ++ s