module Main where

import Data.List

main :: IO ()
main = do
    ls <- lines <$> getContents
    out ls

out :: [String] -> IO ()
out [] = return ()
out (l:ls) = do
    if "%%%" `isPrefixOf` l then do
        putStr "//"
        putStrLn $ drop 3 l
        app ls
      else
        out ls

app :: [String] -> IO ()
app [] = return ()
app (l:ls) = do
    if l /= "" && head l /= ' ' then
        out ls
      else do
        if l /= "" then
            putStrLn $ drop 7 l
          else
            putStrLn l
        app ls
