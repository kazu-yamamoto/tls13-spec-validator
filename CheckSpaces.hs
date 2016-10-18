module Main where

import Data.Char

main :: IO ()
main = do
    inp <- getContents
    let out = map snd $ filter is4 $ map pair $ lines inp
    mapM_ putStrLn out

pair :: String -> (Int, String)
pair l = (length ss, l)
  where
    ss = takeWhile isSpace l

is4 :: (Int, String) -> Bool
is4 (n,_) = n `mod` 4 /= 0
