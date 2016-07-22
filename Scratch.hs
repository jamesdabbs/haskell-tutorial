module Main where

import Lib
import Control.Arrow ((>>>))
import Control.Monad ((>=>))

main :: IO ()
main = someFunc

embiggen :: Int -> Int
embiggen n = n + 5

triple :: Int -> Int
-- triple x = x * 3
triple = (* 3)

tripleList :: [Int] -> [Int]
tripleList = map triple

p0 n = triple (triple (embiggen n))
p0' n = triple $ triple $ embiggen n
p0'' = triple . triple . embiggen
p0''' = embiggen >>> triple >>> triple

smallify :: Int -> Maybe Int
smallify n = if n `mod` 2 == 0
  then Just $ n `div` 2
  else Nothing

tryple :: Int -> Maybe Int
tryple n = if n < 50
  then Just $ triple n
  else Nothing

-- p1 = tryple >>> smallify >>> tryple
p1 n = do
  a <- tryple n
  b <- smallify a
  c <- tryple b
  return c
p1' = tryple >=> smallify >=> tryple


echo = do
  putStrLn "What should I say?"
  word <- getLine
  putStrLn $ "Hello, " ++ word

-- An expanded echo that can repeat things
echo2 = do
  putStrLn "What should I say?"
  word <- getLine
  putStrLn "How many times?"
  count <- getLine
  putStrLn $ repeated word (read count)

-- Note: we separate out the pure, non-io, reusable testable part
-- `intercalate` is the fancy word for Ruby's `join`
repeated :: String -> Int -> String
repeated word count = intercalate "," $ replicate count word
