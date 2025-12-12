module Day03 where

import Data.Char
import Data.List
import Paths_aoc2025 (getDataFileName)

joltage :: Int -> [Int] -> Int
joltage _ [] = 0
joltage 0 _ = 0
joltage depth bank =
  let maxJoltage = maximum (take (length bank - depth + 1) bank)
      maxJoltageIndex = case elemIndex maxJoltage bank of
        Just v -> v
        Nothing -> error "Should not happen"
      result = maxJoltage * 10 ^ (depth - 1)
   in result + joltage (depth - 1) (drop (maxJoltageIndex + 1) bank)

solve :: Int -> [String] -> Int
solve = ((sum .) . (. map (map digitToInt))) . map . joltage

day03 :: IO ()
day03 = do
  inputLines <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines

  putStrLn "Day 03\n"

  putStrLn "Please enter the number of batteries per line: "
  batteriesInput <- getLine
  let numBatteries = read batteriesInput :: Int

  putStrLn ("Result: " ++ show (solve numBatteries inputLines))
