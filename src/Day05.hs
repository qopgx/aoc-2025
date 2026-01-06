module Day05 where

import Data.List
import Data.List.Split
import Paths_aoc2025 (getDataFileName)

mergeRanges :: [Int] -> [[Int]] -> [[Int]]
mergeRanges r [] = [r]
mergeRanges r@[lo, hi] rs@([lo', hi'] : t)
  | lo' - 1 <= hi = [lo, max hi' hi] : t
  | otherwise = r : rs
mergeRanges _ _ = error "mergeRanges: Impossible"

inRange [l, r] x = l <= x && x <= r
inRange _ _ = error "inRange: Impossible"

orHigher predicate [l, r] x = predicate [l, r] x || r < x
orHigher _ _ _ = error "Impossible"

getMergedRanges ranges = foldr mergeRanges [] $ sortOn head $ sortOn last ranges

solve :: [[Int]] -> [Int] -> [Int]
solve ranges ingredients =
  let mergedRanges = getMergedRanges ranges
      sortedIngredients = sort ingredients
   in concat $
        snd $
          mapAccumL
            ( \a b ->
                let trimmed =
                      dropWhile (not . orHigher inRange b) a
                 in (trimmed, takeWhile (inRange b) trimmed)
            )
            sortedIngredients
            mergedRanges

day05 :: IO ()
day05 = do
  [inputRanges, inputIngredients] <- splitOn "\n\n" <$> (getDataFileName "day05-input.txt" >>= readFile)
  let ranges = map (map (read :: String -> Int) . splitOn "-") $ lines inputRanges
      ingredients = map (read :: String -> Int) $ lines inputIngredients
  print $ length $ solve ranges ingredients
  -- Second one didn't make a big difference
  print $ foldl' (\a [l, r] -> r - l + 1 + a) 0 $ getMergedRanges ranges
