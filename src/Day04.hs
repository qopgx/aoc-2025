module Day04 where

import Data.Bool
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Vector as V
import Paths_aoc2025 (getDataFileName)

type Vpos = Int

type Width = Int

type Offset = Int

nxt :: Width -> Vpos -> Offset -> Maybe Vpos
nxt w vp o
  | o == 1 =
      let nextvp = vp + o
          valid = mod vp w /= w - 1
          result = bool Nothing (Just nextvp) valid
       in result
  | elem o [2, 3, 4] =
      let nextvp = vp + (o - 3) + w
          valid = case o of
            2 -> mod vp w /= 0
            4 -> mod vp w /= w - 1
            _ -> True
          result = bool Nothing (Just nextvp) valid
       in result
  | otherwise = error "Invalid offset"

countx :: (Foldable t) => V.Vector Char -> Int -> t Int -> Int
countx symbols x =
  let helper (s, i) v = bool (s, i + 1) (s + 1, i + 1) (v < x && symbols V.! i == '@')
   in fst . foldl' helper (0, 0)

updateDegrees :: Width -> V.Vector Char -> Vpos -> V.Vector Int -> Offset -> V.Vector Int
updateDegrees w symbols vxi degrees o = case nxt w vxi o of
  (Just vxi')
    | vxi' < V.length symbols && symbols V.! vxi' == '@' ->
        V.update degrees (V.fromList [(vxi, degrees V.! vxi + 1), (vxi', degrees V.! vxi' + 1)])
  _ -> degrees

step :: Width -> V.Vector Char -> V.Vector Int -> Vpos -> V.Vector Int
step width symbols degrees vxi =
  let newDegrees = case symbols V.! vxi of
        '@' -> foldl' (updateDegrees width symbols vxi) degrees [1 .. 4]
        '.' -> degrees
        _ -> error "fuck"
   in newDegrees

solve :: Width -> String -> Int
solve width input =
  let symbols = V.fromList input
      symbolsL = V.length symbols
      degrees = V.replicate symbolsL 0
   in countx symbols 4 $ foldl' (step width symbols) degrees [0 .. symbolsL - 1]

{-
  ~ P2 ~
  Soon sa k-core peelingom
-}

class Stack t where
  pop :: t a -> (Maybe a, t a)
  push :: t a -> a -> t a

instance Stack [] where
  pop [] = (Nothing, [])
  pop (top : rest) = (Just top, rest)
  push stack element = (element : stack)

type Size = Int

type State a = (V.Vector a, V.Vector a, V.Vector a, [a])

buildDegreeList :: Width -> V.Vector Char -> Size -> State Int -> Int -> State Int
buildDegreeList = undefined

-- solve' :: Width -> String -> Int
-- solve' width input =
--   let rolls = V.fromList input -- let's call them properly
--       lenRolls = V.length rolls
--       degrees = V.empty :: V.Vector Int
--       vxId = V.replicate lenRolls 0 -- from sparse to sequential indices
--       vxIdBack = V.empty :: V.Vector Int -- back from sequentail to sparse
--       removed = V.empty :: V.Vector Int
--       stack = [] :: [Int]
--    in foldl'
--         (buildDegreeList width rolls lenRolls)
--         (vxId, vxIdBack, removed, stack)
--         [0 .. lenRolls - 1]

sanitizeInputs :: String -> String
sanitizeInputs = filter (/= '\n')

day04 :: IO ()
day04 = do
  inputLines <- getDataFileName "day04-input.txt" >>= readFile

  putStrLn "Day 04"

  let width = length $ head (splitOn "\n" inputLines)

  -- P1 metoda efektivna za guste grafove
  print (solve width (sanitizeInputs inputLines))

-- print (solve' width (sanitizeInputs inputLines))
