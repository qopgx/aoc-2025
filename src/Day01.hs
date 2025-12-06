module Day01 where

import Data.List (foldl')
import Paths_aoc2025 (getDataFileName)

{- P1
  Functional way of thinking, this is a state machine.
  State machine -> foldl
  Reminding self that,
  foldr -> build an expression that can be lazy evaled
  foldl -> just update an accumulator with recursive calls constantly, but thunks
  foldl' -> just update the accumulator constantly, as we'd like
  We could use foldr if we knew there'd be a last 0, we can't know that
-}

type Password = Int

type Position = Int

type Rotation = String

{-
  Helpers:
-}
rotate :: Position -> Rotation -> Position
rotate p [] = error "Impossible"
rotate p ('L' : num) = mod (p - read num) 100
rotate p ('R' : num) = mod (p + read num) 100

increment :: Position -> Password -> Password
increment 0 pw = pw + 1
increment _ pw = pw

transition :: (Position, Password) -> Rotation -> (Position, Password)
transition (po, pw) r = let newpo = rotate po r in (newpo, increment newpo pw)

{-
  The solution function:
-}

solve :: (Position, Password) -> [Rotation] -> (Position, Password)
solve ti rs = foldl' transition ti rs

{-P2-}

{-
  We'll have to change the rules a little this time around
  Fundamentally we'll keep a cumulative rotation as long as it is in one direction
-}

type Direction = Bool

direction' :: Char -> Maybe Direction
direction' 'L' = Just False
direction' 'R' = Just True
direction' _ = Nothing

transition' :: (Position, Password, Maybe Direction) -> Rotation -> (Position, Password, Maybe Direction)
transition' (p, pw, md) (rd : num)
  | Just d' <- md', Just d <- md, d == d' = (p + read num, pw, md)
  -- Noting that since p is the number of wraps, it's not a valid coordinate
  -- So it needs to be mod 100 to turn it into a valid coordinate post calc for
  -- the case p == 0
  | otherwise = (mod (100 - mod p 100) 100 + read num, pw + div p 100, md') -- md' could be Nothing
  where
    md' = direction' rd

solve' :: (Position, Password) -> [Rotation] -> (Position, Password)
solve' (p, pw) rs =
  let (fp, fpw, _) = foldl' transition' (p, pw, direction' 'R') rs in (fp, fpw)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  putStrLn (show $ solve (50, 0) inputLines)
  putStrLn (show $ solve' (50, 0) (inputLines ++ ["N0"]))
