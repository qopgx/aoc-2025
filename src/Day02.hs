module Day02 where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Paths_aoc2025 (getDataFileName)

-- ============================ DIAGONAL ============================

{-
  The idea is that instead of checking each number we write the rules
  of how the number behaves and then just sum up the intervals,
  noting that we need to ceil and floor the edges of the intervals
  based on the requirements (e.g. 999 goes 1010 if left, 32523 goes 9999).
-}

data Diagonal = Diagonal Integer

instance Eq Diagonal where
  Diagonal aHalf == Diagonal bHalf = aHalf == bHalf

instance Show Diagonal where
  show (Diagonal half) = show half ++ show half

toDiagonal :: (Integral a, Show a) => a -> Maybe Diagonal
toDiagonal value
  | value < 0 = Nothing
  | value == 0 = Just (Diagonal 0)
  | digitCount < 2 = Nothing
  | odd digitCount = Nothing
  | otherwise = Just (Diagonal (div integerValue powerTen))
  where
    integerValue = toInteger value
    digitCount = length (show integerValue)
    powerTen = (10 :: Integer) ^ div digitCount 2

instance Num Diagonal where
  Diagonal aHalf + Diagonal bHalf = Diagonal (aHalf + bHalf)

  Diagonal aHalf - Diagonal bHalf
    | aHalf - bHalf < 0 = error "Can't do negatives"
    | otherwise = Diagonal (aHalf - bHalf)

  Diagonal aHalf * Diagonal bHalf = Diagonal (aHalf * bHalf)

  abs (Diagonal half) = Diagonal (abs half)
  signum (Diagonal half) = Diagonal (signum half)

  fromInteger value
    | Just diagonalValue <- toDiagonal value = diagonalValue
    | otherwise = error "Invalid value"

instance Ord Diagonal where
  Diagonal aHalf <= Diagonal bHalf = aHalf <= bHalf

instance Enum Diagonal where
  toEnum intValue
    | intValue < 0 = error "No negatives"
    | otherwise = Diagonal (toInteger intValue)

  fromEnum (Diagonal half)
    | half > toInteger (maxBound :: Int) = error "Too large"
    | otherwise = fromInteger half

back :: Diagonal -> Integer
back (Diagonal half) =
  let halfNDigits = length (show half)
      powerTen = (10 :: Integer) ^ halfNDigits
   in half * powerTen + half

-- ============================ INPUTS ============================

processInput :: String -> [(Integer, Integer)]
processInput =
  map ((\[lowerText, upperText] -> (read lowerText, read upperText)) . splitOn "-")
    . splitOn ","

-- ============================ SOLUTION ============================

ceilDiagonal :: Integer -> Diagonal
ceilDiagonal value
  | value <= 0 = Diagonal 0
  | odd digitCount =
      let halfNDigits = div (digitCount + 1) 2
       in Diagonal ((10 :: Integer) ^ (halfNDigits - 1))
  | otherwise =
      let halfNDigits = div digitCount 2
          powerTen = (10 :: Integer) ^ halfNDigits
          highHalf = div value powerTen
          lowHalf = mod value powerTen
          chosenHalf = if highHalf >= lowHalf then highHalf else highHalf + 1
       in Diagonal chosenHalf
  where
    digitCount = length (show value)

floorDiagonal :: Integer -> Diagonal
floorDiagonal value
  | value <= 0 = Diagonal 0
  | odd digitCount =
      let halfNDigits = div (digitCount - 1) 2
       in if halfNDigits == 0
            then Diagonal 0
            else Diagonal ((10 :: Integer) ^ halfNDigits - 1)
  | otherwise =
      let halfNDigits = div digitCount 2
          powerTen = (10 :: Integer) ^ halfNDigits
          highHalf = div value powerTen
          lowHalf = mod value powerTen
          chosenHalf = if highHalf <= lowHalf then highHalf else highHalf - 1
       in Diagonal chosenHalf
  where
    digitCount = length (show value)

transition :: Integer -> (Integer, Integer) -> Integer
transition accumulator (lowerBound, upperBound) =
  let lowerDiagonal = ceilDiagonal lowerBound
      upperDiagonal = floorDiagonal upperBound
      diagonalsInRange =
        if lowerDiagonal <= upperDiagonal
          -- Using diagonal we can write the interval in this form
          then [lowerDiagonal .. upperDiagonal]
          else []
   in accumulator + sum (map back diagonalsInRange)

solve :: [(Integer, Integer)] -> Integer
solve = foldl' transition 0

-- ============================ SOLUTION P2 ============================

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn (unlines inputLines)
  putStrLn "Processed input:"
  let ranges = processInput (inputLines !! 0)
  print ranges
  print (solve ranges)
