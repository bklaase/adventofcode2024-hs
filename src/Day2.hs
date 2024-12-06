module Main where

import qualified AoC.Lib as L
import qualified Data.Text as T
import qualified Data.Text.Read as R

-- day specific imports

-- parsing input

parse :: T.Text -> [[Integer]]
parse = map (map (fst . getNum) . T.words) . T.lines
  where getNum n = case R.decimal n of Right d -> d

-- solution part1
safeIncrease :: Integer -> Integer -> Bool
safeIncrease x y = 0 < y-x && y-x < 4

isAscending :: [Integer] -> Bool
isAscending (x:y:rest) = safeIncrease x y
  && isAscending (y:rest)
isAscending _ = True

isDescending :: [Integer] -> Bool
isDescending (x:y:rest) = safeIncrease y x
  && isDescending (y:rest)
isDescending _ = True

isSafe :: [Integer] -> Bool
isSafe xs = isAscending xs || isDescending xs

part1 :: [[Integer]] -> Int
part1 = length . filter isSafe

-- solution part2
-- dampenedAsc (x:y:rest)

isDampenedSafe :: [Integer] -> Bool
isDampenedSafe xs = aux [] xs
  where
    aux xs [] = isSafe xs
    aux xs (y:ys) = isSafe (xs ++ ys)
      || aux (xs ++ [y]) ys

part2 = length . filter
        (\x -> isSafe x ||isDampenedSafe x)
-- results
main :: IO ()
main = do
  let solution = L.Solution 2 parse part1 part2
  L.solve solution
