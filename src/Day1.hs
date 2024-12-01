module Main where

import qualified AoC.Lib as L
import qualified Data.Text as T
import Data.List ( sortOn )

splitWhen :: (a-> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p l@(x:xs)
  | p x = splitWhen p xs
  | otherwise = group : splitWhen p rest
      where (group, rest) = break p l

parse = splitWhen (== "   ") . lines

part1 = id

part2 _ = "0"

main :: IO ()
main = do
  let solution = L.Solution 1 parse part1 part2
  L.solve solution
