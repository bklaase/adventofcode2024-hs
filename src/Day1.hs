{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AoC.Lib as L
import qualified Data.Text as T

-- day specific imports
import Data.List (sort)

-- parsing input
parse :: T.Text -> ([Int],[Int])
parse input =
  let
    inputs = map (T.splitOn "   ") $ T.lines input
    xs = map read ([ T.unpack x | (x:y:_) <- inputs ]) :: [Int]
    ys = map read ([ T.unpack y | (x:y:_) <- inputs ]) :: [Int]
  in (xs, ys)

-- solution part1
part1 :: ([Int], [Int]) -> Int
part1 (xs,ys) = sum $ abs <$> zipWith (-) (sort xs) (sort ys)

-- solution part2
part2 :: ([Int], [Int]) -> Int
part2 (xs,ys) = sum $ map similarityScore xs
  where
    similarityScore x = (* x) $ length $ filter (== x) ys 

-- results
main :: IO ()
main = do
  let solution = L.Solution 1 parse part1 part2
  L.solve solution
