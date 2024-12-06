{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AoC.Lib as L
import qualified Data.Text as T

-- day specific imports

-- parsing input
parse = id

-- solution part1
part1 = id

-- solution part2
part2 = id
-- results
main :: IO ()
main = do
  let solution = L.Solution 00 parse part1 part2
  L.solve solution
