module AoC.Lib where

import Data.Time.Clock ( NominalDiffTime, diffUTCTime, getCurrentTime )
import Text.Printf (printf)

import qualified Data.Text.IO as TI
import qualified Data.Text as T

data Solution  a b c = Solution
  { day :: Int,
    parse :: T.Text -> a,
    part1 :: a -> b,
    part2 :: a -> c }

data BenchmarkResult a = BenchmarkResult
  { answer :: a,
    runTime :: NominalDiffTime } deriving Show

solve :: (Show b, Show c) => Solution a b c -> IO ()
solve solution = do
  inputStr <- TI.readFile  (printf "./input/day%d.txt" (day solution))
  let input = parse solution inputStr
  
  ans1 <- benchmark $ part1 solution input
  putStrLn $ "Part1: " ++ show ans1
  
  ans2 <- benchmark $ part2 solution input
  putStrLn $ "Part2: " ++ show ans2


benchmark :: a -> IO (BenchmarkResult a)
benchmark a = do
  startTime <- getCurrentTime
  a `seq` return ()
  stopTime <- getCurrentTime
  let runTime = diffUTCTime  stopTime startTime
  return $ BenchmarkResult a runTime
