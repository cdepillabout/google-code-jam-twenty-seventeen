{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Functor (($>))
import Data.Monoid ((<>))
import Data.Vector (Vector, (!), (//), fromList, modify, toList)
import Data.Vector.Mutable (write)
import qualified Data.Vector as V
import Data.Foldable (traverse_)
import qualified Data.ByteString as BS
import Text.Parsec
       (ParseError, (<|>), char, digit, many1, parse, spaces)
import Text.Parsec.ByteString (Parser)

-----------
-- Types --
-----------

type Number = Int

data Problem = Problem
  { problemNums :: [Number]
  } deriving (Read, Show)

data Solution = Solution
  { solutionNums :: [Number]
  } deriving (Read, Show)

------------
-- Parser --
------------

numberParser :: Parser Number
numberParser = read . (\c -> [c]) <$> digit

problemParser :: Parser Problem
problemParser = Problem <$> (many1 numberParser <* spaces)

problemsParser :: Parser [Problem]
problemsParser = do
  many1 digit *> spaces
  many1 (problemParser <* spaces)

------------
-- Solver --
------------

allNines :: Int -> Vector Number -> Vector Number
allNines i vec = vec // zip [0..i] (cycle [9])

solveProblem' :: Int -> Int -> Vector Number -> Vector Number
solveProblem' len !i vec
  | (i + 1) < len =
    let a = vec ! i
        b = vec ! (i + 1)
    in case a < b of
      True ->
        let newVec = allNines i vec
        in solveProblem' len (i + 1) (modify (\v -> write v (i + 1) (b - 1)) newVec)
      False -> solveProblem' len (i + 1) vec
  | otherwise = vec

solveProblem :: Problem -> Solution
solveProblem (Problem nums) = Solution . reverse . toList $ solveProblem' (length nums) 0 (fromList (reverse nums))

solveProblems :: [Problem] -> [Solution]
solveProblems = fmap solveProblem

-----------
-- Ouput --
-----------

removeLeadingZero :: [Int] -> [Int]
removeLeadingZero (0:n) = removeLeadingZero n
removeLeadingZero ns = ns

format :: [Int] -> String
format (i:is) = show i <> format is
format [] = ""

outputSolution :: (Solution, Int) -> IO ()
outputSolution (Solution nums, caseNum) =
  putStrLn $ "Case #" <> show caseNum <> ": " <> format (removeLeadingZero nums)

outputSolutions :: [Solution] -> IO ()
outputSolutions solutions = traverse_ outputSolution $ zip solutions [1..]

----------
-- Main --
----------

readAndParse :: IO (Either ParseError [Problem])
readAndParse = parse problemsParser "" <$> BS.getContents

main :: IO ()
main = do
  eitherProblems <- readAndParse
  -- print eitherProblems
  case eitherProblems of
    Left parseErr -> putStrLn $ "got parse error: " <> show parseErr
    Right problems -> do
      let solutions = solveProblems problems
      -- print solutions
      outputSolutions solutions
