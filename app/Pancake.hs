{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Functor (($>))
import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import qualified Data.ByteString as BS
import Text.Parsec
       (ParseError, (<|>), char, digit, many1, parse, spaces)
import Text.Parsec.ByteString (Parser)

-----------
-- Types --
-----------

data Pancake
  = Happy
  | Blank
  deriving (Read, Show)

data Problem = Problem
  { pancakes :: [Pancake]
  , flipper :: Int
  } deriving (Read, Show)

data Solution
  = Solution Int
  | Impossible
  deriving (Read, Show)

------------
-- Parser --
------------

intParser :: Parser Int
intParser = read <$> many1 digit

pancakeParser :: Parser Pancake
pancakeParser = (char '+' $> Happy) <|> (char '-' $> Blank)

problemParser :: Parser Problem
problemParser = Problem <$> (many1 pancakeParser <* spaces) <*> intParser

problemsParser :: Parser [Problem]
problemsParser = do
  many1 digit *> spaces
  many1 (problemParser <* spaces)

------------
-- Solver --
------------

doFlip1 Happy = Blank
doFlip1 Blank = Happy

doFlip pancakes 0 = pancakes
doFlip (p:ps) n = doFlip1 p : doFlip ps (n - 1)

flipPancakes :: [Pancake] -> Int -> Maybe [Pancake]
flipPancakes pancakes flipperLen =
  if length pancakes < flipperLen
    then Nothing
    else Just $ doFlip pancakes flipperLen

solveProblem' :: Int -> Problem -> Solution
solveProblem' !acc (Problem (Happy : pancakes) flipperLen) = solveProblem' acc (Problem pancakes flipperLen)
solveProblem' !acc (Problem (Blank : pancakes) flipperLen) =
  let maybeFlippedPancakes = flipPancakes pancakes (flipperLen - 1)
  in case maybeFlippedPancakes of
       Just flippedPancakes -> solveProblem' (acc + 1) (Problem flippedPancakes flipperLen)
       Nothing -> Impossible
solveProblem' !acc (Problem [] flipperLen) = Solution acc

solveProblem :: Problem -> Solution
solveProblem = solveProblem' 0

solveProblems :: [Problem] -> [Solution]
solveProblems = fmap solveProblem

-----------
-- Ouput --
-----------

outputSolution :: (Solution, Int) -> IO ()
outputSolution (Solution int, caseNum) =
  putStrLn $ "Case #" <> show caseNum <> ": " <> show int
outputSolution (Impossible, caseNum) =
  putStrLn $ "Case #" <> show caseNum <> ": IMPOSSIBLE"

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
