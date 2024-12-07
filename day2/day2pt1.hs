-- The levels are either all increasing or all decreasing.
-- Any two adjacent levels differ by at least one and at most three.

-- 7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
-- 1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
-- 9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
-- 1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
-- 8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
-- 1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.

-- How many reports are safe?

import System.IO
import Data.List
import Language.Haskell.TH (isStrict)

-- parseInput whill produce x lists of length y
parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)

isWithin3 :: [Int] -> Bool
isWithin3 xs = all (\(a, b) -> abs (a - b) <= 3 && abs (a - b) > 0) (zip xs (tail xs))

isStrictlyIncreasing :: [Int] -> Bool
isStrictlyIncreasing xs = all (uncurry (>)) (zip xs (tail xs))

isStrictlyDecreasing :: [Int] -> Bool
isStrictlyDecreasing xs = all (uncurry (<)) (zip xs (tail xs))

-- isSafe will return True if the list is either all increasing or all decreasing by 1 or 2 or 3
isSafe :: [Int] -> Bool
isSafe xs = (isStrictlyIncreasing xs || isStrictlyDecreasing xs) && isWithin3 xs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let lists = parseInput input
    print . length . filter isSafe $ lists