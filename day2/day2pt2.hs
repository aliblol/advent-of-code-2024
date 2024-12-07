-- Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.

-- More of the above example's reports are now safe:

-- 7 6 4 2 1: Safe without removing any level.
-- 1 2 7 8 9: Unsafe regardless of which level is removed.
-- 9 7 6 2 1: Unsafe regardless of which level is removed.
-- 1 3 2 4 5: Safe by removing the second level, 3.
-- 8 6 4 4 1: Safe by removing the third level, 4.
-- 1 3 6 7 9: Safe without removing any level.

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

-- Generate all versions of the list with one element removed
removeOneLevel :: [Int] -> [[Int]]
removeOneLevel xs = [take i xs ++ drop (i + 1) xs | i <- [0..length xs - 1]]

-- Check if any list (original or modified by removing one element) is safe
isSafeDampened :: [Int] -> Bool
isSafeDampened xs = isSafe xs || any isSafe (removeOneLevel xs)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let lists = parseInput input
    print . length . filter isSafeDampened $ lists