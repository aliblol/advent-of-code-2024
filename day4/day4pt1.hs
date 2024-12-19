import Data.Foldable (find)
type Grid = [[Char]]
type Position = (Int, Int)
type Direction = (Int, Int)

directions :: [Direction]
directions = [(1, 0), (0, 1), (1, 1), (1, -1), (-1, 0), (0, -1), (-1, -1), (-1, 1)]

-- Check if a word exists in a given direction from a starting position
findWordInDirection :: Grid -> String -> Position -> Direction -> Bool
findWordInDirection grid word (x, y) (dx, dy) = all match (zip word positions)
  where
    positions = take (length word) $ iterate (\(x, y) -> (x + dx, y + dy)) (x, y)
    match (c, (i, j)) = inBounds (i, j) && grid !! i !! j == c
    inBounds (i, j) = i >= 0 && i < length grid && j >= 0 && j < length (head grid)

-- Search for the word in all possible directions from a given starting point
findWordFromPosition :: Grid -> String -> Position -> Int
findWordFromPosition grid word (x, y) = length $ filter (findWordInDirection grid word (x, y)) directions

-- Check if the word exists in any direction from the given position
isWordAtPosition :: Grid -> String -> Position -> Bool
isWordAtPosition grid word (x, y) = any (findWordInDirection grid word (x, y)) directions

-- Iterate over all starting points in the grid
findWordInGrid :: Grid -> String -> Int
findWordInGrid grid word = sum [findWordFromPosition grid word (x, y) | x <- [0..length grid - 1], y <- [0..length (head grid) - 1]]

-- Parse the input file into a grid 
parseInput :: String -> Grid
parseInput = lines

-- Example grid for "XMAS" test case
exampleGrid :: Grid
exampleGrid = ["MMMSXXMASM",
               "MSAMXMSMSA",
               "AMXSXMAAMM",
               "MSAMASMSMX",
               "XMASAMXAMM",
               "XXAMMXXAMA",
               "SMSMSASXSS",
               "SAXAMASAAA",
               "MAMMMXMMMM",
               "MXMXAXMASX"]

-- Example test case
exampleTestCase :: (String, Int)
exampleTestCase = ("XMAS", 18)

-- Test function for example grid
testExampleGrid :: IO ()
testExampleGrid = do
  let (word, expected) = exampleTestCase
  let result = findWordInGrid exampleGrid word
  putStrLn $ "Testing word: " ++ word
  putStrLn $ "Expected: " ++ show expected ++ ", Got: " ++ show result
  putStrLn $ if result == expected then "Test passed!" else "Test failed!"

-- Main function to run all tests
main :: IO ()
main = do
    input <- readFile "input.txt"
    print (findWordInGrid (parseInput input) "XMAS")
