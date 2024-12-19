type Grid = [String]

type Position = (Int, Int)

patternXMAS :: [([Position], [Char])]
patternXMAS =
  [ ([(0, 0), (2, 0), (1, 1), (0, 2), (2, 2)], "MMASS"),
    ([(0, 0), (0, 2), (1, 1), (2, 0), (2, 2)], "MMASS"),
    ([(2, 2), (0, 2), (1, 1), (2, 0), (0, 0)], "MMASS"),
    ([(2, 2), (2, 0), (1, 1), (0, 2), (0, 0)], "MMASS")
  ]

exampleGrid :: Grid
exampleGrid =
  [ "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
  ]

expectedOutput :: Grid
expectedOutput =
  [ ".M.S......",
    "..A..MSMS.",
    ".M.S.MAA..",
    "..A.ASMSM.",
    ".M.S.M....",
    "..........",
    "S.S.S.S.S.",
    ".A.A.A.A..",
    "M.M.M.M.M.",
    ".........."
  ]

exampleTestCase :: Int
exampleTestCase = 9

-- Function to check if a pattern matches at a given starting point
matchPattern :: Grid -> ([Position], [Char]) -> Position -> Bool
matchPattern grid (pattern, letters) (startX, startY) =
  all (\((dx, dy), expectedChar) -> inBounds (startX + dx, startY + dy) && grid !! (startX + dx) !! (startY + dy) == expectedChar) (zip pattern letters)
  where
    inBounds (x, y) = x >= 0 && x < length grid && y >= 0 && y < length (head grid)

-- Detect all patterns in the grid
detectPatterns :: Grid -> [([Position], [Char])] -> [(Position, [Position])]
detectPatterns grid patterns =
  [((x, y), pattern) | x <- [0 .. length grid - 1], y <- [0 .. length (head grid) - 1], (pattern, letters) <- patterns, matchPattern grid (pattern, letters) (x, y)]

-- Generate the output grid based on detected patterns
generateOutputGrid :: Grid -> [(Position, [Position])] -> Grid
generateOutputGrid grid matches = 
  [ [ if (x, y) `elem` markedPositions then grid !! x !! y else '.' | y <- [0 .. length (head grid) - 1] ] | x <- [0 .. length grid - 1] ]
  where
    markedPositions = concatMap (\((startX, startY), pattern) -> map (\(dx, dy) -> (startX + dx, startY + dy)) pattern) matches

-- Parse the input file into a grid 
parseInput :: String -> Grid
parseInput = lines

-- Main function to calculate the total number of pattern matches and generate the output grid
main :: IO ()
main = do
  let matches = detectPatterns exampleGrid patternXMAS
  let result = length matches
  let outputGrid = generateOutputGrid exampleGrid matches

  putStrLn $ "Expected: " ++ show exampleTestCase ++ ", Got: " ++ show result
  putStrLn "Expected Output Grid:"
  mapM_ putStrLn expectedOutput
  putStrLn "Generated Output Grid:"
  mapM_ putStrLn outputGrid

  if outputGrid == expectedOutput
    then putStrLn "Output matches expected output."
    else putStrLn "Output does not match expected output."

  input <- readFile "input.txt"
  let grid = parseInput input
  let matches = detectPatterns grid patternXMAS
  let result = length matches
  print result