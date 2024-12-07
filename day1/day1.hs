-- What is the total distance between your lists?
-- distance = sum of the absolute differences between the corresponding elements of the two lists
import System.IO
import Data.List

parseInput :: String -> ([Int], [Int])
parseInput input =
    let pairs = map ((\[a, b] -> (read a, read b)) . words) (lines input)
    in unzip pairs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (list1, list2) = parseInput input
    print . sum . map abs $ zipWith (-) (sort list1) (sort list2)