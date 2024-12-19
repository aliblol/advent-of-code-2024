import System.IO
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

-- Extracts all mul values from a string, e.g., "mul(422,702)"
extractAllMulValues :: String -> [(Int, Int)]
extractAllMulValues [] = []
extractAllMulValues str =
    case extractMulValues str of
        Just (val, rest) -> val : extractAllMulValues rest
        Nothing -> extractAllMulValues (drop 1 str)

-- Extracts the first mul value from a string and returns the value and the rest of the string
extractMulValues :: String -> Maybe ((Int, Int), String)
extractMulValues str =
    case break (== 'm') str of
        (_, []) -> Nothing
        (beforeM, 'm':'u':'l':'(':rest) ->
            case span isDigit rest of
                (num1, ',':rest') ->
                    case span isDigit rest' of
                        (num2, ')':rest'') ->
                            Just ((read num1, read num2), rest'')
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing

-- Main function to read input file and process lines
main :: IO ()
main = do
    input <- readFile "input.txt"  -- Read the input file
    let values = extractAllMulValues input  -- Extract all mul values
    let products = map (uncurry (*)) values  -- Multiply each pair
    print (sum products)