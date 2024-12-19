import System.IO
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow)

-- Extracts all mul values from a string, e.g., "mul(422,702)"
extractAllMulValues :: Bool -> String -> [(Int, Int)]
extractAllMulValues _ [] = []
extractAllMulValues enabled str =
    case extractInstruction str of
        Just (instruction, rest) -> 
            case instruction of
                Mul val -> 
                    if enabled 
                    then traceShow instruction (val : extractAllMulValues enabled rest) 
                    else traceShow instruction (extractAllMulValues enabled rest)
                Do -> traceShow instruction (extractAllMulValues True rest)
                Dont -> traceShow instruction (extractAllMulValues False rest)
        Nothing -> extractAllMulValues enabled (drop 1 str)


data Instruction = Mul (Int, Int) | Do | Dont
    deriving (Show)

-- Extracts the first instruction from a string and returns the instruction and the rest of the string
extractInstruction :: String -> Maybe (Instruction, String)
extractInstruction str =
    case break (`elem` "md") str of
        (_, []) -> Nothing
        (before, 'd':'o':'(':')':rest) -> Just (Do, rest)
        (before, 'd':'o':'n':'\'':'t':'(':')':rest) -> Just (Dont, rest)
        (before, 'm':'u':'l':'(':rest) ->
            case span isDigit rest of
                (num1, ',':rest') ->
                    case span isDigit rest' of
                        (num2, ')':rest'') ->
                            Just (Mul (read num1, read num2), rest'')
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing

-- Main function to read input file and process lines
main :: IO ()
main = do
    input <- readFile "input.txt"  -- Read the input file
    let values = extractAllMulValues True input  -- Extract all mul values
    let products = map (uncurry (*)) values  -- Multiply each pair
    print (sum products)