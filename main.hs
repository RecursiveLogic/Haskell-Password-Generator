import Data.Char
import Data.List
import System.Random

main :: IO ()
main = do
    greetings
    ioActions

greetings :: IO ()
greetings = putStrLn "\n*** Welcome to the Haskell Password Generator ***"

ioActions :: IO () 
ioActions = do
    charTypes <- getCharacterTypes
    if valid charTypes
        then do 
            makePasswords charTypes
            promptToRepeat
        else invalidCharTypes

getCharacterTypes :: IO String
getCharacterTypes = do
    putStrLn "\nPlease select the types of characters you want to include."
    putStrLn "(e.g., \"2 3\", \"1 2 3 4\", etc.): \n"
    putStrLn "  (1) Uppercase letters"
    putStrLn "  (2) Lowercase letters"
    putStrLn "  (3) Numerals: 1 - 0"
    putStrLn "  (4) Symbols: ~ ! # ..."
    getLine

makePasswords :: String -> IO ()
makePasswords types = do
    putStrLn "Enter the number of desired passwords:"
    number <- fmap read getLine
    putStrLn "Enter length of password:"
    length <- fmap read getLine
    generator <- getStdGen
    -- Password
    let password = passwordGen types (number * length) generator
    putStrLn ("\n" ++ format (slices length password))

promptToRepeat :: IO ()
promptToRepeat = do
    putStrLn "Would you like another set of passwords? (y/_)"
    response <- getLine
    if response `elem` ["Y", "y", "Yes", "yes", "YES"]
        then do
            newStdGen
            ioActions
        else putStrLn "Looks like we're done here...bye!\n"

invalidCharTypes :: IO ()
invalidCharTypes = do
    putStrLn "\n - Invalid Entry -"
    ioActions

{- Pure Functions -}

valid :: String -> Bool
valid = not . null .toIndices

toIndices :: String -> [Int]
toIndices = sort . nub . map (\c -> ord c - ord '1') . filter (`elem` "1234")

availableChars :: [Int] -> String
availableChars = concat . map (chars !!)
    where chars = [['A'..'Z'], ['a'..'z'], "1234567890", "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"]

passwordGen :: (RandomGen g) => String -> Int -> g -> String
passwordGen str len gen = map (chars !!) $ take len (randomRs (0, max) gen)
    where 
        chars = availableChars (toIndices str)
        max = length chars - 1

slices :: Int -> String -> [String]
slices 0 _ = []
slices _ "" = []
slices len str = ("   " ++ take len str) : slices len (drop len str)

format :: [String] -> String
format [] = ""
format strs = (concat . take 5) strs ++ "\n" ++ format (drop 5 strs)