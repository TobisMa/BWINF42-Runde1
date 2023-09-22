{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
import Data.Bits
import Data.Char (isDigit)

main :: IO ()
main = do
    putStr "Filepath: "
    fp <- getLine
    s <- readFile fp
    putStrLn (unlines (solve (lines s)))


lightInformation :: String -> [Bool] -> Int -> ([Bool], Int)
lightInformation (c:lightLine) lights count
    | null lightLine && c == ' ' = (lights, count)
    | null lightLine = (lights ++ [c == 'Q'], if c == 'Q' then count + 1 else count)
    | c == 'Q' = lightInformation lightLine (lights ++ [True]) (count + 1)
    | c == 'X' = lightInformation lightLine (lights ++ [False]) count
    | otherwise = lightInformation lightLine lights count


solve :: [String] -> [String]
solve (_:start:field) = let  -- disregard of the first line in the file since it is not needed
    (lights, lightCount) = lightInformation start [] 0
    in map (\x -> runBuilding field (buildInitialLight lights x 0)) [0..2 `intpow` lightCount - 1]


buildInitialLight :: [Bool] -> Int -> Int -> [Bool]
buildInitialLight lightPositions lightCode count
    | null lightPositions = []
    | otherwise =
        let pos = head lightPositions
        in (pos && (.&.) lightCode (2 `intpow` count) == 2 `intpow` count) : buildInitialLight (tail lightPositions) lightCode (if pos then count + 1 else count)


intpow :: (Num t1, Num t2, Eq t1, Eq t2) => t2 -> t1 -> t2
intpow a b
    | b == 0 = 1
    | a == 0 = 0
    | otherwise = a * intpow a (b-1)


runBuilding :: [String] -> [Bool] -> String
runBuilding f light
    | null f || head f == "" = show light
    | otherwise = let
        cur = head f
        nexts = tail f
        in
            runBuilding nexts (translateBlock (tokenize cur) light [] 0)

tokenize :: [Char] -> [Char]
tokenize = filter (\x -> x /= ' ' && not (isDigit x))

translateBlock :: [Char] -> [Bool] -> [Bool] -> Int -> [Bool]
translateBlock tokenizedRow lights newLights index
    | index >= length tokenizedRow = newLights
    | brick tokenizedRow index == ('W', 'W') = let
    brickResult = whiteBrick (lightStateSegment lights index)
    in
        fst brickResult : snd brickResult : translateBlock tokenizedRow lights newLights (index + 1)
    | brick tokenizedRow index == ('B', 'B') = let
    brickResult = blueBrick (lightStateSegment lights index)
    in
        fst brickResult : snd brickResult : translateBlock tokenizedRow lights newLights (index + 1)
    | brick tokenizedRow index == ('R', 'r') = let
    brickResult = redBrickLSensor (lightStateSegment lights index)
    in
        fst brickResult : snd brickResult : translateBlock tokenizedRow lights newLights (index + 1)
    | brick tokenizedRow index == ('r', 'R') = let
    brickResult = redBrickRSensor (lightStateSegment lights index)
    in
        fst brickResult : snd brickResult : translateBlock tokenizedRow lights newLights (index + 1)
    | tokenizedRow !! index == 'X' = False : translateBlock tokenizedRow lights newLights (index + 1)
    | tokenizedRow !! index == 'L' = lights !! index : translateBlock tokenizedRow lights newLights (index + 1)
    | otherwise = translateBlock tokenizedRow lights newLights (index + 1)
    -- | otherwise = error ("invalid symbol " ++ [tokenizedRow !! index])



listTuple :: [a] -> Int -> (a, a)
listTuple lst lastIndex = (lst !! (lastIndex - 1), lst !! lastIndex)

brick :: [Char] -> Int -> (Char, Char)
brick lst lastIndex = if lastIndex == 0 then ('N', 'N') else listTuple lst lastIndex

lightStateSegment :: [Bool] -> Int -> (Bool, Bool)
lightStateSegment = listTuple -- only called if brick succeeded -> lastIndex will always be >= 1


whiteBrick :: (Bool, Bool) -> (Bool, Bool)
whiteBrick (left, right) = let res = not (left && right) in (res, res)

blueBrick :: (Bool, Bool) -> (Bool, Bool)
blueBrick (left, right) = (left, right)

redBrickLSensor :: (Bool, Bool) -> (Bool, Bool)
redBrickLSensor (left, _) = let res = not left in (res, res)

redBrickRSensor :: (Bool, Bool) -> (Bool, Bool)
redBrickRSensor (_, right) = let res = not right in (res, res)
