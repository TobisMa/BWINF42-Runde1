{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use camelCase" #-}
import           Data.Bits
import           Data.Char (isDigit)

main :: IO ()
-- main = putStrLn( solve  ())))
main = do
    putStrLn "Enter Filepath:"
    fp <- getLine
    s <- readFile fp
    let l = filter (\x -> not (null x)) (lines s)
    let results = solve l
    putStrLn (unlines (map (\x->show x) (snd results)))
    putStrLn (pprint results (last l))

pprint :: ([Bool], [([Bool], [Bool])]) -> [Char] -> String
pprint (lights, results) lamps = "Results:\n" ++ foldl
        (\b x-> b ++ pprintSection "Q" lights (fst x) ++ "\n" ++ pprintSection "L" l (snd x) ++ "--------\n\n")
        ""
        results
    where
        l = map (== 'L') (tokenize lamps)


startCount :: Int
startCount = 1

pprintSection :: String -> [Bool] -> [Bool] -> String
pprintSection prefix lights lightState = fst (foldl (\acc i -> pprintLight prefix lights lightState i acc) ("", startCount) [0..length lights - 1])

pprintLight :: String -> [Bool] -> [Bool] -> Int -> (String, Int) -> (String, Int)
pprintLight prefix lights lightState index acc
    | not (lights !! index) = acc
    | otherwise = (fst acc ++ prefix ++ show (snd acc) ++ ": " ++ if lightState !! index then "On\n" else "Off\n", snd acc + 1)

lightInformation :: String -> [Bool] -> Int -> ([Bool], Int)
lightInformation (c:lightLine) lights count
    | null lightLine && c == ' ' = (lights, count)
    | null lightLine = (lights ++ [c == 'Q'], if c == 'Q' then count + 1 else count)
    | c == 'Q' = lightInformation lightLine (lights ++ [True]) (count + 1)
    | c == 'X' = lightInformation lightLine (lights ++ [False]) count
    | otherwise = lightInformation lightLine lights count


solve :: [String] -> ([Bool], [([Bool], [Bool])])
solve (_:start:field) = let  -- disregard of the first line in the file since it is not needed
    (lights, lightCount) = lightInformation start [] 0  -- obtaining bitmap of light/nolight
    in (
        lights,
        map (\x -> (buildInitialLight lights x 0, runBuilding field (buildInitialLight lights x 0))) [0..2 `intpow` lightCount - 1]  --calculating each option of the
    )


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


runBuilding :: [String] -> [Bool] -> [Bool]
runBuilding f light
    | null f || head f == "" = light
    | otherwise = let
        cur = head f
        nexts = tail f
        in
            runBuilding nexts (translateBlock (tokenize cur) light [] 0 False)

tokenize :: [Char] -> [Char]
tokenize = filter (\x -> x /= ' ' && not (isDigit x))

translateBlock :: [Char] -> [Bool] -> [Bool] -> Int -> Bool -> [Bool]
translateBlock tokenizedRow lights newLights index hadBrick
    | index >= length tokenizedRow - 1 = newLights
    | not hadBrick && brick tokenizedRow index == ('W', 'W') = let
        brickResult = whiteBrick (lightStateSegment lights index)
        in
            fst brickResult : snd brickResult : translateBlock tokenizedRow lights newLights (index + 1) True
    | not hadBrick && brick tokenizedRow index == ('B', 'B') = let
        brickResult = blueBrick (lightStateSegment lights index)
        in
            fst brickResult : snd brickResult : translateBlock tokenizedRow lights newLights (index + 1) True
    | not hadBrick && brick tokenizedRow index == ('R', 'r') = let
        brickResult = redBrickLSensor (lightStateSegment lights index)
        in
            fst brickResult : snd brickResult : translateBlock tokenizedRow lights newLights (index + 1) True
    | not hadBrick && brick tokenizedRow index == ('r', 'R') = let
        brickResult = redBrickRSensor (lightStateSegment lights index)
        in
            fst brickResult : snd brickResult : translateBlock tokenizedRow lights newLights (index + 1) True
    | tokenizedRow !! index == 'X' = False : translateBlock tokenizedRow lights newLights (index + 1) False
    | tokenizedRow !! index == 'L' = lights !! index : translateBlock tokenizedRow lights newLights (index + 1) False
    | otherwise = translateBlock tokenizedRow lights newLights (index + 1) False
    -- | otherwise = error ("invalid symbol " ++ [tokenizedRow !! index])



listTuple :: Show a => [a] -> Int -> (a, a)
-- listTuple lst lastIndex = if lastIndex >= 8
--     then error ("Error index " ++ show lastIndex ++ "  " ++ show lst ++ "  " ++ show (length lst))
--     else (lst !! (lastIndex - 1), lst !! lastIndex)
listTuple lst lastIndex = (lst !! (lastIndex - 1), lst !! lastIndex)

brick :: [Char] -> Int -> (Char, Char)
brick lst lastIndex = if lastIndex == 0 || lastIndex >= length lst 
    then ('N', 'N') 
    else listTuple lst lastIndex

lightStateSegment :: [Bool] -> Int -> (Bool, Bool)
lightStateSegment = listTuple -- only called if brick succeeded -> lastIndex will always be >= 1

whiteBrick :: (Bool, Bool) -> (Bool, Bool)
whiteBrick (left, right) = let res = not left && not right in (res, res)

blueBrick :: (Bool, Bool) -> (Bool, Bool)
blueBrick (left, right) = (left, right)

redBrickLSensor :: (Bool, Bool) -> (Bool, Bool)
redBrickLSensor (left, _) = let res = not left in (res, res)

redBrickRSensor :: (Bool, Bool) -> (Bool, Bool)
redBrickRSensor (_, right) = let res = not right in (res, res)
