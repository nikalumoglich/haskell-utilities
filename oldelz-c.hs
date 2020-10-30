import System.Environment
import System.IO
import Data.Char
import Numeric
import Data.Bits
import Data.List

data Phrase = Triplet Int Int Char
  deriving Show

findStart (s:str) buffer = case elemIndex s buffer of
  Nothing    -> -1
  Just index -> index

findLength str buffer start
  | match == 1 && start /= -1 = 0
  | otherwise = match
  where
  chopped = drop start buffer
  repeatCount
    | length chopped == 0 = 0
    | (length str) < (length chopped) = 1
    | otherwise = (length str) `div` (length chopped)
  repeatPattern pattern 0 = []
  repeatPattern pattern count =  pattern ++ (repeatPattern pattern (count - 1))
  findMatch _ [] = 0
  findMatch [] _ = 0
  findMatch (p:ps) (q:qs)  
    | p == q = 1 + findMatch ps qs
    | otherwise = 0
  match = findMatch str (repeatPattern chopped repeatCount) 

getTriplets [] _ =  []
getTriplets str buffer = Triplet start len nextChar : getTriplets (drop len (tail str)) (buffer ++ (take len str) ++ [nextChar]) where
  start = findStart str buffer
  len = findLength str buffer start
  nextChar = head (drop len str)
  --  | length str > 1 = head (drop len str)
  --  | otherwise = head str

printTriplet (Triplet start length character) = (show start) ++ " " ++ (show length) ++ " " ++ [character] 

main = do
  -- input <- getLine
  -- fileContents <- readFile input
  -- putStrLn $ compress input --fileContents
  -- putStrLn fileContents
  -- mapM_ putStrLn (map printTriplet (getTriplets fileContents []))
  (path : _) <- getArgs
  handler <- openBinaryFile path ReadMode
  contents <- hGetContents handler
  -- no need to close h
  --putStrLn $ show contents
  mapM_ putStrLn (map printTriplet (getTriplets contents []))

-- badadadabaab
-- b (0,0,b)
-- a (0,0,a)
-- d (0,0,d)
-- adadab (2,5)
-- aa (2,1,a)
-- b (0,0,b)