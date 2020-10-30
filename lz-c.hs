import System.Environment
import System.IO
import Data.Char
import Numeric
import Data.Bits
import Data.List

data Phrase = Triplet Int Int Char
  deriving Show

currentOccur str buffer = findOccurrence str buffer
currentOccurSize str buffer currentOccur = findLength str buffer currentOccur
nextChunk currentOccur currentOccurSize buffer = drop (currentOccur + currentOccurSize) buffer

findStart str [] = 0
findStart str buffer
 | currentOccur == 0 = 0
 | otherwise = case currentOccurSize > findLength str nextChunk nextOccur of
  True  -> currentOccur
  False -> currentOccur + currentOccurSize + nextOccur + (findStart str nextChunk)
  where
  currentOccur = findOccurrence str buffer
  currentOccurSize = findLength str buffer currentOccur
  nextChunk = drop (currentOccur + currentOccurSize + 1) buffer
  nextOccur = findOccurrence str nextChunk

findOccurrence (s:str) buffer = case elemIndex s buffer of
  Nothing    -> 0
  Just index -> index

findLength str buffer start = findMatch (init str) (repeatPattern chopped repeatCount) where
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

getTriplets [] _ =  []
getTriplets [x] buffer = [Triplet (-1) 0 x]
getTriplets str buffer = Triplet start len nextChar : getTriplets (drop len (tail str)) (buffer ++ (take len str) ++ [nextChar]) where
  start = findStart str buffer
  len = findLength str buffer start
  nextChar = head (drop len str)

printTriplet (Triplet start length character) = (show start) ++ " " ++ (show length) ++ " " ++ (show $ ord character) 

--

buildTriplet line = Triplet (read start) (read len) (chr $ read character) where
  params    = words line
  start     = params!!0
  len       = params!!1
  character = params!!2

getStartFromTriplet (Triplet s _ _) = s
getLenFromTriplet   (Triplet _ l _) = l
getCharFromTriplet  (Triplet _ _ c) = c

assembleStr [] buffer = []
assembleStr (triplet:triplets) buffer = append ++ (assembleStr triplets (buffer ++ append)) where
  start     = getStartFromTriplet triplet
  len       = getLenFromTriplet triplet
  character = getCharFromTriplet triplet
  getRepeatedPattern s l = take len (drop start buffer)
  append = (getRepeatedPattern start len) ++ [character] 

--

main = do
  (path : _) <- getArgs
  handler <- openBinaryFile path ReadMode
  contents <- hGetContents handler
  let triplets = getTriplets contents []
  -- putStrLn $ show $ length triplets
  mapM_ putStrLn (map printTriplet triplets)