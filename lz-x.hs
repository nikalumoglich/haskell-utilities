import System.Environment
import System.IO
import Data.Char
import Numeric
import Data.Bits
import Data.List
import Data.List

data Phrase = Triplet Int Int Char
  deriving Show

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
  chopped = drop start buffer
  repeatCount
    | length chopped == 0 = 0
    | (length str) < (length chopped) = 1
    | otherwise = (length str) `div` (length chopped)
  repeatPattern pattern 0 = []
  repeatPattern pattern count =  pattern ++ (repeatPattern pattern (count - 1))

  getRepeatedPattern s l = take len (drop start buffer)
  append = (getRepeatedPattern start len) ++ [character]

main = do
  (path : _) <- getArgs
  handler <- openBinaryFile path ReadMode
  contents <- hGetContents handler
  putStrLn (assembleStr (map buildTriplet (lines contents)) []) --(map printTriplet (getTriplets contents []))