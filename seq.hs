import Data.Char

sequenc start 0      = 0
sequenc start digits = (start * (10 ^ (digits - 1))) + (sequenc (start + 1) (digits - 1))

main :: IO ()
main = do
	p <- getLine
	let input = read p

	q <- getLine
	let digits = read q

	putStrLn $ show (sequenc input digits)
	
