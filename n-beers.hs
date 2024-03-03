-- Jonathan Isay Bernal Arriaga
-- 02/03/2024

-- | This program is a simple implementation of the "99 Bottles of Beer" song.
-- It generates the lyrics for a given number of bottles and prints them to the console.

-- | The main function of the program.
beer :: IO ()
beer = do
  putStr "Enter the number of bottles: "
  n <- readLn -- | Reads an integer from standard input
  putStrLn "" 
  if n > 0
    then putStrLn $ song n
    else putStrLn "Invalid number of bottles"
  

-- | Returns the string representation of the number of bottles of beer.
bottlesOfBeer :: Int -> String
bottlesOfBeer 0 = "No more bottles of beer"
bottlesOfBeer 1 = "1 bottle of beer"
-- | The input parameter 'n' represents the number of bottles.
bottlesOfBeer n = show n ++ " bottles of beer"

-- | Returns a verse of the song based on the number of bottles.
verse :: Int -> String
verse n
  | n == 1 = bottlesOfBeer n ++ " on the wall, " ++ bottlesOfBeer n ++ ".\n" ++
             "Take it down and pass it around, " ++ bottlesOfBeer (n - 1) ++ " on the wall.\n\n"
  | otherwise = bottlesOfBeer n ++ " on the wall, " ++ bottlesOfBeer n ++ ".\n" ++
                "Take one down and pass it around, " ++ bottlesOfBeer (n - 1) ++ " on the wall.\n\n"

-- | Returns the entire song based on the number of bottles.
song :: Int -> String
song n = concatMap verse [n, n-1 .. 1] ++
                 bottlesOfBeer 0 ++ " on the wall, " ++ bottlesOfBeer 0 ++ ".\n" ++
                 "Go to the store and buy some more, " ++ bottlesOfBeer n ++ " on the wall."
