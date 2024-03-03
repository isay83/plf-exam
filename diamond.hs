-- Jonathan Isay Bernal Arriaga
-- 02/03/2024

-- The user is prompted to enter a letter, and the program will print a diamond
-- pattern with the given letter at the center.

import Data.Char (ord, chr, toUpper, isLetter)
import System.IO (getChar)

-- | The main function of the program.
diamond :: IO ()
diamond = do
  putStr "Enter a letter: "
  letter <- getChar
  getChar -- | Clears the input buffer
  putStrLn ""
  if isLetter letter && letter /= 'Ñ' && letter /= 'ñ' -- There is a problem with this letters
    then make $ toUpper letter -- | Called function make with the letter in uppercase
    else putStrLn "Invalid character"

-- | Generates a diamond pattern with the given letter at the center.
make :: Char -> IO ()
make c = do
  -- | Calculates the numeric value of a character 'c' based on its position in the alphabet.
  -- The value is determined by subtracting the ASCII value of 'A' from the ASCII value of 'c',
  -- and adding 1 to the result.
  let n = ord c - ord 'A' + 1
      -- | The list of rows for creating a diamond pattern.
      -- The list starts with numbers from 1 to n, followed by numbers from n-1 to 1.
      rows = [1..n] ++ [n-1,n-2..1]
      -- | The list of column numbers for a diamond shape with n rows.
      cols = [1..2*n-1]
      -- | The matrix variable represents a 2-dimensional list that forms a diamond shape.
      -- Each element in the matrix is either a character representing a letter or a space.
      -- The diamond shape is created based on the conditions specified in the list comprehension.
      matrix = [[if r + c == n + 1 || c - r == n - 1 
            then chr (ord 'A' + r - 1) 
            else ' ' | c <- cols] | r <- rows]
  -- | Prints each element of the matrix on a new line.
  --   The matrix is printed using the 'putStrLn' function.
  mapM_ putStrLn matrix
