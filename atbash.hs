-- Jonathan Isay Bernal Arriaga
-- 02/03/2024

-- | This program implements an Atbash cipher. The Atbash cipher is a substitution technique
--   where each letter of the alphabet is replaced by its mirror letter. 
--   The program allows the user to choose between encrypting or decrypting a message. 
--   To encode, non-alphabetic characters are filtered out, the Atbash cipher is applied to 
--   each letter and grouped into sets of 5 characters. 
--   To decode, the Atbash cipher is applied to each letter after filtsering 
--   out the non-alphabetic characters.

import Data.Char (isAlpha, isLower, isUpper, chr, ord)

-- | The main function of the program.
atbash :: IO ()
atbash = do
    putStrLn "Enter 'encode' to encode a message or 'decode' to decode a message:"
    option <- getLine
    case option of
        "encode" -> do
            putStrLn "Enter the message to encode:"
            message <- getLine
            putStrLn $ "Encoded message: " ++ encode message
        "decode" -> do
            putStrLn "Enter the message to decode:"
            message <- getLine
            putStrLn $ "Decoded message: " ++ decode message
        _ -> putStrLn "Invalid option"

-- The function uses the 'ord' function to get the ASCII value of the input character,
-- performs the necessary calculations to find the mirror image character, and then uses
-- the 'chr' function to convert the ASCII value back to a character.
mirror :: Char -> Char
mirror c
    | isLower c = chr (ord 'a' + ord 'z' - ord c)
    | isUpper c = chr (ord 'A' + ord 'Z' - ord c)
    | otherwise = c

-- | Encodes a message.
--   It filters out non-alphabetic characters using the `isAlpha` function.
--   It maps each alphabetic character to its Atbash cipher equivalent using the `mirror` function.
--   It chunks the resulting list of characters into groups of 5 using the `chunks` function.
--   It converts each group of characters into a space-separated string using the `unwords` function.
encode :: String -> String
encode = unwords . chunks 5 . map mirror . filter isAlpha
    where
        chunks _ [] = []
        chunks n xs = take n xs : chunks n (drop n xs)

-- | Decodes a message encoded.
--   Applies the 'mirror' function to each character in a string after filtering out non-alphabetic characters.
--   The 'mirror' function replaces each letter with its mirror image in the alphabet.
decode :: String -> String
decode = map mirror . filter isAlpha
