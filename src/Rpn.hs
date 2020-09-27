module Rpn
  ( rpn,
  )
where

rpn :: (Eq a, Fractional a, Read a) => String -> Either a String
rpn = head . foldl foldingFunction [] . parseWords
  where
    foldingFunction ((Right x) : xs) _ = [Right x]
    foldingFunction ((Left x) : (Left y) : ys) ('*' : _) = (Left (x * y)) : ys
    foldingFunction ((Left x) : (Left 0) : ys) ('/' : _) = [Right "Zero division"]
    foldingFunction ((Left x) : (Left y) : ys) ('/' : _) = (Left (x / y)) : ys
    foldingFunction ((Left x) : (Left y) : ys) ('+' : _) = (Left (x + y)) : ys
    foldingFunction ((Left x) : (Left y) : ys) ('-' : _) = (Left (x - y)) : ys
    foldingFunction xs numberString = parseNumber numberString : xs

parseNumber :: (Fractional a, Read a) => String -> Either a String
parseNumber str = case reads str of
  [(n, _)] -> Left n
  _ -> Right $ "No parse for " ++ str

parseWords :: String -> [String]
parseWords str = split str ' '

split :: String -> Char -> [String]
split str delim = splitHelper str delim ""

-- Given String and a delimitor char splits a String into a list of Strings
-- The last parameter is an accumulator
-- the String is "abcd  abcd abcd" delimitor is " "
splitHelper :: String -> Char -> String -> [String]
splitHelper "" _ "" = []
splitHelper "" _ r = [r]
splitHelper (x : xs) delim "" -- if accumulator is empty - proceed to the next element
  | x == delim = splitHelper xs delim "" -- "abcd @abcd abcd"
  | otherwise = splitHelper xs delim [x] -- "@bcd @bcd @bcd"
splitHelper (x : xs) delim acum -- if accumulator is not empty
  | x == delim = acum : splitHelper xs delim "" -- "abcd@ abcd@abcd"
  | otherwise = splitHelper xs delim (acum ++ [x]) -- "a@@@ a@@@ a@@@"
