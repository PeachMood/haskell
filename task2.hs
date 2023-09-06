import Data.Char hiding (digitToInt, intToDigit)

toDecimal :: Integer -> [Char] -> String
toDecimal base snumber = if base == 1 then show (turingMachine base snumber) else show (decimalNumber base snumber)
        where turingMachine :: Integer -> [Char] -> Integer
              turingMachine base snumber = foldl (\acc digit -> acc + digitToInt (correctInput base digit)) (-1) snumber
              decimalNumber :: Integer -> [Char] -> Integer
              decimalNumber base snumber = foldl (\acc digit -> acc*base + digitToInt (correctInput base digit)) (digitToInt (correctInput base (head snumber))) (tail snumber)
              digitToInt :: Char -> Integer
              digitToInt digit
                |digit >= '0' && digit <= '9' = toInteger (ord digit - ord '0')
                |digit >= 'a' && digit <= 'z' = toInteger (ord digit - ord 'a' + 10)
                |digit >= 'A' && digit <= 'Z' = toInteger (ord digit - ord 'A' + 36)
                |otherwise = error "Incorrect number input"
              correctInput :: Integer -> Char -> Char
              correctInput base digit
                |base > 61 || base < 1 = error "Incorrect number system input"
                |base == 1 && digit == '1' = digit
                |digitToInt digit >= 0 && digitToInt digit < base = digit
                |otherwise = error "Incorrect number input"

fromDecimal :: Integer -> [Char] -> [Char]
fromDecimal toBase snumber = if toBase == 1 then turingMachine (toNumber toBase snumber) else baseNumber toBase (toNumber toBase snumber) []
        where turingMachine :: Integer -> [Char]
              turingMachine decimalNumber
                |decimalNumber == 0 = "1"
                |otherwise = '1' : turingMachine (decimalNumber - 1)
              baseNumber :: Integer -> Integer -> [Char] -> [Char]
              baseNumber toBase decimalNumber snumber
                |decimalNumber >= toBase = baseNumber toBase (decimalNumber `div` toBase) (intToDigit (decimalNumber `mod` toBase) : snumber)
                |otherwise = (intToDigit (decimalNumber `mod` toBase)) : snumber
              intToDigit :: Integer -> Char
              intToDigit digit
                |digit >= 0 && digit <= 9 = chr (ord '0' + fromInteger digit)
                |digit >= 10 && digit <= 35 = chr (ord 'a' + fromInteger digit - 10)
                |digit >= 36 && digit <= 61 = chr (ord 'A' + fromInteger digit - 36)
              toNumber :: Integer -> [Char] ->Integer
              toNumber toBase snumber = toInteger (foldl (\acc digit-> acc*10 + ord (correctInput toBase digit) - ord '0') (ord (correctInput toBase (head snumber)) - ord '0') (tail snumber))
              correctInput :: Integer -> Char -> Char
              correctInput toBase digit
                |toBase > 61 || toBase < 1 = error "Incorrect number system input"
                |digit >= '0' && digit <='9' = digit
                |otherwise = error "Incorrect number input"

convertFromTo :: Integer -> Integer -> [Char] -> [Char]
convertFromTo fromBase toBase snumber
  |toBase == 10 = toDecimal fromBase snumber
  |otherwise = fromDecimal toBase (toDecimal fromBase snumber)
