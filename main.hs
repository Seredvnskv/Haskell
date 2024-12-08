import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- ZADANIE 22 --
palindromDziesietny :: Int -> Bool
palindromDziesietny n = 
    let number = show n -- zapisuje liczbe n jako string -- 
        reversedNumber = reverse number 
    in if number == reversedNumber then True
       else False

-- zamienia liczbe na binarana i zapisuje jako ciag znakow --
zamienNaBinarny :: Int -> String
zamienNaBinarny n = showIntAtBase 2 intToDigit n ""

palindromBinarny :: String -> Bool 
palindromBinarny n = 
  let reversedBinary = reverse n
  in if n == reversedBinary then True
     else False

generujPalindromy :: Int -> ([Int], Int)
generujPalindromy n
  | n < 0 = ([], 0)
  | otherwise =
    if palindromDziesietny(n) && palindromBinarny(zamienNaBinarny(n)) == True then
      -- zapisuje wynik funkcji w zmiennych lista i suma po czym dodaje do liczbe do listy i sumy --
      let (lista, suma) = generujPalindromy(n - 1) in (n : lista, suma + n) 
    else generujPalindromy(n - 1)
  
main = print(generujPalindromy(10))