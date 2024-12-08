-- ZADANIE 22 --
palindromDziesietny :: Int -> Bool
palindromDziesietny n = 
    let number = show n -- zapisuje liczbe n jako string -- 
        reversedNumber = reverse number 
    in if number == reversedNumber then True
       else False

-- zamienia liczbe na binarana i zapisuje jako ciag znakow --
zamienNaBinarny :: Int -> String
zamienNaBinarny n 
  | n == 0 = "0"
  | otherwise = reverse (zamienNaBinarny_(n))

zamienNaBinarny_ :: Int -> String
zamienNaBinarny_ n
  | n == 0 = ""
  | otherwise = show (n `mod` 2) ++ zamienNaBinarny_ (n `div` 2)

palindromBinarny :: String -> Bool 
palindromBinarny n = 
  let reversedBinary = reverse n
  in if n == reversedBinary then True
     else False

generujPalindromy :: Int -> ([Int], Int)
generujPalindromy n
  | n < 0 = ([], 0)
  | otherwise =
    if palindromDziesietny(n) == True then
      if palindromBinarny(zamienNaBinarny(n)) == True then
        -- zapisuje wynik funkcji w zmiennych lista i suma po czym dodaje ta liczbe do listy i sumy --
      let (lista, suma) = generujPalindromy(n - 1) in (n : lista, suma + n)
      else generujPalindromy(n -1)
    else generujPalindromy(n - 1)
  
main :: IO ()
main = print(generujPalindromy(10))