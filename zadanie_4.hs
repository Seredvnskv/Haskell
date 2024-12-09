pandigital :: Char -> String
pandigital '0' = "00"   --koniec rekurencji
pandigital 'A' = "AA" ++ pandigital '9' --przeskok z 10 na 9
pandigital c
        --dodaj 2 znaki do łańcucha i wywołaj rekurencyjnie dla mniejszego
    | (c >= '0' && c <= '9') || ( c > 'A' && c <= 'F' ) = replicate 2 (c)  ++ pandigital (toEnum (fromEnum c - 1)) 
   
    | otherwise = error "N poza zakresem!"

main :: IO ()
main = print (pandigital 'E')
