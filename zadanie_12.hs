--ZADANIE 12--

-- Funkcja generująca listę ułamków w postaci par (licznik, mianownik)
ulamki :: Int -> [(Int, Int)]
ulamki n = [(1,d) | d <- [2..n]]

-- Funkcja obliczająca długość okresu binarnego dla danego ułamka
dlugoscOkresu :: Int -> Int -> Int
dlugoscOkresu licznik mianownik = znajdzCykl licznik [] 0
  where
    znajdzCykl :: Int -> [Int] -> Int -> Int
    znajdzCykl reszta odwiedzone liczba_iteracji
      |reszta == 0 = 0 --rozwiniecie skonczone
      |reszta `elem` odwiedzone = liczba_iteracji - index reszta odwiedzone
      | otherwise = 
        let nowaReszta = (reszta*2) `mod` mianownik
        in znajdzCykl nowaReszta (odwiedzone ++ [reszta])(liczba_iteracji+1)

    -- Funkcja do znajdowania indeksu elementu w liście
    index :: Int -> [Int] -> Int
    index x xs = znajdzIndex x xs 0
      where
        znajdzIndex _ [] _ = error "Nie znaleziono elementu"
        znajdzIndex y (z:zs) i
          | y == z    = i
          | otherwise = znajdzIndex y zs (i+1)


-- Funkcja generująca binarne rozwinięcie ułamka
binarneRozwiniecie :: Int -> Int -> String
binarneRozwiniecie licznik mianownik = generujBinarne (licznik `mod` mianownik) []
  where
    generujBinarne :: Int -> [Int] -> String
    generujBinarne reszta odwiedzone
      | reszta == 0 = ""  -- Skończone rozwinięcie
      | reszta `elem` odwiedzone = ""  -- Cykl (koniec rozwinięcia)
      | otherwise = 
          let nowaReszta = (reszta * 2) `mod` mianownik
              cyfra = if (reszta * 2) >= mianownik then '1' else '0'
          in cyfra : generujBinarne nowaReszta (odwiedzone ++ [reszta])

-- Funkcja formatująca rozwinięcie binarne
formatRozwiniecie :: Int -> Int -> Int -> String
formatRozwiniecie licznik mianownik dlugoscOkresu =
    let rozw = binarneRozwiniecie licznik mianownik
        dlugoscPrefiksu = length rozw - dlugoscOkresu
        (prefix, cykl) = splitAt dlugoscPrefiksu rozw
    in "1/" ++ show mianownik ++ " = 0." ++ prefix ++ "(" ++ cykl ++ ")"

-- Funkcja do generowania listy ułamków z najdłuższymi okresami
najdluzszyOkres :: Int -> [String]
najdluzszyOkres n =
    let listaUlamkow = ulamki n
        okresy = [(l, m, dlugoscOkresu l m) | (l, m) <- listaUlamkow]
        maxOkres = maximum [dl | (_, _, dl) <- okresy]
        maxUlamki = [(l, m, dl) | (l, m, dl) <- okresy, dl == maxOkres]
    in [formatRozwiniecie l m dl | (l, m, dl) <- maxUlamki]

main :: IO ()
main = print (najdluzszyOkres 10)
