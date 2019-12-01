{-
--funció que aplana una llista de llistes d’enters en una llista d’enters. (alter)
flatten :: [[Int]] -> [Int] 
flatten x = foldl1 (++) x   -- foldl1 no necesita elemento inicial, en su lugar aplica la funcion (++ en este caso) a los dos primeros elementos
-}

--funció que aplana una llista de llistes d’enters en una llista d’enters.
flatten :: [[Int]] -> [Int] 
flatten x = foldl (++) [] x

{-
--funció que retorna la llargada d’una cadena de caràcters. (alter)
--myLength :: String -> Int 
--myLength x = length x

--funciö que retorna la llargada d’una cadena de caràcters. (alter2)
myLength :: String -> Int 
myLength x = foldl1 (+) (map (const 1) x)
-}

--funciö que retorna la llargada d’una cadena de caràcters.
myLength :: String -> Int 
myLength x = foldl (+)  0 (map (const 1) x)

{---funció que inverteix els elements d’una llista d’enters. (alt)
myReverse :: [Int] -> [Int] 
myReverse x = foldl (flip (++)) [] (map (:[]) x)
                                    --convierto cada elemento en una lista
-}

--funció que inverteix els elements d’una llista d’enters.
myReverse :: [Int] -> [Int] 
myReverse x = foldr (flip (++)) [] (map (:[]) x)
                                    --convierto cada elemento en una lista 
                                    


--funció que, donada una llista de llistes d’elements ℓ i un element x ens torna la llista que indica quants cops apareix x en cada llista de ℓ.
countOcc :: Int -> [Int] -> Int
countOcc n x = sum(map(const 1)(filter (== n) x))  --contador de elements x en una llista

countIn :: [[Int]] -> Int -> [Int]
countIn x n = map (countOcc n) x   --aplica el contador a cada subllista de la llista

--funció que, donat un string amb blancs i caràcacters alfabètics, en retorna la primera paraula.
firstWord :: String -> String
firstWord x = takeWhile (/= ' ') (dropWhile (== ' ') x)

