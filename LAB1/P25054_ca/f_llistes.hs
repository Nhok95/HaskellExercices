--donada una llista d’enters, calcula la seva llargada.
myLength:: [Int] -> Int 
myLength [] = 0
myLength [n] = 1
myLength (x:xs) = 1 + myLength (xs)

{-
myLength:: [Int] -> Int
myLength l = length[x | x <- l]
-}

--donada una llista d’enters no buida, calcula el seu màxim.
myMaximum :: [Int] -> Int
myMaximum [n] = n
myMaximum (x:xs) = max x (myMaximum xs)

{-
myMaximum:: [Int] -> Int
myMaximum [x] = x
myMaximum (x:y:xs) = if x > y
                        then myMaximum (x:xs)
                        else myMaximum (y:xs)
-}

--donada una llista d’enters no buida, calcula la seva mitjana.
sumal:: [Int] -> Int
sumal [] = 0
sumal [n] = n
sumal (x:xs) = x + sumal xs

average :: [Int] -> Float
average [n] = fromIntegral n
average l = fromIntegral (sumal l)/ fromIntegral (myLength l) 

--donada una llista, retorna el palíndrom que comença amb la llista invertida.
invertllista :: [Int] -> [Int]
invertllista [] = []
invertllista (x:xs) = invertllista xs ++ [x] -- coloquem el primer element al final 
                                             -- y tornem a aplicar la funció a el que queda

buildPalindrome :: [Int] -> [Int] 
buildPalindrome [] = []
buildPalindrome l =  invertllista l ++ l   --llista invertida concatenada(++) amb la llista original (l)

--donada una llista d’enters x i una llista d’enters y, retorna la llista x havent eliminat totes les ocurrències dels elements en y.

remove :: [Int] -> [Int] -> [Int] 
remove l [] = l
remove l1 (x:xs) =  remove [y | y <- l1,  y /= x] xs

--aplana una llista de llistes produint una llista d’elements.
flatten :: [[Int]] -> [Int] 
flatten [] = []
flatten (x:xs) = x ++ flatten xs 

--donada una llista d’enters, retorni dues llistes, una que conté els parells i una que conté els senars, en el mateix ordre relatiu que a l’original.
evenl :: [Int] -> [Int]
evenl [] = []
evenl (x:xs) = if even x
                  then x: evenl xs
                  else evenl xs

oddl :: [Int] -> [Int]
oddl [] = []
oddl (x:xs) = if odd x
                  then x: oddl xs
                  else oddl xs


oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens l = (oddl l, evenl l)

--retorna la llista de divisors primers d’un enter estrictament positiu.
auxPrime:: Int ->Int ->Int
auxPrime n d
    |d == 0  = 0
    |otherwise = let x = mod n d
                 in if x == 0 then  (auxPrime n (d-1)) + 1
                              else (auxPrime n (d-1))
--n es primer
isPrime:: Int->Bool
isPrime n = (auxPrime n n) == 2

listPrime :: Int -> Int -> [Int]
listPrime x 1 = []
listPrime x y = let z = mod x y
                in if ((auxPrime y y) == 2 && z == 0)
                   then listPrime x (y-1) ++ [y]
                   else listPrime x (y-1)


primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors x
            | isPrime x = [x]
            | otherwise = listPrime x (x-1)
