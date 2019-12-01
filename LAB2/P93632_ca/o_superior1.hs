{- sin orden superior
eql :: [Int] -> [Int] -> Bool
eql [] [] = True
eql (x:xs) (y:ys) = head(x:xs) == head(y:ys) && length(x:xs) == length(y:ys)
-}

--funció que indiqui si dues llistes d’enters són iguals.
eql :: [Int] -> [Int] -> Bool
eql a b = length a == length b && all (==True) (zipWith (==) a b)

{- --funcio que calculi el producte dels elements d’una llista d’enters. (alter)
prod :: [Int] -> Int
prod (x:xs) = foldr (*) x xs
-}

--funcio que calculi el producte dels elements d’una llista d’enters.
prod :: [Int] -> Int
prod x = product x

--funció que multiplica tots el nombres parells d’una llista d’enters.
prodOfEvens :: [Int] -> Int 
prodOfEvens x = prod (filter even x)


--funció que generi la llista de totes les potències de 2. 
powersOf2 :: [Int]
powersOf2 = iterate (2*) 1


--funció que calculi el producte escalar de dues llistes de reals de la mateixa mida.
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct a b = sum (zipWith (*) a b)
