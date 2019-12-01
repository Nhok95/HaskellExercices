--donada una llista ordenada i un element, insereixi ordenadament el nou element a la llista.
insert :: [Int] -> Int -> [Int]
insert [] y = [y]  -- si llista buida, ara la llista te com a unic element y
insert [x] y 
    | x <= y = [x, y]
    | otherwise = [y, x]
insert (x:(xs:xss)) y
    | x > y = y: (x:(xs:xss))
    | x <= y && xs > y = x:[y] ++ (xs:xss)
    | otherwise = [x] ++ insert (xs:xss) y

--algorisme d’ordenació per inserció utilitzant la funció anterior.
isort :: [Int] -> [Int]
isort [] = []
isort [x] = [x]
isort (x:xs) = insert (isort xs) x   


--donada una llista i un element x, elimini la primera ocurrència de x de la llista. Podeu assumir que l’element sempre és a la llista.
remove :: [Int] -> Int -> [Int]
remove [] y = []
remove [x] y = []  --hem assumit que "y" esta sempre en la llista
remove (x:xs) y
    | x == y = xs  -- si el primer element es igual al element y nomes em queda com a resultat la llista restant
    |otherwise = [x] ++ remove xs y --si no tinc el primer element concatenat amb el resultat de aplicar remove a xs 

--algorisme d’ordenació per selecció utilitzant la funció anterior.
ssort :: [Int] -> [Int]
ssort [] = []
ssort [x] = [x]
ssort l = m : ssort (remove l m)  --concatenem l'element mes petit de la llista amb el que queda de la llista treient l'element m
    where m = minimum l     --m es l'element mes petit de la llista



--donades dues llistes ordenades, les fusioni per obtenir una llista amb tots els seus elements ordenats.
merge :: [Int] -> [Int] -> [Int]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(y:ys)               -- a = (x:xs) / b = (y:ys) 
    | x <= y    = x : (merge xs b)
    | otherwise = y : (merge a ys)

--algorisme d’ordenació per fusió utilitzant la funció anterior.
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort l = merge (msort (take lgth l))  (msort (drop lgth l))
    where lgth = length l `div` 2 


--algorisme d’ordenació ràpida.
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort menores ++ [x] ++ qsort mayores
    where menores = [y | y <- xs, y <= x]
          mayores = [y | y <- xs, y >  x]

--funció anterior generalizada per fer ara una funció que ordeni llistes de qualsevol tipus.
genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:xs) =
    let menores = genQsort (filter (<=x) xs)
        mayores = genQsort (filter (>x) xs)
    in  menores ++ [x] ++ mayores