--funció que, donat un predicat sobre els enters i una llista d’enters, retorna el nombre d’elements dela llista que satisfan el predicat.
--Nota: Aquesta funció d’ordre superior existeix en llenguatges de tractament de fulls de càlcul com ara EXCEL.
countIf :: (Int -> Bool) -> [Int] -> Int
countIf f xs =  sum [1 | x <- xs, f x]


--funció que, donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes resultant d’aplicar cada una de les funcions de la segona llista als elements de la primera llista.
pam :: [Int] -> [Int -> Int] -> [[Int]] 
pam xs fs = [ map f xs | f <- fs  ]


--funció que, donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes on cada llista és el resultat d’aplicar successivament les funcions de la segona llista a cada element de la primera llista.
--Nota: Qualsevol semblança amb La parte contratante de la primera parte será considerada como la parte contratante de la primera parte és pura casualitat.
pam2_aux :: Int -> [Int -> Int] -> [Int]
pam2_aux x fs = [f x | f <- fs]

pam2 :: [Int] -> [Int -> Int] -> [[Int]] 
pam2 xs fs = [pam2_aux x fs | x <- xs]


--funció que fa el plegat dels elements que satisfan la propietat donada. 
filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl f f2 x ys = foldl f2 x (filter f ys)


--funció que donada una relació entre enters, una llista i un element, ens retorna la llista amb l’element inserit segons la relació.
insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] 
insert f [] y = [y]
insert f [x] y 
    |f x y == True  = [x,y]
    | otherwise     = [y,x]
insert f (x:(xs:xss)) y
    |f x y == False  = y:(x:(xs:xss))
    |(f x y == True) && (f xs y == False)  = x:[y] ++ (xs:xss)
    |otherwise      = x : insert f (xs:xss) y


--Utilitzant la funció insert.
--funció que ordeni la llista per inserció segons la relació donada.
insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] 
insertionSort f y = foldr (\x xs  -> insert f  xs x) [] y

