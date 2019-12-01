--funció que emuli el map usant llistes per comprensió.
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = [y | y <- f x: myMap f xs ]


--funció que emuli el filter usant llistes per comprensió.
myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter f x = [y | y <-  x, f y ]


--funció que que emuli el zipWith usant llistes per comprensió i zip.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith f xs ys = [ f x y | (x,y) <- zip xs ys ]


--funció que, donades dues llistes d’enters, genera la llista que aparella els elements si l’element de la segona llista divideix al de la primera.
thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify xs ys = [(x,y) | x <- xs, y <- ys, mod x y == 0]

--funció que, donat un natural no nul, genera la llista ordenada amb els seus factors (no necessàriament primers).
factors :: Int -> [Int] 
factors x = [ xs | xs <- [1..x], mod x xs == 0 ]