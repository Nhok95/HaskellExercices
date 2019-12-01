myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x [] = x
myFoldl f x (y:ys) = myFoldl f (f x y) ys   

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f x [] = x
myFoldr f x (y:ys) = f y (myFoldr f x ys)


myIterate :: (a -> a) -> a -> [a]
myIterate f x = [ xs| xs <- x: myIterate f (f x) ]


myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil f f2 x = if f x then x else myUntil f f2 fx
    where fx = f2 x
    
myMap :: (a -> b) -> [a] -> [b]
myMap f a = [ f x | x <- a]


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f a = [ x | x <- a , f x == True]


{-
myAll :: (a -> Bool) -> [a] -> Bool
myAll f [] = True
myAll f a = myFoldl (&&) True l 
    where l = [ f x | x <- a] 
-}

myAll :: (a -> Bool) -> [a] -> Bool
myAll f a = and (myMap f a)

{-
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = True
myAny f a = myFoldl (||) False l 
    where l = [ f x | x <- a] 
-}

myAny :: (a -> Bool) -> [a] -> Bool
myAny f a = or (myMap f a)

myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)
myZip _ _ = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = [ f x y | (x, y) <- myZip a b]
