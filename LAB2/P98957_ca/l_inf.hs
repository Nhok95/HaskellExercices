--Generar la seqüència dels uns [1,1,1,1,1,1,1,1,…].
{- (opcion 1)
ones :: [Integer] 
ones = 1:(ones)
-}

--opcion2
ones :: [Integer]
ones = repeat 1


--Generar la seqüència dels naturals [0,1,2,3,4,5,6,7…].
nats :: [Integer] 
nats = iterate (1+) 0

posints :: [Integer]
posints = iterate (1+) 1

fromTwo :: [Integer]
fromTwo = iterate (1+) 2
    
--Generar la seqüència dels enters [0,1,−1,2,−2,3,−3,4…].
ifunc :: Integer -> Integer
ifunc n
    |n > 0 = neg n
    |otherwise = (neg n) + 1
    where 
        neg n = n * (-1)

ints :: [Integer] 
ints = iterate ifunc 0

--Generar la seqüència dels nombres triangulars: 0,1,3,6,10,15,21,28,…].
triangulars :: [Integer] 
triangulars = [0] ++ [ div (x*(x+1)) 2 | x <- posints]

--Generar la seqüència dels nombres factorials: [1,1,2,6,24,120,720,5040,…].
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact x = (fact (x-1)) * x

factorials :: [Integer] 
factorials = [  fact x  | x <- nats]
    

--Generar la seqüència dels nombres de Fibonacci: [0,1,1,2,3,5,8,13,…].
fib2 :: Integer -> (Integer,Integer)
fib2 0 = (0,0)
fib2 1 = (1,0)
fib2 x =  (a+b, a)
    where (a, b) = fib2 (x-1)


fibs :: [Integer] 
fibs = [ fst (fib2 x) | x <- nats ]


--Generar la seqüència dels nombres primers: [2,3,5,7,11,13,17,19,…].
{-
opcio1
auxPrime :: Integer -> Integer -> Integer
auxPrime n d
    |d == 0    = 0
    |otherwise = let x  = mod n d
                 in if x == 0 then (auxPrime n (d-1)) +1
                              else (auxPrime n (d-1))

esPrimer :: Integer -> Bool
esPrimer x = (auxPrime x x) == 2

primes :: [Integer] 
primes = [ x | x <- nats, (esPrimer x) == True]
-}

primes :: [Integer]
primes = lprimes $ fromTwo
    where lprimes (x:xs) = x:(lprimes $ filter (\y -> (mod y x)/=0) xs)

--Generar la seqüència ordenada dels nombres de Hamming: [1,2,3,4,5,6,8,9,…]. Els nombres de Hamming són aquells que només tenen 2, 3 i 5 com a divisors primers.
{- (opcio1)
esHamming :: Integer -> Bool
esHamming 0 = False
esHamming 1 = True
esHamming h
    | mod h 2 == 0 = True
    | mod h 3 == 0 = True
    | mod h 5 == 0 = True
    | otherwise = False

hammings :: [Integer]
hammings = [ x | x <- nats, (esHamming x) == True]

(opcio2)
merge2 :: [Integer] -> [Integer] -> [Integer]
merge2 p@(x:xs) q@(y:ys) 
    | x < y    = x:merge2 xs q
    | x > y    = y:merge2 p  ys
    |otherwise = x:merge2 xs ys
merge2 [] ys   = ys
merge2 xs []   = xs

merge3 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
merge3 xs ys zs = merge2 xs (merge2 ys zs)

hamming :: [Integer]
hamming = 1: merge3 [2*i | i <- hamming]
                    [3*i | i <- hamming]
                    [5*i | i <- hamming]
-}
hammings :: [Integer]
hammings = 1 : map (2*) hammings `merge` map (3*) hammings `merge` map (5*) hammings
  where merge (x:xs) (y:ys)
          | x < y = x : xs `merge` (y:ys)
          | x > y = y : (x:xs) `merge` ys
          | otherwise = x : xs `merge` ys

--Generar la seqüència mira i digues: [1,11,21,1211,111221,312211,13112221,1113213211,…].
getFirst :: ([Integer],Integer,Integer) -> [Integer]
getFirst (xs,x,y) = xs

getSecond :: ([Integer],Integer,Integer) -> Integer
getSecond (xs,x,y) = x

getThird :: ([Integer],Integer,Integer) -> Integer
getThird (xs,x,y) = y

las :: ([Integer],Integer, Integer) ->Integer -> ([Integer],Integer, Integer)
las (xs,0,0) x = (xs,1,x)
las (xs,n,y) x = if x == y then (xs,n+1,y) else (xs ++ [n] ++ [y],1,x)


lookNsay :: [Integer]
lookNsay = [x | x <- getNsay [1]]

getNsay :: [Integer] -> [Integer]
getNsay x = [(getListNsay x)]  ++ (getNsay  (getXsay x))


getXsay :: [Integer] -> [Integer]
getXsay x = getFirst l ++ [getSecond l] ++ [getThird l]
    where l = foldl las ([],0,0) x

    
getListNsay :: [Integer] -> Integer
getListNsay [x] = x
getListNsay xs = last xs + 10*getListNsay (init xs) 


--Generar la seqüència de les files del triangle de Tartaglia (també anomenat triangle de Pascal): [[1],[1,1],[1,2,1],[1,3,3,1],…]. 
{-
tartaglia :: [[Integer]]
tartaglia :: iterate (\x -> zipWith (+) (0:x) (x++[0]) ) [1]
-}
  
tartaglia :: [[Integer]]
tartaglia = [1] : map (\x -> zipWith (+) (0:x) (x++[0]) ) tartaglia