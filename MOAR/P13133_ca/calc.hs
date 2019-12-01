


--funció que, donat un natural n, retorni la suma de tots els múltiples de 3 o de 5 per sota de n.

{-funció aux que retorna els múltiples de 3 per sota de n-}
{-
(opcio 1)
getMult3 :: Integer -> [Integer]
getMult3 n = [xs | xs <- [3,6..n-1]]

getMult5 :: Integer -> [Integer]
getMult5 n = [ys | ys <- [5,10..n-1]]

merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(y:ys) 
    | x == y    = x: (merge xs ys)
    | x < y     = x: (merge xs b)
    | otherwise = y: (merge a ys)

sumMultiples35 :: Integer -> Integer 
sumMultiples35 n = foldl1 (+) m 
    where m = merge x y
          x = getMult3 n
          y = getMult5 n
          
          -}
          
sumMultiples :: Integer -> Integer -> Integer
sumMultiples n f = div (f * n1 * (n1 + 1)) 2 
    where n1 = div (n - 1) f
      
sumMultiples35 :: Integer -> Integer
sumMultiples35 n = sumMultiples n 3 + sumMultiples n 5 - sumMultiples n 15


--funció que, donat un natural n, retorni l’n-èsim elements de la sèrie de Fibonacci.
fib :: Int -> (Integer,Integer)
fib 0 = (0,0)
fib 1 = (1,0)
fib n = (a+b, a)
    where a = fst $ fib (n-1)
          b = fst $ fib (n-2)

fibonacci :: Int -> Integer
fibonacci n = fst $ fib n

--funció que, donat un natural n, retorni la suma de tots els elements parells inferiors a n de la sèrie de Fibonacci.
getNfibaux :: Int -> [Integer]
getNfibaux 0 = [0]
getNfibaux n = (getNfibaux (n-1)) ++ [f]
    where f = fst $ fib n
          

getEvenNfib :: Integer -> [Integer]
getEvenNfib n = filter (even) $ filter (<n) (getNfibaux m)
    where m = fromIntegral n :: Int
sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = sum $ getEvenNfib n

{-
--funció que, donat un natural n≥1, retorna el factor primer més gran de n.
largestPrimeFactor :: Int -> Int
-}

--funció que, donat un natural n, retorni si n és palindròmic, és a dir, si n es llegeix igual del dret que del revés.
isPalindromic :: Integer -> Bool 
isPalindromic n = if (show n) == p then True
                            else False
    where p = reverse $ show n