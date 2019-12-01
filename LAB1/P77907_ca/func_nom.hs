--valor absolut
absValue:: Int->Int
absValue n
    |n >= 0   = n
    |otherwise = n*(-1)
---------------

--exponencial
power:: Int->Int->Int
power x p
    | p == 0   = 1
    |otherwise = x*(power x (p-1))

{-
power x 0 = 1
power x p = x * power x (p - 1)
-}
---------------    
    
    
auxPrime:: Int ->Int ->Int
auxPrime n d
    |d == 0  = 0
    |otherwise = let x = mod n d
                 in if x == 0 then  (auxPrime n (d-1)) + 1
                              else (auxPrime n (d-1))
    

--n es primer
isPrime:: Int->Bool
isPrime n = (auxPrime n n) == 2
---------------    

{-
-- primes lazy

primes = l(primes [2..])

--lprimes 0 _ = []
lprimes (x:xs) = x:(lprimes ( filter (\y -> (mod y x)/= 0 xs ) )

-}

--slow fibbonacci
slowFib::Int->Int
slowFib n
    |n == 0  = 0
    |n == 1  = 1
    |otherwise = slowFib (n-1) + slowFib (n-2)

--------------- 

fib2 :: Int -> (Int, Int)
fib2 0 = (0, 0)
fib2 1 = (1, 0)
fib2 n = (a + b, a)
    where (a, b) = fib2 (n - 1)

--fast fibbonacci
quickFib:: Int->Int
quickFib n = fst (fib2 n)  -- quickFib = fst.fib2


{-

-- fibbonacci lazy
lfib m n = m : (lfib n (m+n))

ultraquickfib n = (lfib 0 1) !! n

-}
---------------
