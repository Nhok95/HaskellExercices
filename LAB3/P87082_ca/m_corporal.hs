main = do
    line <- getLine
    if line == "*"
       then return ()
       else do
           imc line
           main

           
getImc::(Fractional a, Ord a) => a -> a -> [Char]
getImc m h
    | n < 18            = "magror"
    | n >= 18 && n <25  = "corpulencia normal"
    | n >= 25 && n <30  = "sobrepes"
    | n >= 30 && n <40  = "obesitat"
    | otherwise         = "obesitat morbida"
    where n = m/(h^2)

imc:: String -> IO ()
imc l = putStrLn  (name ++ ": " ++ getImc (read m::Double) (read h::Double))
    where (name:m:h:rest) = words l
