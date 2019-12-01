main = do
    name <- getLine
    let n = reverse name
    putStrLn ("Hola " ++ getMacu n)
    
    
getMacu:: String -> String
getMacu (x:xs) = if x == 'a' || x == 'A' then "maca!"
                                         else "maco!"