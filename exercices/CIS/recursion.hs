skips :: [a] -> [[a]]
skips xs = [skips' xs n (n+1)| n <- [0..(length xs)]]

skips' :: [a] -> Int -> Int -> [a]
skips' xs n add
        |n >= length xs = []
        |otherwise = (xs !! n) : (skips' xs (n+add) add)

localMaxima :: [Int] -> [Int] 
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (x1:x2:x3:xs)
                |x2 > x1 && x2 > x3 = [x2] ++ (localMaxima (x3:xs))
                |otherwise = localMaxima (x2:x3:xs)


