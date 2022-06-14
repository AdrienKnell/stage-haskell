xor :: [Bool] -> Bool
xor xs = foldr (xor') False xs

xor' :: Bool -> Bool -> Bool
xor' x acc
    |x /= acc = True
    |otherwise = False

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldl (\acc x -> acc ++ [(f x)]) [] xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs) 

sieveSundaram :: Integer -> [Integer]
sieveSundaram x = map (times2plus1) (myFoldl (\acc y -> acc ++ (func y)) [] [1..x])

times2plus1 :: Integer -> Integer
times2plus1 x = 2*x + 1  

func :: Integer -> [Integer]
func n = if (length [(i, j)| (i,j) <- (cartProd [1..(n-1)] [1..(n-1)]), i <= j, i+j+2*i*j == n]) /= 0 then [] else [n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]