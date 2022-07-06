{-# LANGUAGE FlexibleInstances #-}

data Stream a = Cons a (Stream a) -- new structure to represent infinite lists.

streamToList :: Stream a -> [a]
streamToList (Cons c s) = c : streamToList s

instance Show a => Show (Stream a) where
    show c = show $ take 50 $ streamToList c -- To show the 50 first elements of the infinite list.

z :: Stream Integer
z = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger x = Cons x (streamRepeat 0)
    (Cons v1 s1) + (Cons v2 s2) = Cons (v1+v2) (s1 + s2)
    negate (Cons v s) = Cons (negate v) (negate s)
    (Cons v1 s1) * (Cons v2 s2) = Cons (v1*v2) ((streamMap (*v1) s2) + (s1 * (Cons v2 s2)))

instance Fractional (Stream Integer) where
  (Cons x xs) / (Cons y ys) = q
    where q = Cons (x `div` y) (streamMap (`div` y) (xs - q * ys))

fibo :: Stream Integer
fibo = z / (1 - z - z*z)

gfBinaryWords :: Stream Integer
gfBinaryWords = 1 / (1 - 2*z)

--TOOLS --

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons c v) = Cons (f c) (streamMap f v)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f init = Cons init (streamFromSeed f (f init)) 