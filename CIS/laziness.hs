{-# LANGUAGE FlexibleInstances #-}

(&&!) :: Bool -> Bool -> Bool
True  &&! True  = True
True  &&! False = False
False &&! True  = False
False &&! False = False


fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibInfinite :: [Integer]
fibInfinite = [fib n | n <- [0..]]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

data Stream a = Cons a (Stream a) 

streamToList :: Stream a -> [a]
streamToList (Cons c s) = c : streamToList s

instance Show a => Show (Stream a) where
    show c = show $ take 20 $ streamToList c

infinite1 :: Stream Integer
infinite1 = Cons 1 (infinite1)

--TOOLS --

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons c v) = Cons (f c) (streamMap f v)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f init = Cons init (streamFromSeed f (f init)) 

----------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

--------OPTIONAL FIBONACCI ----------

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger x = Cons x (streamRepeat 0)
    (Cons v1 s1) + (Cons v2 s2) = Cons (v1+v2) (s1 + s2)
    negate (Cons v s) = Cons (negate v) (negate s)
    (Cons v1 s1) * (Cons v2 s2) = Cons (v1*v2) ((streamMap (*v1) s2) + (s1 * (Cons v2 s2)))

instance Fractional (Stream Integer) where
  (Cons x xs) / (Cons y ys) = q
    where q = Cons (x `div` y) (streamMap (`div` y) (xs - q * ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)

gfBinaryWords :: Stream Integer
gfBinaryWords = 1 / (1 - 2*x)

gfBinaryWordsWithoutAAA :: Stream Integer
gfBinaryWordsWithoutAAA = (1 + x + x*x) / (1 - x - x*x - x*x*x)
