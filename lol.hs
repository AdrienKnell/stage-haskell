{-# LANGUAGE FlexibleInstances #-}

type GF = [Integer]

class CombinatClass a where
  gf :: a -> GF
  
data Ast a = Eps
         | Z
         | Union (Ast a) (Ast a)
         | Prod (Ast a) (Ast a)
         | Seq (Ast a)
         | Name a
         | Rec (Ast a) (Ast a -> Ast a)

instance (Show a) => Show (Ast a) where
  show Z = "Z"
  show Eps = "Ɛ"
  show (Union x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Prod x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
  show (Seq x) = "Seq(" ++ (show x) ++ ")"
  show (Name x) = show x
  show (Rec x f) = show x ++ " = " ++ show (f x)

infixr 8 .*.
infixr 7 .+.
infixr 9 .=.

(.+.) :: Ast a -> Ast a -> Ast a
(.+.) = Union

(.*.) :: Ast a -> Ast a -> Ast a
(.*.) = Prod

(.=.) :: a -> (Ast a -> Ast a) -> Ast a
(.=.) x eq = Rec (Name x) eq

binaryTrees :: Ast String
binaryTrees = "B" .=. (\b -> Eps .+. Z .*. b .*. b)

a = Prod Z Z

instance CombinatClass (Ast a) where
  gf Eps = 1 : repeat 0
  gf Z = 0 : 1 : repeat 0
  gf (Union a b) = zipWith (+) (gf a) (gf b)
  gf (Prod a b) = [sum $ zipWith (*) (take n (gf a)) (reverse $ take n (gf b)) | n <- [1..]]
  -- gf (Seq a) = gf (Union Eps (Prod a (Seq a)))
  gf rule@(Rec name phi) = [last . take n . foldr (\n -> \currGf-> gf' currGf $ (phi rule)) (repeat 0) $ [1..n] | n <- [1..]] where
    gf' currGf (Rec name _) = currGf
    gf' _ Eps = 1 : repeat 0
    gf' _ Z = 0 : 1 : repeat 0
    gf' currGf (Union a b) = zipWith (+) (gf' currGf a) (gf' currGf b)
    gf' currGf (Prod a b) = [sum $ zipWith (*) (take n (gf' currGf a)) (reverse $ take n (gf' currGf b)) | n <- [1..]]

class CombinatClassN a where
  gfN :: a -> Int -> GF

instance CombinatClassN (Ast a) where
  gfN Eps n = take (n+1) $ 1 : repeat 0
  gfN Z n = take (n+1) $ 0 : 1 : repeat 0
  gfN (Union a b) n = take (n+1) $ zipWith (+) (gf a) (gf b)
  gfN (Prod a b) n = [sum $ zipWith (*) (take k (gf a)) (reverse $ take k (gf b)) | k <- [1..n+1]]
  gfN rule@(Rec name phi) n = take (n+1) $ foldr (\n -> \currGf-> gf' currGf $ (phi rule)) (repeat 0) [1..n+1] where
    gf' currGf (Rec name _) = currGf
    gf' _ Eps = 1 : repeat 0
    gf' _ Z = 0 : 1 : repeat 0
    gf' currGf (Union a b) = zipWith (+) (gf' currGf a) (gf' currGf b)
    gf' currGf (Prod a b) = [sum $ zipWith (*) (take n (gf' currGf a)) (reverse $ take n (gf' currGf b)) | n <- [1..]]

--------------------------------------------------------------------

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