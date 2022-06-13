type GF = [Integer]

class CombinatClass a where
  gf :: a -> GF
  
data Ast a = Eps
         | Z
         | Union (Ast a) (Ast a)
         | Prod (Ast a) (Ast a)
         | Name a
         | Rec (Ast a) (Ast a -> Ast a)

instance (Show a) => Show (Ast a) where
  show Z = "Z"
  show Eps = "Ɛ"
  show (Union x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Prod x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
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

instance CombinatClass (Ast a) where
  gf Eps = 1 : repeat 0
  gf Z = 0 : 1 : repeat 0
  gf (Union a b) = zipWith (+) (gf a) (gf b)
  gf (Prod a b) = [sum $ zipWith (*) (take n (gf a)) (reverse $ take n (gf b)) | n <- [1..]]
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

