type GF = [Int]
  
data Ast a = Eps
         | Z
         | Scalar Int
         | Union (Ast a) (Ast a)
         | Prod (Ast a) (Ast a)
         | Seq (Ast a)
         | Power (Ast a) Int
         | PowerSet (Ast a)
         | Derive (Ast a)
         | Set (Ast a)
         | Cycle (Ast a)
         | Name a
         | Rec (Ast a) (Ast a -> Ast a)

instance (Show a) => Show (Ast a) where
  show Z = "Z"
  show Eps = "Ɛ"
  show (Scalar i) = show i
  show (Power x i) = show x ++ "^" ++ show i
  show (Union x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Prod x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
  show (Seq x) = "Seq(" ++ (show x) ++ ")"
  show (Derive x) = "Derive(" ++ (show x) ++ ")"  
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

binaryWords :: Ast String
binaryWords = "BW" .=. (\b -> Seq (Z .+. Z))

binaryWordsStartingWithABA :: Ast String
binaryWordsStartingWithABA = (Power Z 3) .*. (Seq (Z .+. Z))

multiplyZtimesZ :: Ast String
multiplyZtimesZ = Z .*. Z .*. Z .*. Z .*. Z .*. Z

class CombinatClassN a where
  gfN :: a -> Int -> GF

instance CombinatClassN (Ast a) where
  gfN Eps n = take (n+1) $ 1 : repeat 0
  gfN Z n = take (n+1) $ 0 : 1 : repeat 0
  gfN (Union a b) n = take (n+1) $ zipWith (+) (gfN a (n+1)) (gfN b (n+1))
  gfN (Prod a b) n = [sum $ zipWith (*) (take k (gfN a (n+1))) (reverse $ take k (gfN b (n+1))) | k <- [1..n+1]]
  gfN (Scalar a) n = take (n+1) $ a : repeat 0
  gfN (Power a 1) n = take (n+1) $ gfN a n
  gfN (Power a i) n = take (n+1) $ gfN (Prod a (Power a (i-1))) (n+1)
  gfN (Seq a) n = take (n+1) $ gfN (Rec Z (\b -> Seq a)) (n+1)
  gfN (Derive a) n = tail $ take (n+2) $ gfN a (n+1)
  gfN rule@(Rec name phi) n = take (n+1) $ foldr (\n -> \currGf-> gf' currGf $ (phi rule)) (repeat 0) [1..n+1] where
    gf' currGf (Rec name _) = currGf
    gf' _ Eps = 1 : repeat 0
    gf' _ Z = 0 : 1 : repeat 0
    gf' currGf (Union a b) = zipWith (+) (gf' currGf a) (gf' currGf b)
    gf' currGf (Prod a b) = [sum $ zipWith (*) (take n (gf' currGf a)) (reverse $ take n (gf' currGf b)) | n <- [1..]]
    gf' currGf (Seq a) = gf' currGf (Union Eps (Prod a (Rec Z (\a -> a)))) -- (Rec Z (\a -> a)) because we don't care 
  
class CombinatClassEGFN a where
  gfEGFN :: a -> Int -> GF

instance CombinatClassEGFN (Ast a) where
  gfEGFN Eps n = take (n+1) $ 1 : repeat 0
  gfEGFN Z n = take (n+1) $ 0 : 1 : repeat 0
  gfEGFN (Union a b) n = take (n+1) $ zipWith (+) (gfEGFN a (n+1)) (gfEGFN b (n+1))
  gfEGFN (Prod a b) n = [sum $ zipWith (*) (take n' (gfEGFN a (n+1))) (zipWith (*) (coefBinomialArray (n'-1)) (reverse $ take n' (gfEGFN b (n+1)))) | n' <- [1..n+1]]
  gfEGFN (Scalar a) n = take (n+1) $ a : repeat 0
  gfEGFN (Power a 1) n = take (n+1) $ gfEGFN a n
  gfEGFN (Power a i) n = take (n+1) $ gfEGFN (Prod a (Power a (i-1))) n 
  gfEGFN (Derive a) n = tail $ gfEGFN a (n+1)
  gfEGFN (Seq a) n = gfEGFN (Rec Z (\b -> Seq a)) n
  gfEGFN (Set a) n = gfEGFN (Rec Z (\b -> Set a)) n
  gfEGFN (Cycle a) n = (take (n+1) $ gfEGFN (Prod (Derive a) (Seq a)) n)
  gfEGFN rule@(Rec name phi) n = take (n+1) $ foldr (\x -> \currGf-> gf' currGf (phi rule)) (repeat 0) [1..n+1] where
    gf' currGf (Rec name _) = currGf
    gf' _ Eps = 1 : repeat 0
    gf' _ Z = 0 : 1 : repeat 0
    gf' currGf (Derive a) = tail $ gf' currGf a 
    gf' currGf (Union a b) = zipWith (+) (gf' currGf a) (gf' currGf b)
    gf' currGf (Prod a b) = [sum $ zipWith (*) (take n' (gf' currGf a)) (zipWith (*) (coefBinomialArray (n'-1)) (reverse $ take n' (gf' currGf b))) | n' <- [1..]]
    gf' currGf (Seq a) = gf' currGf (Union Eps (Prod a (Rec Z (\a -> a)))) -- (Rec Z (\a -> a)) because we don't care 
    gf' currGf (Set a) = gf' currGf (Prod (Derive a) (Rec Z (\a -> a)))

coefBinomial :: Int -> Int -> Int
coefBinomial k n = div (product [(n-k+1)..n]) (factorial k)

coefBinomialArray :: Int -> [Int]
coefBinomialArray n = [coefBinomial k n | k<-[0..n]]

factorial :: Int -> Int
factorial 0 = 1
factorial x = (factorial (x-1)) * x

applyCbinomial :: [Int] -> Int -> Int -> [Int]
applyCbinomial [] _ _ =  []
applyCbinomial (x:xs) index len = x * (coefBinomial index len) : (applyCbinomial xs (index+1) len)  



main :: IO ()
main = 
  do
    putStr("---------------------------------------------------\n")
    putStr("|                       START                     |\n")
    putStr("---------------------------------------------------\n")
    print "UNLABELED binary Trees: " 
    print $ gfN binaryTrees 10
    putStr("\n")
    print "UNLABELED binary Words : "
    print $ gfN binaryWords 10 
    putStr("\n")
    print "UNLABELED binary Words not starting by ABA : "
    print $ gfN binaryWordsStartingWithABA 10
    putStr("\n---------------------------------------------------- \n")
    print "LABELED binary Trees: " 
    print $ gfEGFN binaryTrees 10
    putStr("\n")
    print "LABELED binary Words : "
    print $ gfEGFN binaryWords 10 
    putStr("\n")
    print "LABELED binary Words not starting by ABA : "
    print $ gfEGFN binaryWordsStartingWithABA 10


-- class CombinatClass a where
--   gf :: a -> GF

-- instance CombinatClass (Ast a) where
--   gf Eps = 1 : repeat 0
--   gf Z = 0 : 1 : repeat 0
--   gf (Union a b) = zipWith (+) (gf a) (gf b)
--   gf (Scalar a) = a : repeat 0
--   gf (Power a 1) = gf a
--   gf (Power a i) = gf $ Prod a (Power a (i-1)) 
--   gf (Prod a b) = [sum $ zipWith (*) (take n (gf a)) (reverse $ take n (gf b)) | n <- [1..]]
--   gf (Seq a) = gf $ Rec Z (\b -> Seq a)
--   gf rule@(Rec name phi) = [last . take n . foldr (\n -> \currGf-> gf' currGf (phi rule)) (repeat 0) $ [1..n] | n <- [1..]] where
--     gf' currGf (Rec name _) = currGf
--     gf' _ Eps = 1 : repeat 0
--     gf' _ Z = 0 : 1 : repeat 0
--     gf' _ (Scalar a) = a : repeat 0
--     gf' currGf (Power a 1) = gf' currGf a
--     gf' currGf (Power a i) = gf' currGf $ Prod a (Power a (i-1)) 
--     gf' currGf (Union a b) = zipWith (+) (gf' currGf a) (gf' currGf b)
--     gf' currGf (Prod a b) = [sum $ zipWith (*) (take n (gf' currGf a)) (reverse $ take n (gf' currGf b)) | n <- [1..]]
--     gf' currGf (Seq a) = gf' currGf (Union Eps (Prod a (Rec Z (\a -> a)))) -- (Rec Z (\a -> a)) because we don't care 