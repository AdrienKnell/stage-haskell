-- data Btree a = Leaf a | Node a (Btree a) (Btree a) deriving (Show)
-- testTree = Node 24 (Node 5 (Leaf 0) (Leaf 0)) (Node 1 (Leaf 0) (Leaf 5))

-- data Binary_words = Empty | ZeroOrOne Char Binary_words
-- testBinary_word = ZeroOrOne '0' (ZeroOrOne '0' (ZeroOrOne '1' Empty))

-- data Binary_words a = [a] | [a] : (Binary_words a) 

type GeneratingFunction = [Integer]

class CombinatorialClass a where
    gf :: a -> GeneratingFunction

data Struct a b = Eps
                | Z a
                | Union (Struct a b) (Struct a b)
                | Prod (Struct a b) (Struct a b)
                | Seq (Struct a b)

instance CombinatorialClass (Struct a b) where
  gf Eps = 1 : repeat 0
  gf (Z _) = [0, 1] ++ repeat 0

  gf (Union Eps a) = plusEps (gf a)
  gf (Union a Eps) = plusEps (gf a)
  gf (Union (Z _) a) = plusZ (gf a)
  gf (Union a (Z _)) = plusZ (gf a)
  gf (Union a b) = union (gf a) (gf b)

  gf (Prod Eps a) = (gf a)
  gf (Prod a Eps) = (gf a)
  gf (Prod (Z _) a) = [0] ++ (gf a)
  gf (Prod a (Z _)) = [0] ++ (gf a)
  gf (Prod a b) = [0] ++ prod' (gf a) (gf b) -- We put a 0 because ...

--   gf (Seq a) = gf (Union Eps (Prod a (Seq a)))
  gf (Seq a) = plusEps (prod (gf a) (gf (Seq a)))

prod' :: GeneratingFunction -> GeneratingFunction -> GeneratingFunction
prod' xs ys = prod xs ys

plusEps :: GeneratingFunction -> GeneratingFunction
plusEps (x:xs)= (x+1) : xs

plusZ :: GeneratingFunction -> GeneratingFunction
plusZ (x1:x2:xs)= x1 : (x2+1) : xs

testEps = Eps
testZ = Z 0
binWord = Seq (Union (Z 0) (Z 1))
binTreeLeafs = Union (Z 0) (Prod binTreeLeafs binTreeLeafs)
binTreeInternNodes = Union Eps (Prod (Z 0) (Prod binTreeInternNodes binTreeInternNodes))
randomStructure = Union (Seq randomStructure) (Z 0)


union :: GeneratingFunction -> GeneratingFunction -> GeneratingFunction
union [] xs = xs
union xs [] = xs
union (x:xs) (y:ys) = (x+y) : union xs ys

prod_n :: Int -> GeneratingFunction -> GeneratingFunction -> Integer
prod_n n xs ys = sum $ zipWith (*) (take n xs) (reverse (take n ys)) --calcul of A_n (cardinal of words with length=n in A)

prod :: GeneratingFunction -> GeneratingFunction -> GeneratingFunction
prod xs ys = [prod_n n xs ys | n <- [1..]] -- Returns [A_0, A_1, A_2, ...]

-- instance CombinatorialClass (Btree a) where
--     generating_function _ = catalans

-- instance CombinatorialClass Binary_words where
--     generating_function _ = binaryWordsBis

-- instance Show Binary_words where
--     show = showBinaryWords 
    
-- showBinaryWords :: Binary_words -> String
-- showBinaryWords Empty = ""
-- showBinaryWords (ZeroOrOne x xs) = x : (showBinaryWords xs) 

-- fromStringToBinary_word :: String -> Binary_words
-- fromStringToBinary_word "" = Empty
-- fromStringToBinary_word (x:xs) = ZeroOrOne x (fromStringToBinary_word xs)

-- zeros :: GeneratingFunction
-- zeros = 0 : zeros

-- epsilon, z :: GeneratingFunction
-- epsilon = 1 : zeros
-- z = 0 : 1 : zeros


-- seq_calc :: GeneratingFunction -> GeneratingFunction -- correct
-- seq_calc xs = [1] ++ prod xs (seq_calc xs)

-- catalans :: GeneratingFunction -- correct
-- catalans = [0, 1] ++ tail (prod catalans catalans)

-- catalans2 :: GeneratingFunction -- bug 
-- catalans2 = [0, 1] ++ tail (prod catalans2 catalans2)

-- -- binaryWords :: GeneratingFunction -- correct
-- -- binaryWords = [1] ++ (map (2*) binaryWords)

-- binaryWordsBis:: GeneratingFunction -- correct
-- binaryWordsBis = seq_calc ([2])

-- binaryWords_withoutaaa :: GeneratingFunction
-- binaryWords_withoutaaa = prod ([1, 1, 1] ++ zeros) (seq_calc ([1, 1, 1] ++ zeros))

-- unary_binary_trees :: GeneratingFunction
-- unary_binary_trees = [1] ++ union unary_binary_trees (prod unary_binary_trees unary_binary_trees)

-- general_trees :: GeneratingFunction --exactly the same as catalans
-- general_trees = [0, 1] ++ tail( tail (prod general_trees general_trees))
