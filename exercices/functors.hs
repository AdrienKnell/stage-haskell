data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap func (Leaf a) = Leaf (func a)
    fmap func (Node a left right) = Node (func a) (fmap func left) (fmap func right)  

instance Applicative Tree where
    pure = Leaf
    (Leaf func) <*> t = fmap func t
    (Node func left right) <*> (Node v leftN rightN) = Node (func v) (left <*> (Node v leftN rightN)) (right <*> (Node v leftN rightN))
    (Node func left right) <*> (Leaf v) = (Leaf (func v))

instance Monad Tree where
    (Leaf v) >>= func = func v
    (Node v left right) >>= func = Node (func v) (left >>= func) (right >>= func)

testTree = Node 24 (Node 5 (Leaf 0) (Leaf 7)) (Node 1 (Leaf 0) (Leaf 5))