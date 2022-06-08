import Control.Applicative
import Data.Monoid

test_List = Value 4 (Value (-2) (Empty))

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor

instance Functor List where
    fmap func (Empty) = Empty
    fmap func (Value v l) = Value (func v) (fmap func l) 

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty b = b
combineLists a Emp ty = a
combineLists (Value v xs) b = (Value v (combineLists xs b))  

-- Make our list a Monoid

instance Semigroup List where
    Empty <> x = x
    x <> y = y

instance Monoid List where
    mempty = Empty
    --mappend xs v = combineLists xs v 

-- Make our list an Applicative

instance Applicative List where
    pure a = Value a Empty 
    Empty <*> _ = Empty
    Value f _ <*> Empty = Empty
    Value f xs <*> Value v ys = Value (f v) (xs <*> ys)

instance Monad List where
    Empty >>= func = Empty
    (Value v xs) >>= func = combineLists (func v) (xs >>= func)

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty
threeValueList = Value 1 $ Value 2 $ Value 3 Empty
fourValueList = Value 6 $ Value 18 $ Value 3 $ Value 8 $ Value (-4) Empty
fiveValueList = Value 17 $ Value 0 $ Value (-9) $ Value 4 $ Value (-9) Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)
multTwo = (*2)
divTwo = (/2)

-- Use <$> and <*> on the lists with a binary function

a = divTwo <$> twoValueList
b = (Value plusTwo $ Value multTwo Empty) <*> twoValueList

-- Create some lists of binary functions

-- Use <*> on the binary functions list and the number lists