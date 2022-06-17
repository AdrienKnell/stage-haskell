-- module Employee where

-- import           Data.Tree

-- -- Employee names are represented by Strings.
-- type Name = String

-- -- The amount of fun an employee would have at the party, represented
-- -- by an Integer
-- type Fun  = Integer

-- -- An Employee consists of a name and a fun score.
-- data Employee = Emp { empName :: Name, empFun :: Fun }
--   deriving (Show, Read, Eq)

-- -- A small company hierarchy to use for testing purposes.
-- testCompany :: Tree Employee
-- testCompany
--   = Node (Emp "Stan" 9)
--     [ Node (Emp "Bob" 2)
--       [ Node (Emp "Joe" 5)
--         [ Node (Emp "John" 1) []
--         , Node (Emp "Sue" 5) []
--         ]
--       , Node (Emp "Fred" 3) []
--       ]
--     , Node (Emp "Sarah" 17)
--       [ Node (Emp "Sam" 4) []
--       ]
--     ]

-- testCompany2 :: Tree Employee
-- testCompany2
--   = Node (Emp "Stan" 9)
--     [ Node (Emp "Bob" 3) -- (8, 8)
--       [ Node (Emp "Joe" 5) -- (5, 6)
--         [ Node (Emp "John" 1) [] -- (1, 0)
--         , Node (Emp "Sue" 5) [] -- (5, 0)
--         ]
--       , Node (Emp "Fred" 3) [] -- (3, 0)
--       ]
--     , Node (Emp "Sarah" 17) -- (17, 4)
--       [ Node (Emp "Sam" 4) [] -- (4, 0)
--       ]
--     ]

-- treeFold :: (a -> [b] -> b) -> Tree a -> b
-- treeFold f (Node x ys) = f x (fmap (treeFold f) ys)

-- -- A type to store a list of guests and their total fun score.
-- data GuestList = GL [Employee] Fun
--   deriving (Show, Eq)

-- instance Ord GuestList where
--   compare (GL _ f1) (GL _ f2) = compare f1 f2

-- glCons :: Employee -> GuestList -> GuestList
-- glCons employ@(Emp empName empFun) (GL [] fun) = GL [employ] empFun
-- glCons employ@(Emp empName empFun) (GL xs fun) = GL (xs ++ [employ]) (fun + empFun)

-- instance Semigroup GuestList where
--   (GL xs1 fun1) <> (GL xs2 fun2) = GL (xs1 ++ xs2) (fun1 + fun2)

-- instance Monoid GuestList where
--   mempty = GL [] 0

-- moreFun :: GuestList -> GuestList -> GuestList
-- moreFun g1@(GL xs1 fun1) g2@(GL xs2 fun2)
--         | fun1 > fun2 = g1
--         | otherwise = g2

-- -- nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- -- nextLevel boss gls = 

-- maxFun :: Tree Employee -> GuestList
-- maxFun (Node a _) = nextLevel a ()  

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

data Maybe2 a = Just2 a | Nothing2 deriving (Show)

type Name = String

instance Functor Maybe2 where
  fmap f Nothing2 = Nothing2
  fmap f (Just2 v) = Just2 (f v) 

instance Applicative Maybe2 where
  pure              = Just2
  Nothing2 <*> _     = Nothing2
  _ <*> Nothing2     = Nothing2
  Just2 f <*> Just2 x = Just2 (f x)

m_name1, m_name2 :: Maybe2 Name
m_name1 = Nothing2
m_name2 = Just2 "Brent"

m_phone1, m_phone2 :: Maybe2 String
m_phone1 = Nothing2
m_phone2 = Just2 "555-1234"

ex01 = Employee <$> m_name1 <*> m_phone1
ex02 = Employee <$> m_name1 <*> m_phone2
ex03 = Employee <$> m_name2 <*> m_phone1
ex04 = (Employee <$> m_name2) <*> m_phone2 -- important to understand 

