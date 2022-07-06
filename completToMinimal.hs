import Data.Map

type GF = [Int]

data EquationAst =
  Eps
  | Z
  | Union EquationAst EquationAst
  | Prod EquationAst EquationAst
  | Primitive EquationAst
  | Derive EquationAst
  | Rule String
  | Seq EquationAst
  | Power EquationAst Int
  | PowerSet EquationAst
  | Set EquationAst
  | Cycle EquationAst
  deriving (Ord, Eq, Show)

type Specification = Map EquationAst EquationAst

data MinimalAst = 
  EpsM
  | ZM
  | UnionM MinimalAst MinimalAst
  | ProdM MinimalAst MinimalAst
  | PrimitiveM MinimalAst
  | DeriveM MinimalAst
  | RuleM EquationAst
  deriving (Ord, Eq, Show)
 
type MinSpec = Map EquationAst MinimalAst
type MinSpecGF = Map MinimalAst [Int]


-- instance (Show a) => Show (Spec a) where
--   show Z = "Z"
--   show Eps = "Ɛ"
--   show (Union x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
--   show (Prod x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
--   show (Seq x) = "Seq(" ++ (show x) ++ ")"
--   show (Derive x) = "Derive(" ++ (show x) ++ ")"  
--   show (Name x) = show x
--   show (Rec x f) = show x ++ " = " ++ show (f x)

-- class CombinatClassEGFN a where
--   gfEGFN :: a -> MinSpecGF -> Int -> GF

-- instance CombinatClassEGFN MinSpec where
--   gfEGFN dico dicoGF n = 
--                 |size dico == 1 = (map (gfEGFN' n dicoGF) dico)
--                 |otherwise = --(IDEM ?) or only the axiom

-- gfEGFN' :: MinSpecGF -> MinimalAst -> GF
-- gfEGFN' dicoGF EpsM = take (n+1) $ 1 : repeat 0
-- gfEGFN' dicoGF ZM = take (n+1) $ 0 : 1 : repeat 0
-- gfEGFN' dicoGF (UnionM a b) = take (n+1) $ zipWith (+) (gfEGFN' a (n+1)) (gfEGFN' b (n+1))
-- gfEGFN' dicoGF (ProdM a b) = [sum $ zipWith (*) (take n' (gfEGFN' a (n+1))) (zipWith (*) (coefBinomialArray (n'-1)) (reverse $ take n' (gfEGFN' b (n+1)))) | n' <- [1..n+1]]
-- gfEGFN' dicoGF (DeriveM a) = tail $ gfEGFN' a (n+1)
-- gfEGFN' dicoGF (RuleM a) = 

evalEq :: MinSpecGF -> Int -> MinimalAst -> GF
evalEq dicoGF n EpsM = Prelude.take (n+1) $ 1 : repeat 0
evalEq dicoGF n ZM = Prelude.take (n+1) $ 0 : 1 : repeat 0
evalEq dicoGF n (UnionM a b) = Prelude.take (n+1) $ zipWith (+) (evalEq a (n+1)) (evalEq b (n+1))
evalEq dicoGF n (ProdM a b) = [sum $ zipWith (*) (Prelude.take n' (evalEq a (n+1))) (zipWith (*) (coefBinomialArray (n'-1)) (reverse $ Prelude.take n' (evalEq b (n+1)))) | n' <- [1..n+1]]
evalEq dicoGF n (DeriveM a) = tail $ evalEq a (n+1)
evalEq dicoGF n (RuleM a) = Data.Map.lookup (RuleM a) dicoGF


iterJoyal :: MinSpec -> MinSpecGF -> Int -> MinSpecGF
iterJoyal spec oldSpecGF n = Data.Map.map (evalEq oldSpecGF n) spec 

gfEGFN :: MinSpec -> MinSpecGF -> Int -> MinSpecGF 
gfEGFN dico dicoGF n = Prelude.foldr (\n accu -> iterJoyal dico accu n) dicoGF [1..n]

----- TOOLS ------

coefBinomial :: Int -> Int -> Int
coefBinomial k n = div (product [(n-k+1)..n]) (factorial k)

coefBinomialArray :: Int -> [Int]
coefBinomialArray n = [coefBinomial k n | k<-[0..n]]

factorial :: Int -> Int
factorial 0 = 1
factorial x = (factorial (x-1)) * x


