import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import GHC.Integer (divInteger)

type GF = [Integer]

data EquationAst =
  Eps
  | Z
  | Union EquationAst EquationAst
  | Prod EquationAst EquationAst
  | Primitive EquationAst
  | Derive EquationAst
  | Rule String
  | Seq EquationAst
  | Set EquationAst
  | Cycle EquationAst
  deriving (Ord, Eq, Show)

toStringAST :: EquationAst -> String
toStringAST Eps = "Eps"
toStringAST Z = "Z"
toStringAST (Union a b) = "Union (" ++ (toStringAST a) ++ " " ++ (toStringAST b) ++ ")"
toStringAST (Prod a b) = "Prod " ++ (toStringAST a) ++ " " ++ (toStringAST b) ++ ")"
toStringAST (Primitive a) = "Primitive " ++ (toStringAST a) ++ ")"
toStringAST (Derive a) = "Derive " ++ (toStringAST a) ++ ")"
toStringAST (Rule a) = "Rule " ++ a
toStringAST (Seq a) = "Seq (" ++ (toStringAST a) ++ ")"
toStringAST (Set a) = "Set (" ++ (toStringAST a) ++ ")"
toStringAST (Cycle a) = "Cycle (" ++ (toStringAST a) ++ ")"

type Specification = M.Map EquationAst EquationAst -- M.Map (Rule "") EquationAst

data MinimalAst = 
  EpsM
  | ZM
  | UnionM MinimalAst MinimalAst
  | ProdM MinimalAst MinimalAst
  | PrimitiveM MinimalAst
  | DeriveM MinimalAst
  | RuleM EquationAst
  deriving (Ord, Eq, Show)
 
type MinSpec = M.Map EquationAst MinimalAst
type MinSpecGF = M.Map EquationAst [Integer]

------ Examples ------

dicoComplet :: Specification
dicoComplet = M.fromList([(Rule "A", Union Eps (Prod Z (Prod (Rule "A") (Seq (Rule "A")))))])

dicoMin :: MinSpec
dicoMin = M.fromList([(Rule "A",  ProdM ZM (ProdM ZM (ProdM ZM ZM)))])

binaryTrees = M.fromList([(Rule "A", UnionM EpsM (ProdM ZM (ProdM (RuleM $ Rule "A") (RuleM $ Rule "A"))))])

dicoGF :: MinSpecGF
dicoGF = createOriginalDicoGF dicoMin

gfFinalTest :: MinSpecGF
gfFinalTest = gfEGFN (completToMin dicoComplet) (createOriginalDicoGF $ completToMin dicoComplet) 25

------ Convert complet to Minimal -------

createOriginalDicoGF :: MinSpec -> MinSpecGF
createOriginalDicoGF spec = M.map (\x -> listZeros) spec

completToMin :: Specification -> MinSpec
completToMin spec = M.map (completToMinAUX) spec

completToMinAUX :: EquationAst -> MinimalAst
completToMinAUX Eps = EpsM
completToMinAUX Z = ZM
completToMinAUX (Union a b) = (UnionM (completToMinAUX a) (completToMinAUX b))
completToMinAUX (Prod a b) = (ProdM (completToMinAUX a) (completToMinAUX b))
completToMinAUX (Rule a) = (RuleM $ Rule a)
completToMinAUX (Seq a) = (RuleM $ Rule ("Seq (" ++ (toStringAST a) ++ ")"))

completToMinSecondeStep :: MinSpec 
completToMinSecondeStep minSpec = foldr (\x accu -> M.union accu (addEquation minSpec x)) minSpec minSpec

addEquation :: MinSpec -> MinimalAst -> MinSpec
addEquation minSpec EpsM = M.empty
addEquation minSpec ZM = M.empty
addEquation minSpec (UnionM a b) = M.union (addEquation a) (addEquation b)
addEquation minSpec (ProdM a b) = M.union (addEquation a) (addEquation b)
addEquation minSpec (PrimitiveM a) = (addEquation a)
addEquation minSpec (DeriveM a) = (addEquation a)
addEquation minSpec (RuleM (Seq a))
                      |M.member (Seq a) minSpec = M.empty 
                      |otherwise = M.fromList [(Rule (toStringAST (Seq a)), UnionM EpsM (ProdM a (RuleM (Seq a))))]



------ Calcul of GF ------

listZeros :: [Integer]
listZeros = 0 : listZeros

-- listZerosInteger :: Integer -> [Integer]
-- listZerosInteger 0 = [0]
-- listZerosInteger n = [0] ++ listZerosInteger (n-1)

evalEq :: MinSpecGF -> Int -> MinimalAst -> GF
evalEq dicoGF n EpsM = take (n+1) $ 1 : repeat 0
evalEq dicoGF n ZM = take (n+1) $ 0 : 1 : repeat 0
evalEq dicoGF n (UnionM a b) = take (n+1) $ zipWith (+) (evalEq dicoGF (n+1) a) (evalEq dicoGF (n+1) b)
evalEq dicoGF n (ProdM a b) = [sum $ zipWith (*) (take n' (evalEq dicoGF (n+1) a)) (zipWith (*) (coefBinomialArray (n'-1)) (reverse $ take n' (evalEq dicoGF (n+1) b))) | n' <- [1..n+1]]
evalEq dicoGF n (DeriveM a) = tail $ evalEq dicoGF (n+1) a
evalEq dicoGF n (RuleM a) = Maybe.fromJust (M.lookup a dicoGF)

iterJoyal :: MinSpec -> MinSpecGF -> Int -> MinSpecGF
iterJoyal spec oldSpecGF n = M.map (evalEq oldSpecGF n) spec 

gfEGFN :: MinSpec -> MinSpecGF -> Int -> MinSpecGF 
gfEGFN dico dicoGF n = foldr (\x accu -> iterJoyal dico accu n) dicoGF [1..(n+1)]

----- TOOLS ------

prod :: [Int] -> Integer
prod [] = 1
prod (x:xs) = (toInteger x) * prod xs

coefBinomial :: Int -> Int -> Integer
coefBinomial k n = divInteger (prod [(n-k+1)..n]) (factorial k)

coefBinomialArray :: Int -> [Integer]
coefBinomialArray n = [coefBinomial k n | k<-[0..n]]

factorial :: Int -> Integer
factorial 0 = 1
factorial x = (factorial (x-1)) * (toInteger x)


