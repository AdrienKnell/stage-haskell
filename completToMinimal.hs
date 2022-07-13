import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import GHC.Integer (divInteger)

----------------

-- TO DO
--      - Show OK
--      - .+. au lieu de Union OK
--      - Restructurer le code, faire moins de foldr et pattern matching
--      - Faire des tests OK
--      - Nettoyer le code OK
--      - Corriger le bug de dicoComplet3

----------------

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
  deriving (Ord, Eq)

data MinimalAst = 
  EpsM
  | ZM
  | UnionM MinimalAst MinimalAst
  | ProdM MinimalAst MinimalAst
  | PrimitiveM MinimalAst
  | DeriveM MinimalAst
  | RuleM EquationAst
  deriving (Ord, Eq)

instance Show EquationAst where
  show Z = "Z"
  show Eps = "Ɛ"
  show (Union x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Prod x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
  show (Seq x) = "Seq(" ++ (show x) ++ ")"
  show (Set x) = "Set(" ++ (show x) ++ ")"
  show (Cycle x) = "Cycle(" ++ (show x) ++ ")"
  show (Derive x) = "Derive(" ++ (show x) ++ ")"  
  show (Primitive x) = "Primitive(" ++ (show x) ++ ")" 
  show (Rule s) = "Rule " ++ s

instance Show MinimalAst where
  show ZM = "Z"
  show EpsM = "Ɛ"
  show (UnionM x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (ProdM x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
  show (DeriveM x) = "DeriveM(" ++ (show x) ++ ")"  
  show (PrimitiveM x) = "PrimitiveM(" ++ (show x) ++ ")"
  show (RuleM s) = "RuleM " ++ (show s) 

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

(.+.) :: EquationAst -> EquationAst -> EquationAst
(.+.) = Union

(.*.) :: EquationAst -> EquationAst -> EquationAst
(.*.) = Prod

type Specification = M.Map EquationAst EquationAst -- M.Map (Rule "") EquationAst
type MinSpec = M.Map EquationAst MinimalAst
type MinSpecGF = M.Map EquationAst [Integer]

------ Convert complet to Minimal -------

createOriginalDicoGF :: MinSpec -> MinSpecGF
createOriginalDicoGF spec = M.map (\x -> listZeros) spec

removeAllCycle :: Specification -> Specification
removeAllCycle spec = M.map (removeCycle) spec

removeCycle :: EquationAst -> EquationAst
removeCycle Eps = Eps
removeCycle Z = Z
removeCycle (Union a b) = Union (removeCycle a) (removeCycle b)
removeCycle (Prod a b) = Prod (removeCycle a) (removeCycle b)
removeCycle (Rule a) = Rule a
removeCycle (Primitive a) = Primitive (removeCycle a)
removeCycle (Derive a) = Derive (removeCycle a)
removeCycle (Seq a) = Seq (removeCycle a)
removeCycle (Set a) = Set (removeCycle a)
removeCycle (Cycle a) = Primitive (Prod (Derive a) (Seq a))

completToMin :: Specification -> MinSpec
completToMin spec = M.map (completToMinAUX) spec

completToMinAUX :: EquationAst -> MinimalAst
completToMinAUX Eps = EpsM
completToMinAUX Z = ZM
completToMinAUX (Union a b) = (UnionM (completToMinAUX a) (completToMinAUX b))
completToMinAUX (Prod a b) = (ProdM (completToMinAUX a) (completToMinAUX b))
completToMinAUX (Rule a) = (RuleM $ Rule a)
completToMinAUX (Derive a) = DeriveM (completToMinAUX a)
completToMinAUX (Primitive a) = PrimitiveM (completToMinAUX a)
completToMinAUX (Seq a) = (RuleM $ Seq a)
completToMinAUX (Set a) = (RuleM $ Set a)

completToMinSecondStep :: MinSpec -> MinSpec
completToMinSecondStep minSpec = foldr (\x accu -> M.union accu (addEquation accu x)) minSpec minSpec

addEquation :: MinSpec -> MinimalAst -> MinSpec
addEquation minSpec EpsM = M.empty
addEquation minSpec ZM = M.empty
addEquation minSpec (UnionM a b) = M.union (addEquation minSpec a) (addEquation minSpec b)
addEquation minSpec (ProdM a b) = M.union (addEquation minSpec a) (addEquation minSpec b)
addEquation minSpec (PrimitiveM a) = (addEquation minSpec a)
addEquation minSpec (DeriveM a) = (addEquation minSpec a)
addEquation minSpec (RuleM (Rule a)) = M.empty
addEquation minSpec (RuleM (Seq a))
  |M.member (Seq a) minSpec = M.empty
  |otherwise = case M.null specFromA of
                 True -> M.fromList [((Seq a), UnionM EpsM (ProdM (completToMinAUX a) (RuleM (Seq a))))]
                 False -> M.union specFromA $ M.fromList [((Seq a), UnionM EpsM (ProdM (RuleM a) (RuleM (Seq a))))]
                 where specFromA = addEquation minSpec (RuleM a)
addEquation minSpec (RuleM (Set a))
  |M.member (Seq a) minSpec = M.empty
  |otherwise = case M.null specFromA of
                 True -> M.fromList [((Set a), PrimitiveM (ProdM (DeriveM (completToMinAUX a)) (RuleM (Set a))))]
                 False -> M.union specFromA $ M.fromList [((Set a), PrimitiveM (ProdM (DeriveM (RuleM a)) (RuleM (Set a))))]
                 where specFromA = addEquation minSpec (RuleM a)
-- addEquation minSpec (RuleM (Cycle a))
--   |M.member (Seq a) minSpec = M.empty
--   |otherwise = case M.null specFromA of
--                  True -> M.fromList [((Cycle a), UnionM EpsM (ProdM (completToMinAUX a) (RuleM (Seq a))))]
--                  False -> M.union specFromA $ M.fromList [((Cycle a), Primitive (ProdM (DeriveM (RuleM a)) (RuleM (Seq a))))]
addEquation minSpec (RuleM a) = M.empty


------ Calcul of GF ------

listZeros :: [Integer]
listZeros = 0 : listZeros

evalEq :: MinSpecGF -> Int -> MinimalAst -> GF
evalEq dicoGF n EpsM = take (n+1) $ 1 : repeat 0
evalEq dicoGF n ZM = take (n+1) $ 0 : 1 : repeat 0
evalEq dicoGF n (UnionM a b) = take (n+1) $ zipWith (+) (evalEq dicoGF (n+1) a) (evalEq dicoGF (n+1) b)
evalEq dicoGF n (ProdM a b) = [sum $ zipWith (*) (take n' (evalEq dicoGF (n+1) a)) (zipWith (*) (coefBinomialArray (n'-1)) (reverse $ take n' (evalEq dicoGF (n+1) b))) | n' <- [1..n+1]]
evalEq dicoGF n (DeriveM a) = tail $ evalEq dicoGF (n+1) a
evalEq dicoGF n (PrimitiveM a) = 1 : (evalEq dicoGF (n+1) a)
evalEq dicoGF n (RuleM a) = Maybe.fromJust (M.lookup a dicoGF)

iterJoyal :: MinSpec -> MinSpecGF -> Int -> MinSpecGF
iterJoyal spec oldSpecGF n = M.map (evalEq oldSpecGF n) spec 

gfEGFN :: MinSpec -> MinSpecGF -> Int -> MinSpecGF 
gfEGFN dico dicoGF n = foldr (\x accu -> iterJoyal dico accu n) dicoGF [1..(n+1)]

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

------ TESTS ------

-- TESTS on Specification --
dicoComplet0 :: Specification
dicoComplet0 = M.fromList [(Rule "A", Eps .+. Z)]

dicoComplet1 :: Specification -- binary trees
dicoComplet1 = M.fromList([(Rule "A", Eps .+. (Z .*. ((Rule "A") .*. (Rule "A"))))])

dicoComplet2 :: Specification
dicoComplet2 = M.fromList([(Rule "A", Eps .+. (Z .*. (Rule "B"))), (Rule "B", Eps .+. Z)])

dicoComplet3 :: Specification -- Binary words
dicoComplet3 = M.fromList([(Rule "A", Seq (Z .+. Z))])

dicoComplet4 :: Specification
dicoComplet4 = M.fromList([(Rule "A", Seq (Z .+. (Seq (Rule "A"))))])

dicoComplet5 :: Specification
dicoComplet5 = M.fromList [(Rule "A", Eps .+. (Z .*. ((Rule "A") .*. (Seq (Rule "A")))))]

dicoComplet6 :: Specification
dicoComplet6 = M.fromList [(Rule "A", Eps .+. (Z .*. ((Rule "A") .*. (Seq (Seq (Rule "A"))))))]

dicoComplet7 :: Specification
dicoComplet7 =  M.fromList [(Rule "A", Seq ((Rule "A") .+. (Rule "A")))]

dicoComplet8 :: Specification
dicoComplet8 = M.fromList [(Rule "A", Eps .+. (Z .*. ((Rule "A") .*. (Seq ((Rule "A") .+. ((Rule "A") .*. (Rule "A")))))))]

dicoComplet9 :: Specification
dicoComplet9 = M.fromList [(Rule "A", Set (Cycle Z))]

main :: IO ()
main = 
  do
    putStr("---------------------------------------------------\n")
    putStr("|                       START                     |\n")
    putStr("---------------------------------------------------\n")
    print "LABELED binary Trees: " 
    print $ gfFinalTest dicoComplet1
    putStr("\n")
    print "LABELED binary Words : "
    print $ gfFinalTest dicoComplet3
    putStr("\n")
    print "Set (Cycle Z) : "
    print $ gfFinalTest dicoComplet9
    putStr("\n")


-- TESTS on MinSpec --
dicoMin :: MinSpec
dicoMin = M.fromList([(Rule "A",  ProdM ZM (ProdM ZM (ProdM ZM ZM)))])

binaryTrees = M.fromList([(Rule "A", UnionM EpsM (ProdM ZM (ProdM (RuleM $ Rule "A") (RuleM $ Rule "A"))))])

dicoGF :: MinSpecGF
dicoGF = createOriginalDicoGF dicoMin

gfFinalTest :: Specification -> MinSpecGF
gfFinalTest dicoComplet = gfEGFN (completToMinSecondStep (completToMin dicoComplet)) (createOriginalDicoGF (completToMinSecondStep $ completToMin (removeAllCycle dicoComplet))) 10

