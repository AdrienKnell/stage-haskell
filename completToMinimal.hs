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
type MinSpec = M.Map MinimalAst MinimalAst
type MinSpecGF = M.Map MinimalAst [Integer]

instance Show MinSpec where
  show = M.mapWithKey (\k v-> show k ++ " = " + show v)
  


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

specToMinSpec :: Specification -> MinSpec
specToMinSpec = M.foldrWithKey addEqToMinSpec M.empty

addEqToMinSpec :: EquationAst -> EquationAst -> MinSpec -> MinSpec
addEqToMinSpec (Rule a) eq minSpec = M.insert (RuleM $ Rule a) eqM minSpecM
  where (eqM, minSpecM) = computeMinEq eq minSpec
addEqToMinSpec _ _ _ = undefined

computeMinEq :: EquationAst -> MinSpec -> (MinimalAst, MinSpec)
computeMinEq Z minSpec = (ZM, minSpec)
computeMinEq Eps minSpec = (EpsM, minSpec)
computeMinEq (Derive a) minSpec = (DeriveM aM, minSpecA)
  where (aM, minSpecA) = computeMinEq a minSpec
computeMinEq (Primitive a) minSpec = (PrimitiveM aM, minSpecA)
  where (aM, minSpecA) = computeMinEq a minSpec
computeMinEq (Rule a) minSpec = (RuleM $ Rule a, minSpec)
computeMinEq (Union a b) minSpec = (UnionM aM bM, minSpecM)
  where (aM, minSpecA) = computeMinEq a minSpec
        (bM, minSPecB) = computeMinEq b minSpec
        minSpecM = M.union minSpecA minSPecB
computeMinEq (Prod a b) minSpec = (ProdM aM bM, minSpecM)
  where (aM, minSpecA) = computeMinEq a minSpec
        (bM, minSPecB) = computeMinEq b minSpec
        minSpecM = M.union minSpecA minSPecB
computeMinEq (Seq eq) minSpec = (RuleM (Seq eq), minSpecM) -- SEQ
  where (eq', minSpec') = computeMinEq eq minSpec
        minSpecM = M.insert (RuleM (Seq eq)) eqM minSpec'
        eqM = UnionM EpsM (ProdM eq' (RuleM (Seq eq)))
computeMinEq (Set eq) minSpec = (RuleM (Set eq), minSpecM) -- SET
  where (eq', minSpec') = computeMinEq eq minSpec
        minSpecM = M.insert (RuleM (Set eq)) eqM minSpec'
        eqM = ProdM (PrimitiveM eq') (RuleM (Set eq))  
computeMinEq (Cycle eq) minSpec = (RuleM (Cycle eq), minSpecM) -- CYCLE
  where (eq', minSpec') = computeMinEq eq minSpec
        minSpecM = M.insert (RuleM (Cycle eq)) eqM minSpec'
        eqM = PrimitiveM (ProdM (DeriveM eq') (RuleM (Seq eq)))
        eqMseq = UnionM EpsM (ProdM eq' (RuleM (Seq eq)))


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
evalEq dicoGF n (RuleM a) = Maybe.fromJust (M.lookup (RuleM a) dicoGF)

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
dicoMin = M.fromList([(RuleM $ Rule "A",  ProdM ZM (ProdM ZM (ProdM ZM ZM)))])

binaryTrees = M.fromList([(RuleM $ Rule "A", UnionM EpsM (ProdM ZM (ProdM (RuleM $ Rule "A") (RuleM $ Rule "A"))))])

dicoGF :: MinSpecGF
dicoGF = createOriginalDicoGF dicoMin

gfFinalTest :: Specification -> MinSpecGF
gfFinalTest dicoComplet = gfEGFN (specToMinSpec $ removeAllCycle dicoComplet) (createOriginalDicoGF (specToMinSpec (removeAllCycle dicoComplet))) 10

