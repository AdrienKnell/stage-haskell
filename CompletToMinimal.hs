module CompletToMinimal
(
  EquationAst (..),
  MinimalAst (..),
  toStringAST,
  (.+.),
  (.*.),
  Specification,
  MinSpec,
  MinSpecGF,
  createOriginalDicoGF,
  specToMinSpec,
  addEqToMinSpec,
  computeMinEq,
  listZeros,
  GF
)where

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

-- minSpecToStr = concat . M.mapWithKey (\k v-> show k ++ " = " ++ show v)
-- instance Show MinSpec where
--   show = minSpecToStr


------ Convert complet to Minimal -------

listZeros :: [Integer]
listZeros = 0 : listZeros

createOriginalDicoGF :: MinSpec -> MinSpecGF
createOriginalDicoGF spec = M.map (\x -> listZeros) spec

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
        eqM = PrimitiveM $ ProdM (DeriveM eq') (RuleM (Set eq))
computeMinEq (Cycle eq) minSpec = (RuleM (Cycle eq), minSpecM) -- CYCLE
  where (eq', minSpec') = computeMinEq eq minSpec
        minSpecM = M.insert (RuleM (Seq eq)) eqMseq $ M.insert (RuleM (Cycle eq)) eqM minSpec'
        eqM = PrimitiveM (ProdM (DeriveM eq') (RuleM (Seq eq)))
        eqMseq = UnionM EpsM (ProdM eq' (RuleM (Seq eq)))