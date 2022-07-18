module CalculGF
(
    evalEq,
    iterJoyal,
    gfEGFN,
    prod,
    coefBinomial,
    coefBinomialArray,
    factorial
)where

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import GHC.Integer (divInteger)

import CompletToMinimal

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