-- -XTypeSynonymInstances -XFlexibleInstances -XOverlappingInstances
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import GHC.Integer (divInteger)

import CompletToMinimal
import CalculGF

------ TESTS ------

-- TESTS on Specification --
dicoComplet0 :: Specification -- test with Epsilon
dicoComplet0 = M.fromList [(Rule "A", Eps)]

dicoComplet1 :: Specification -- test for non-recursive Union 
dicoComplet1 = M.fromList [(Rule "A", Eps .+. Z)]

dicoComplet2 :: Specification -- test for non-recursive Product
dicoComplet2 = M.fromList [(Rule "A", Z .*. Z)]

dicoComplet3 :: Specification -- test for simple recursive GF : PLANAR BINARY TREES
dicoComplet3 = M.fromList([(Rule "A", Z .+. (Z .*. ((Rule "A") .*. (Rule "A"))))])

dicoComplet4 :: Specification -- test for several equations A and B
dicoComplet4 = M.fromList([(Rule "A", Eps .+. (Z .*. (Rule "B"))), (Rule "B", Eps .+. Z)])

dicoComplet5 :: Specification -- tests for sequence without recursion: BINARY WORDS
dicoComplet5 = M.fromList([(Rule "A", Seq (Z .+. Z))])

dicoComplet6 :: Specification -- test for sequence with recursion : PLANE LABBELED TREES 
dicoComplet6 = M.fromList([(Rule "A", Z .*. (Seq (Rule "A")))])

dicoComplet7 :: Specification -- test for sequence with recursion and simple recursion mixed up
dicoComplet7 = M.fromList [(Rule "A", Eps .+. (Z .*. ((Rule "A") .*. (Seq (Rule "A")))))]

dicoComplet8 :: Specification -- test for several sequence nested
dicoComplet8 = M.fromList [(Rule "A", Eps .+. (Z .*. ((Rule "A") .*. (Seq (Seq (Rule "A"))))))]

dicoComplet9 :: Specification -- test for double recursion sequence
dicoComplet9 =  M.fromList [(Rule "A", Seq ((Rule "A") .+. (Rule "A")))]

dicoComplet10 :: Specification -- test for set : CAYLEY TREES
dicoComplet10 = M.fromList [(Rule "A", Z .*. (Set $ Rule "A"))]

dicoComplet11 :: Specification -- test for set and cycle nested : 1/(1-Z)
dicoComplet11 = M.fromList [(Rule "A", Set (Cycle Z))]

dicoComplet12 :: Specification -- test for set and seq nested : Permutations
dicoComplet12 = M.fromList [(Rule "A", Set (Seq Z))]

dicoComplet13 :: Specification -- test for set and set nested : Set partitions
dicoComplet13 = M.fromList [(Rule "A", Set (Set Z))]

dicoComplet14 :: Specification -- test for set and set nested : Surjections
dicoComplet14 = M.fromList [(Rule "A", Cycle (Cycle Z))]

testFunction :: [Integer] -> [Integer] -> String 
testFunction l1 l2
            |(take 10 $ l1) == l2 = "---------Test PASSED---------"
            |otherwise = "Test FAILED you should have : " ++ show l2

listInteger :: [Int] -> [Integer]
listInteger [] = []
listInteger (x:xs) = toInteger x : (listInteger xs)

myPrint :: Show a => Maybe a -> IO ()
myPrint (Just x) = print x
myPrint n        = print n

main :: IO ()
main = 
  do
    putStr("---------------------------------------------------\n")
    putStr("|                       START                     |\n")
    putStr("---------------------------------------------------\n")
    print "Eps + Z : " 
    myPrint $ M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet1 15
    print $ testFunction (Maybe.fromJust (M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet1 15)) (listInteger [1,1,0,0,0,0,0,0,0,0])
    putStr("\n")
    print "Z * Z : " 
    myPrint $ M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet2 15
    print $ testFunction (Maybe.fromJust (M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet2 15)) (listInteger [0,0,2,0,0,0,0,0,0,0])
    putStr("\n")
    print "LABELED PLANAR BINARY TREES : " 
    myPrint $ M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet3 15
    print $ testFunction (Maybe.fromJust (M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet3 15)) (listInteger [0,1,0,6,0,240,0,25200,0,5080320])
    putStr("\n")
    print "LABELED BINARY WORDS : "
    myPrint $ M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet5 15
    print $ testFunction (Maybe.fromJust (M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet5 15)) (listInteger [1,2,8,48,384,3840,46080,645120,10321920,185794560])
    putStr("\n")
    print "Cayley Trees : " 
    myPrint $ M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet10 15
    print $ testFunction (Maybe.fromJust (M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet10 15)) (listInteger [0,1,2,9,64,625,7776,117649,2097152,43046721])
    putStr("\n")
    print "Set (Cycle Z) : "
    myPrint $ M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet11 15
    print $ testFunction (Maybe.fromJust (M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet11 15)) (listInteger [1,1,2,6,24,120,720,5040,40320,362880])
    putStr("\n")
    print "Set (Set Z) : "
    myPrint $ M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet13 15
    print $ testFunction (Maybe.fromJust (M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet13 15)) (listInteger [1,1,2,5,15,52,203,877,4140,21147])
    putStr("\n")
    print "Set (Cycle Z), Permutations : "
    myPrint $ M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet12 15
    print $ testFunction (Maybe.fromJust (M.lookup (RuleM $ Rule "A") $ gfCompute dicoComplet12 15)) (listInteger [1,1,3,13,73,501,4051,37633,394353,4596553])
    putStr("\n")

gfCompute :: Specification -> Int -> MinSpecGF
gfCompute dicoComplet n = gfEGFN (specToMinSpec $ dicoComplet) (createOriginalDicoGF (specToMinSpec dicoComplet)) n

gfN :: Specification -> Int -> [Integer]
gfN spec n = Maybe.fromJust $ M.lookup (RuleM $ Rule "A") $ gfCompute spec n

(.=.) :: EquationAst -> EquationAst -> Specification
(.=.) a b = M.fromList [(a, b)]