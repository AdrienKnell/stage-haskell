import completToMinimal.hs

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

main :: IO ()
main = 
  do
    putStr("---------------------------------------------------\n")
    putStr("|                       START                     |\n")
    putStr("---------------------------------------------------\n")
    print "Eps + Z : " 
    print $ gfFinalTest dicoComplet1
    putStr("\n")
    print "Z * Z : " 
    print $ gfFinalTest dicoComplet2
    putStr("\n")
    print "LABELED PLANAR BINARY TREES : " 
    print $ gfFinalTest dicoComplet3
    putStr("\n")
    print "LABELED BINARY WORDS : "
    print $ gfFinalTest dicoComplet5
    putStr("\n")
    print "Cayley Trees : " 
    print $ gfFinalTest dicoComplet10
    putStr("\n")
    print "Set (Cycle Z) : "
    print $ gfFinalTest dicoComplet11
    putStr("\n")
    print "Set (Set Z) : "
    print $ gfFinalTest dicoComplet13
    putStr("\n")
    print "Set (Cycle Z), Permutations : "
    print $ gfFinalTest dicoComplet12
    putStr("\n")


-- TESTS on MinSpec --
dicoMin :: MinSpec
dicoMin = M.fromList([(RuleM $ Rule "A",  ProdM ZM (ProdM ZM (ProdM ZM ZM)))])

binaryTrees = M.fromList([(RuleM $ Rule "A", UnionM EpsM (ProdM ZM (ProdM (RuleM $ Rule "A") (RuleM $ Rule "A"))))])

dicoGF :: MinSpecGF
dicoGF = createOriginalDicoGF dicoMin

gfFinalTest :: Specification -> MinSpecGF
gfFinalTest dicoComplet = gfEGFN (specToMinSpec $ dicoComplet) (createOriginalDicoGF (specToMinSpec dicoComplet)) 20

