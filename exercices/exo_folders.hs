isAll :: (Eq a) => [a] -> a -> Bool
isAll xs val  = foldl (\acc x -> x==val && acc) True xs

lengthWithFoldr :: [a] -> Int
lengthWithFoldr xs = foldr (\x -> (+) 1) 0 xs 

mapfolder :: [a] -> (a -> b) -> [b]
mapfolder xs f = foldr (\x -> (:) (f x)) [] xs

revFold :: [a] -> [a]
revFold xs = foldl (\acc x -> x : acc) [] xs 

prefixesFold :: [a] -> [[a]]
prefixesFold xs = foldr (\x acc -> acc ++ [(last acc)++[x]]) [[]] xs