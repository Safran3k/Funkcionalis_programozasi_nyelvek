module Nyolcadik_hazi where


merge' :: [Int] -> [Int] -> [Int]
merge' [] ys = ys
merge' (x:xs) ys = x : merge' xs ys

insert' :: Int -> [Int] -> [Int]
insert' n [] = [n]
insert' n (m:ms)
    | n < m = n : m : ms
    | otherwise = m : insert' n ms

sort' :: [Int] -> [Int]
sort' [] = []
sort' (n:ns) = insert' n (sort' ns)

mergeSort :: [Int] -> [Int] -> [Int]
mergeSort xs ys = sort' (merge' xs ys)




