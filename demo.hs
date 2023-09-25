module Masodik_hazi where

isTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
isTriangle a b c
    | a <= 0 || b <= 0 || c <= 0 = False  
    | a + b <= c || a + c <= b || b + c <= a = False  
    | otherwise = True 


isPythagoreanTriple :: Int -> Int -> Int -> Bool
isPythagoreanTriple a b c
    | a <= 0 || b <= 0 || c <= 0 = False  
    | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = True
    | otherwise = False  


isLeapYear :: Int -> Bool
isLeapYear year
    | (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0) = True
    | otherwise = False