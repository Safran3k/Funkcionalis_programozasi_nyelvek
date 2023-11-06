module Harmadik_hazi where

fib1 :: Int -> Int
fib1 n
    | n <= 0 = 0
    | n == 1 = 1
    | otherwise = rectFib (n - 1) 0 1
  where
    rectFib :: Int -> Int -> Int -> Int
    rectFib 0 a _ = a
    rectFib n a b = rectFib (n - 1) b (a + b)


fib2 :: Int -> Int
fib2 n = f n 0 1
  where
    f :: Int -> Int -> Int -> Int
    f 0 a _ = 0
    f 1 a _ = a
    f n a b = f (n - 1) b (a + b)