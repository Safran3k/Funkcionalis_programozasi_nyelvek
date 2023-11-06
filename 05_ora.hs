module Otodik_hazi where
import Text.Printf


powersOfTwo :: [Int]
powersOfTwo = [2 ^ p | p <- [0..]]


average :: Fractional a => [a] -> a
average [] = error "List is empty."
average listInput = averageHelper listInput 0 0
  where
    averageHelper [] sum amountOfElements = sum / fromIntegral amountOfElements
    averageHelper (x:xs) sum amountOfElements = averageHelper xs (sum + x) (amountOfElements + 1)

hoursAndMinutes :: [(String, String)]
hoursAndMinutes = [(printf "%02d" hour, printf "%02d" minute) | hour <- [0..23 :: Int], minute <- [0..59 :: Int]]

exponentiation :: Int -> Int -> Int
exponentiation _ 0 = 1
exponentiation x n
  | n > 0 = x ^ n
  | otherwise = 1 `div` (x ^ (-n))
