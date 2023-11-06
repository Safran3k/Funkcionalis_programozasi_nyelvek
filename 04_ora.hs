module Negyedik_hazi where

pairs :: Char -> Char -> Bool
pairs x y
  | x == '(' && y == ')' = True
  | x == '[' && y == ']' = True
  | x == '{' && y == '}' = True
  | otherwise = False

calculator :: (Int, Char, Int) -> Int
calculator (a, op, b)
  | op == '+' = a + b
  | op == '-' = a - b
  | op == '*' = a * b
  | op == '/' =
    if b == 0
      then error "Nullával osztás nem megengedett"
      else div a b

isSpace :: Char -> Bool
isSpace x
  | x == ' ' = True
  | otherwise = False