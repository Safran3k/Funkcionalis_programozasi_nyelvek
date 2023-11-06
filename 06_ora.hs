module Hatodik_hazi where
import Text.Printf 

data Month = Month { name :: String, days :: Int }

months :: [Month]
months =
  [ Month "January" 31, 
    Month "February" 0,  -- Napok száma majd a paraméterül megadott évtől függ
    Month "March" 31,
    Month "April" 30,
    Month "May" 31,
    Month "June" 30,
    Month "July" 31,
    Month "August" 31,
    Month "September" 30,
    Month "October" 31,
    Month "November" 30,
    Month "December" 31
  ]

isLeapYear :: Int -> Bool
isLeapYear year
  | (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0) = True
  | otherwise = False

setFebruaryLength :: Int -> Month -> Month
setFebruaryLength year month =
  if isLeapYear year
    then month { days = 29 }
    else month { days = 28 }

setMonths :: Int -> [Month]
setMonths year = map (setFebruaryLength year) months

monthDayPairs :: Int -> [(String, String)]
monthDayPairs year =
  let adjustedMonths = setMonths year
  in [(name month, printf "%s %02d" (name month) day) | month <- adjustedMonths, day <- [1..(days month)]]



generateAlphabet :: [Char]
generateAlphabet = ['A'..'Z']

everyThird :: [Char]
everyThird = [c | (c, index) <- zip generateAlphabet [1..], (index `mod` 3) == 1]


