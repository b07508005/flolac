-- This exercise covers the first 6 chapters of "Learn You a Haskell for Great Good!"

-- Chapter 1 - http://learnyouahaskell.com/introduction
-- Chapter 2 - http://learnyouahaskell.com/starting-out
-- Chapter 3 - http://learnyouahaskell.com/types-and-typeclasses
-- Chapter 4 - http://learnyouahaskell.com/syntax-in-functions
-- Chapter 5 - http://learnyouahaskell.com/recursion
-- Chapter 6 - http://learnyouahaskell.com/higher-order-functions

-- Download this file and then type ":l Chapter-1-6.hs" in GHCi to load this exercise
-- Some of the definitions are left "undefined", you should replace them with your answers.

-- Find the penultimate (second-to-last) element in list xs
penultimate :: [ a ] -> a
penultimate xs = last . init $ xs

-- Find the antepenultimate (third-to-last) element in list xs
antepenultimate :: [ a ] -> a
antepenultimate xs = last . init . init $ xs 

-- Left shift list xs by 1
-- For example, "shiftLeft [1, 2, 3]" should return "[2, 3, 1]"
shiftLeft :: [ a ] -> [ a ]
shiftLeft [] = []
shiftLeft xs = tail xs ++ [ head xs ] 

-- Left shift list xs by n
-- For example, "rotateLeft 2 [1, 2, 3]" should return "[3, 1, 2]"
rotateLeft :: Int -> [ a ] -> [ a ]
rotateLeft n xs 
 | n <= 0 = xs
 | otherwise = ( rotateLeft . pred ) n $ shiftLeft xs

-- Insert element x in list xs at index k
-- For example, "insertElem 100 3 [0,0,0,0,0]" should return [0,0,0,100,0,0]
insertElem :: Int -> Int -> [ Int ] -> [ Int ]
insertElem x k xs 
 | k == 0 = x : xs
 | null xs = []
 | otherwise = head xs : insertElem x ( pred k ) ( tail xs )

-- Here we have a type for the 7 days of the week
-- Try typeclass functions like "show" or "maxBound" on them
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
         deriving (Eq, Ord, Show, Bounded, Enum)   

-- Note that if you try "succ Sun", you should get an error, because "succ" is not defined on "Sun"
-- Define "next", which is like "succ", but returns "Mon" on "next Sun"
next :: Day -> Day
next day 
 | day == Sun = Mon
 | otherwise = succ day

-- Return "True" on weekend
isWeekend :: Day -> Bool
isWeekend day = day == Sat || day == Sun

data Task = Work | Shop | Play deriving (Eq, Show)

-- You are given a schedule, which is a list of pairs of Tasks and Days
schedule :: [(Task, Day)]
schedule = [(Shop, Fri), (Work, Tue), (Play, Mon), (Play, Fri)]

-- However, the schedule is a mess
-- Sort the schedule by Day, and return only a list of Tasks. 
-- If there are many Tasks in a Day, you should keep its original ordering
-- For example, "sortTask schedule" should return "[(Play, Mon), (Work, Tue), (Shop, Fri), (Play, Fri)]"
merge ::  [(Task, Day)] -> [(Task, Day)] ->  [(Task, Day)]
merge xs ys
 | null xs = ys
 | null ys = xs
 | ( snd $ head ys ) < ( snd $ head xs ) = merge ys xs
 | otherwise = head xs : merge ( tail xs ) ys 

sortTask :: [(Task, Day)] -> [(Task, Day)]
sortTask xs 
 | null xs = []
 | half_length == 0 = xs
 | otherwise = merge ( sortTask $ take half_length xs ) ( sortTask $ drop half_length xs ) 
 where half_length = ( length xs ) `div` 2

-- This function converts days to names, like "show", but a bit fancier
-- For example, "nameOfDay Mon" should return "Monday"
nameOfDay :: Day -> String
nameOfDay x 
 =  head [ y | y<- [ "Monday", "Tuesday", "Wendesday", "Thursday", "Friday", "Saturday", "Sunday" ],  show x == ( take 3 y ) ]

-- You shouldn't be working on the weekends
-- Return "False" if the Task is "Work" and the Day is "Sat" or "Sun"
labourCheck :: Task -> Day -> Bool
labourCheck task day = task /= Work || ( day /= Sat && day /= Sun )

-- Raise x to the power y using recursion
-- For example, "power 3 4" should return "81"
power :: Int -> Int -> Int
power x y 
 | x == 0 = 0 
 | y == 0 = 1
 | otherwise = x * ( power x $ pred y  )

-- Convert a list of booleans (big-endian) to a interger using recursion
-- For example, "convertBinaryDigit [True, False, False]" should return 4
convertBinaryDigit :: [Bool] -> Int
convertBinaryDigit bits
 | null bits = 0
 | otherwise = ( if last bits then 1 else 0 ) + 2 * ( convertBinaryDigit $ init bits )

-- Create a fibbonaci sequence of length N in reverse order
-- For example, "fib 5" should return "[3, 2, 1, 1, 0]"
fib :: Int -> [ Int ]
fib n
 | n == 1 = [ 0 ]
 | n == 2 = [ 1, 0 ]
 | otherwise = head ( fib ( n - 1  ) ) + head ( fib ( n - 2 ) ) : fib ( n - 1 )

-- Determine whether a given list is a palindrome
-- For example, "palindrome []" or "palindrome [1, 3, 1]" should return "True"
palindrome :: Eq a => [a] -> Bool
palindrome xs
 | null xs || length xs == 1 = True
 | head xs == last xs = palindrome . init . tail $ xs
 | otherwise = False 

-- Map the first component of a pair with the given function
-- For example, "mapFirst (+3) (4, True)" should return "(7, True)"
mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f pair = ( f . fst $ pair, snd pair )  

-- Devise a function that has the following type
someFunction :: (a -> b -> c) -> (a -> b) -> a -> c
someFunction a_b_c a_b a = a_b_c a $ a_b a