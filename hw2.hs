-- Name: Zixuan You, zyou@ucsc.edu. I have not found programming pair yet. If mandatory, I will try to find one for next homework.


-- The following is answer for Question 1

myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl f base [] = base                      -- Edge condition for empty list
myFoldl f base (x:xs) = myFoldl f (f base x) xs          -- Modify the base by f function, then recursively call myFoldl on tail of the list



-- The following is answer for Question 2

myReverse :: [a] -> [a]

myReverse = foldl (\ acc x -> x:acc) []              -- foldl extracts each element and put it before the lists already got, thus reversing the list.



-- The following is answer for Question 3

myFoldr :: (a -> b -> b) -> b -> [a] -> b

myFoldr f base xs = foldl (flip f) base (myReverse xs)        -- Use flip function to change the order of arguments, from a->b->b to b->a->b


-- The following is answer for Question 4

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a

myFoldl2 f base xs = foldr (flip f) base (reverser xs)       -- Use helper function reverser, which is also implemented by foldr, to get the reverse of list
	where reverser = foldr (\x acc -> acc ++ [x]) []


-- The following is answer for Question 5

isUpper :: Char -> Bool

isUpper = (`elem` ['A'..'Z'])     -- Check whether in range of 'A' to 'Z'


-- The following is answer for Question 6

onlyCapitals1 :: String -> String

onlyCapitals1 = filter (isUpper)         -- Use filter to reserve the character which return "Tree" on isUpper function


-- The following is answer for Question 7

onlyCapitals2 :: String -> String

onlyCapitals2 xs = [x| x<- xs, isUpper x]


-- The following is answer for Question 8

onlyCapitals3 :: String -> String

onlyCapitals3 "" = ""                    -- Edge condition for recursion

onlyCapitals3 (x:xs)
	| isUpper x = x:(onlyCapitals3 xs)
	| otherwise = onlyCapitals3 xs


-- The following is answer for Question 9

divRemainder :: Int -> Int -> (Int, Int)

divRemainder a b = ((a `div` b), (a `mod` b))


-- The following is answer for Question 10
-- My solution for this problem is to first transform the integer to a string, by show.
-- Then by using guards, I do the function recursively.
digitSum :: Int -> Int

digitSum x
	| length xs == 1 = (read xs) + 0                    -- Edge condition, if the number has only one digit, return that digit as sum
	| otherwise = read (take 1 xs) + digitSum (read (tail xs))            -- Recursively calculate the sum of digits other than the first one and add it with first digit
	where xs = show x






-- The following is answer for Question 11


-- Define words needed to say numbers

digits = ["", "one ", "two ", "three ", "four ", "five ", "six ", "seven ", "eight ", "nine "]

teens = ["", "eleven ", "twelve ", "thirteen ", "fourteen ", "fifteen ", "sixteen ", "seventeen ",
	"eighteen ", "nineteen "]

tens = ["", "ten ", "twenty ", "thirty ", "forty ", "fifty ", "sixty ", "seventy ", "eighty ", "ninety "]

bigs = ["", "hundred ", "thousand ", "million ", "billion ", "trillion ", "quadrillion ", "quintillion ", "sextillion ",
		"septillion ", "octillion ", "nonillion ", "decillion ", "undecillion ", "duodecillion ", "tredecillion ", 
		"quattuordecillion ", "quindecillion ", "sexdecillion ", "septendecillion ", "octodecillion ", "novemdecillion ", "vigintillion "]




-- Define a helper function: sayNumHundred, to handle the number from range 1 to 999 and say them

sayNumHundred :: String -> String
sayNumHundred "" = ""                               -- Two cases to handle the edge inputs
sayNumHundred "0" = "Zero"


-- check the hundred digits, if exsisting. Then handle the tens and unit digit, three cases may happen: from 11 to 19, from 0 to 9, or otherwise.
-- Note that tens !! 0, bigs !! 0 and teens !! 0 are all "", so that all 2-digit numbers can be handled from these cases.

sayNumHundred xs 
	| (fst numby100) /= 0 = digits !! (fst numby100) ++ bigs !! 1 ++ sayNumHundred (show (snd numby100))     -- hundred digit is not zero
	| ((fst numby10) == 1 && (snd numby10) /= 0) = teens !! (snd numby10)              -- from 11 to 19
	| otherwise = tens !! (fst numby10) ++ digits !! (snd numby10)                     -- either from 0 to 9 or from 20 to 99
	where numby100 = divRemainder (read xs) 100                             -- numby100 and numby10 make use of question 9
	      numby10 = divRemainder (read xs) 10




-- sayNum function, using helper sayNumHundred function
-- find out the string inside bigs by looking at the quotient of number length divided by 3. If remainder is not zero, index of bigs will be added 1.
-- Calculate the remainder of number length divided by 3, take this number of digit first, say them by helper function.
-- Then recursively say the rest of digits.

sayNum :: String -> String
sayNum "" = ""                          -- Two cases or error input, for robustness
sayNum "0" = "Zero"

sayNum xs
	| len >= 67 = "Error input, please input String of number from 1 to 10^66 - 1"            -- Error message if input number out of range
	| len <= 3 = sayNumHundred xs                                                             -- Edge condition
	| (snd quo) == 0 = sayNumHundred (take 3 xs) ++ bigs !! (fst quo) ++  sayNum (drop 3 xs)
	| otherwise = sayNumHundred (take (snd(quo)) xs) ++ bigs !! (1 + fst quo) ++ sayNum (drop (snd(quo)) xs)
	where len = length xs
	      quo = divRemainder len 3
