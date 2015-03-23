-- Name: Zixuan You, zyou@ucsc.edu. I have not found programming pair yet. I will try to find one for next homework.


-- The following is answer for Question 2

citeAuthor :: String -> String -> String
citeAuthor first last = last ++ ", " ++ first


-- The following is answer for Question 3

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
	where (f:_) = firstName
	      (l:_) = lastName



-- The following is answer for Question 4

title :: (String, String, Int) -> String
title (_, bookTitle, _) = bookTitle


-- The following is answer for Question 5

citeBook :: (String, String, Int) -> String
citeBook (author, bookTitle, year) = bookTitle ++ " (" ++ author ++ ", " ++ (show year) ++ ")"



-- The following is answer for Question 6
-- By running following codes in ghci, I actually have '\n' in the output.
-- If you do want a real "new line", please input: putStrLn (bibliography_rec yourList) in the ghci, and it will output results line by line.

bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = ""                 --edge condition for empty list
bibliography_rec (x:[]) = citeBook x     --edge condition for only one element in the list, in this case no '\n' in the end.
bibliography_rec (x:xs) = citeBook x ++ "\n" ++ bibliography_rec xs


-- The following is answer for Question 7
-- Use the foldl to build the String, with the starting element as ""
-- Still using '\n' for new line. See the comments of Question 6 of really seeing them.

bibliography_fold :: [(String, String, Int)] -> String
bibliography_fold [] = ""                                                        -- add empty-list case to make the program more robust, and the line below handles non-empty list
bibliography_fold ys = foldl (\acc y -> acc ++ citeBook(y) ++ "\n") "" (init ys) ++ citeBook (last ys)      -- add '\n' between two cites, but not in the end


-- The following is answer for Question 8
-- average year in type of Int will be returned, so averageYear [("","",1993),("","",1994)] will have Int result 1993.

-- The following is a helper function getLast3, which extracts the last component of any 3-tuple. a,b,c can be any concrete type, same or different.
getLast3 :: (a,b,c)->c
getLast3 (_,_,x) = x

-- Function to return average publication year, making use of helper function
averageYear :: [(String, String, Int)] -> Int
averageYear xs = (sum $ map (getLast3) xs) `div` (length xs)




-- The following is answer for Question 9
-- (words txt) transform the String to a list of String (word); filter p keeps the strings which has form of "[...]" in the list, indicating they are references.
-- length function counts the length of list, i.e. the number of references.


references :: String -> Int
references txt = length $ filter p (words txt)
	where p x = (head x == '[' && last x == ']')



-- The following is answer for Question 10

-- First, define a helper funciton referenceIndex to extract the number inside reference.
-- By subtracting that number by 1 can we get its index in the [(String, String, Int)] list
-- By using read functions we convert that number to Int, which can be used as index.
referenceIndex :: String -> Int
referenceIndex x = read ( (head ( tail x)):[] ) - 1


-- Recursive function to cite text, taking the reference list [(String, String, Int)] and a list of Strings (words)
-- Return a list of words, with references substituted by responding citations 
citeTextRec :: [(String, String, Int)] -> [String] -> [String]
citeTextRec _ [] = []                               -- edge condition
citeTextRec xs (x:xxs)                         
	| p x = (citeBook (xs !! (referenceIndex x)) ) : (citeTextRec xs xxs)              -- if x is a reference "[n]", substitute it (by index of reference) and recursively call function on tail.
	| otherwise = x : (citeTextRec xs xxs)                                             -- otherwise, recursive call function on tail and concat x with the result
		where p x = (head x == '[' && last x == ']')



-- Words transform the txt into a list of Strings (words), then use citeTextRec to replace the reference
-- unwords the result to build a single String then return
citeText :: [(String, String, Int)] -> String -> String
citeText xs txt = unwords (citeTextRec xs (words txt))
