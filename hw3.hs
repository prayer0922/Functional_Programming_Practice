-- Name: Zixuan You, zyou@ucsc.edu. I have not found programming pair yet. If mandatory, I will try to find one for next homework.

import Data.List

data BST k v = Empty |
               Node k v (BST k v) (BST k v)


-- The following is answer for Question 1

val :: BST k v -> Maybe v

val Empty = Nothing
val (Node _ v _ _) = Just v



-- The following is answer for Question 2

size :: BST k v -> Int

size Empty = 0                -- Edge condition
size (Node _ _ left right) = 1 + size left + size right


-- The following is answer for Question 3

ins :: (Ord k) => k -> v -> BST k v -> BST k v

ins k v Empty = Node k v (Empty) (Empty)              -- Edge condition

ins k v (Node k' v' left right)
	| k == k' = (Node k' v left right)                -- If key already used, just update its value
	| k < k' = (Node k' v' (ins k v left) right)          -- Recursively insert into left subtree
	| otherwise = (Node k' v' left (ins k v right))       -- Recursively insert into right subtree



-- The following is answer for Question 4

instance (Show v) => Show (BST k v) where
  show Empty = ""
  show (Node k v left right) = "("++ (show left) ++ (show v) ++ (show right) ++ ")"



-- The following is answer for Question 5

data JSON = JStr String
          | JNum Double
          | JArr [JSON]
          | JObj [(String, JSON)]


instance Show JSON where
  show (JStr str) = show str
  show (JNum num) = show num
-- Use intercalate from Data.List to insert commas, JArr [] will output: []  
  show (JArr j) = "[" ++ intercalate "," (map show j) ++ "]"
-- Use intercalate from Data.List to insert commans, JObj [] will output: {}
  show (JObj obj) = "{" ++ intercalate "," (map (\(str, j) -> (show str) ++ ":" ++ (show j)) obj) ++ "}"




-- The following is answer for Question 6

class Json a where
  toJson :: a -> JSON
  fromJson :: JSON -> a

instance Json Double where
  toJson = JNum
  fromJson (JNum d) = d
  fromJson _ = error "Can only transform from JNum to Double"        -- For wrong input, make it robust to non-exhaustive pattern warnings.



instance (Json a) => Json [a] where
  toJson a = JArr (map toJson a)
  fromJson (JArr j) = map fromJson j                                 -- fromJson (JArr [JNum 2,JNum 3]) :: [Double] with explicit type signiture will work
  fromJson _ = error "can only transform JArr to List"               -- For wrong input, make it robust to non-exhaustive pattern warnings.



