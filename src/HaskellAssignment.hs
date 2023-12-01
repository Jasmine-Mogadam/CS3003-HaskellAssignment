module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
  
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst func list
    | null list = NoMatch -- if list is empty, return NoMatch
    | func (head list) = Match 0 -- if head item matches, return Match 0
    | otherwise = case findFirst func (tail list) of -- run again without head
                    NoMatch -> NoMatch
                    Match index -> Match (index + 1)  -- increase index of Match

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome c
    | head c == last c = palindrome (init (tail c)) -- if the first and last are equal, continue with both removed
    | otherwise = False -- if first and last are not equal, it is not a palindrome
