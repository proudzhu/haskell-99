module Haskell99 where

import Data.List

-- List
-- | Problem 1
-- Find the last element of a list.
--
-- >>> myLast [1,2,3,4]
-- 4
--
-- >>> myLast ['x','y','z']
-- 'z'
myLast :: [a] -> a
myLast [] = error "Empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- | Problem 2
-- Find the last but one element of a list.
--
-- >>> myButLast [1,2,3,4]
-- 3
--
-- >>> myButLast ['a'..'z']
-- 'y'
myButLast :: [a] -> a
myButLast [] = error "Empty list!"
myButLast [_] = error "Not enough elements!"
myButLast (x:xs) = if length xs == 1 then x else myButLast xs

-- | Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
--
-- >>> elementAt [1,2,3] 2
-- 2
--
-- >>> elementAt "haskell" 5
-- 'e'
elementAt :: [a] -> Int -> a
elementAt [] 1 = error "Not enough elements!"
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)

-- | Problem 4
-- Find the number of elements of a list.
--
-- >>> myLength [123,456,789]
-- 3
--
-- >>> myLength "Hello, world!"
-- 13
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- | Problem 5
-- Reverse a list.
--
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
--
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- | Problem 6
-- Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x).
--
-- >>> isPalindrome [1,2,3]
-- False
--
-- >>> isPalindrome "madamimadam"
-- True
--
-- >>> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome lst = lst == myReverse lst

-- | Problem 7
-- Flatten a nested list structure.
--
-- >>> flatten (Elem 5)
-- [5]
--
-- >>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
--
-- >>> flatten (List [])
-- []
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- >>> flatten2 (Elem 5)
-- [5]
--
-- >>> flatten2 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
--
-- >>> flatten2 (List [])
-- []
flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List x) = concatMap flatten2 x

-- | Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element.
-- The order of the elements should not be changed.
--
-- >>> compress "aaaabccaadeeee"
-- "abcade"
compress :: (Eq a) => [a] -> [a]
compress = map head . group

-- | Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
--
-- >>> pack ['a','a','a','a','b','c','c','a','a','d','e','e','e','e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: (Eq a) => [a] -> [[a]]
pack = group
