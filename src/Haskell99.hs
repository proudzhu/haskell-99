{-|
Module      : Haskell99
Description : Solutions for Ninety-Nine Haskell Problems
Copyright   : (c) proudzhu, 2016
License     : GPL-3
Maintainer  : proudzhu.fdu@gmail.com
Stability   : experimental
Portability : POSIX

Solutions for [Ninety-Nine Haskell Problems]
(https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)
-}
module Haskell99 where

import Data.List
import Control.Arrow
import System.Random (getStdGen, randomRIO, randomRs)

-- * List

-- ** Problem 1
-- | Find the last element of a list.
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


-- ** Problem 2
-- | Find the last but one element of a list.
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


-- ** Problem 3
-- | Find the K'th element of a list. The first element in the list is number 1.
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


-- ** Problem 4
-- | Find the number of elements of a list.
--
-- >>> myLength [123,456,789]
-- 3
--
-- >>> myLength "Hello, world!"
-- 13
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs


-- ** Problem 5
-- | Reverse a list.
--
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
--
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- ** Problem 6
-- | Find out whether a list is a palindrome.
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


-- ** Problem 7
-- | NestedList
data NestedList a = Elem a | List [NestedList a]

-- | Flatten a nested list structure.
--
-- >>> flatten (Elem 5)
-- [5]
--
-- >>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
--
-- >>> flatten (List [])
-- []
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


-- | Another implementation for flatten
--
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


-- ** Problem 8
-- | Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element.
-- The order of the elements should not be changed.
--
-- >>> compress "aaaabccaadeeee"
-- "abcade"
compress :: (Eq a) => [a] -> [a]
compress = map head . pack


-- ** Problem 9
-- | Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
--
-- >>> pack ['a','a','a','a','b','c','c','a','a','d','e','e','e','e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: (Eq a) => [a] -> [[a]]
pack = group

-- | Another implementation for pack
--
-- >>> pack2 ['a','a','a','a','b','c','c','a','a','d','e','e','e','e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 [x] = [[x]]
pack2 (x:xs) = if x `elem` head (pack xs)
               then (x:head (pack xs)) : tail (pack xs)
               else [x] : pack xs


-- ** Problem 10
-- | Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length
-- encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is
-- the number of duplicates of the element E.
--
-- >>> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: (Eq a) => [a] -> [(Int, a)]
encode lst = map (\x -> (length x, head x)) $ pack lst

-- | Another implementation for encode
--
-- >>> encode2 "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode2 :: (Eq a) => [a] -> [(Int, a)]
encode2 lst = map (length Control.Arrow.&&& head) $ pack lst


-- ** Problem 11
-- | ListItem
data ListItem a = Single a | Multiple Int a
    deriving (Show)

-- | Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.
--
-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified lst = map toListItem $ encode lst
    where toListItem (x,y) = if x == 1 then Single y else Multiple x y


-- ** Problem 12
-- | Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
--
-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeModifiedHelper
    where
        decodeModifiedHelper (Single x) = [x]
        decodeModifiedHelper (Multiple n x) = replicate n x


-- ** Problem 13
-- | Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates,
-- as in problem 9, but only count them. As in problem P11, simplify the result
-- list by replacing the singleton lists (1 X) by X.
--
-- >>> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs

-- | Helper function for encodeDirect
--
-- >>> encodeDirect' 1 'a' "aaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect' :: (Eq a) => Int -> a -> [a] -> [ListItem a]
encodeDirect' n y [] = [encodeElement n y]
encodeDirect' n y (x:xs)
    | y == x = encodeDirect' (n + 1) y xs
    | otherwise = encodeElement n y : encodeDirect' 1 x xs

-- | Convert (i, y) to ListItem
--
-- >>> encodeElement 1 'a'
-- Single 'a'
--
-- >>> encodeElement 3 'b'
-- Multiple 3 'b'
encodeElement :: Int -> a -> ListItem a
encodeElement 1 y = Single y
encodeElement n y = Multiple n y


-- ** Problem 14
-- | Duplicate the elements of a list.
--
-- >>> dupli [1,2,3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs


-- ** Problem 15
-- | Replicate the elements of a list a given number of times.
--
-- >>> repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) i = replicate i x ++ repli xs i


-- ** Problem 16
-- | Drop every N'th element from a list.
--
-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery lst i = dropEvery' i lst
    where
        dropEvery' _ [] = []
        dropEvery' 1 (_:xs) = dropEvery' i xs
        dropEvery' n (x:xs) = x : dropEvery' (n - 1) xs


-- ** Problem 17
-- | Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
--
-- split "abcdefghik" 3
-- ["abc", "defghik"]
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split lst@(x:xs) i
    | i > 0 = (x : ys, zs)
    | otherwise = ([], lst)
    where (ys,zs) = split xs (i - 1)


-- ** Problem 18
-- | Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits
-- included). Start counting the elements with 1.
--
-- >>> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice lst i j = drop (i - 1) $ take j lst


-- ** Problem 19
-- | Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
--
-- >>> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
--
-- >>> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate lst 0 = lst
rotate lst i = drop num lst ++ take num lst
    where num = i `mod` length lst


-- ** Problem 20
-- | Remove the K'th element from a list.
--
-- >>> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
removeAt i lst = (lst !! (i - 1), take (i - 1) lst ++ drop i lst)


-- ** Problem 21
-- | Insert an element at a given position into a list.
--
-- >>> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt x lst i = take (i - 1) lst ++ [x] ++ drop (i - 1) lst


-- ** Problem 22
-- | Create a list containing all integers within a given range.
--
-- >>> range 4 9
-- [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range i j = takeWhile (<= j) $ iterate (+1) i


-- ** Problem 23
-- | Extract a given number of randomly selected elements from a list.
--
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, length xs - 1) gen]


-- ** Problem 24
-- | Draw N different random numbers from the set 1..M.
--
diffSelect :: Int -> Int -> IO [Int]
diffSelect n x = rndSelect [1..x] n


-- ** Problem 25
-- | Generate a random permutation of the elements of a list.
--
rndPermu :: [a] -> IO [a]
rndPermu = rndElem . permutations

-- | RndElem
--
rndElem :: [a] -> IO a
rndElem lst = do
    index <- randomRIO (0, length lst - 1)
    return $ lst !! index
