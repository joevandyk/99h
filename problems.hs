import Test.QuickCheck
import Text.Printf
import Control.Monad
import Data.List
import qualified Data.Map as M 
import Control.Monad.State hiding (when)

 
-- 99 Haskell Problems
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10
--
-- http://cseweb.ucsd.edu/classes/wi11/cse230/lectures/quickcheck.html 
-- is a great place to learn about quickCheck.

quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

app x = if x == "hello" then x ++ ", world" else x

-- 1 (get last element of a list)
-- Each test takes an array and an element to add to the end
-- and checks that myLast returns the element that we added to the end.
myLast = last 

myLast' [x] = x
myLast' (x:xs) = myLast' xs

prop_1 :: [[String] -> String] -> [String] -> String -> Bool
prop_1 [] _ _ = True
prop_1 (f:fs) xs x = f(xs ++ [x]) == x && prop_1 fs xs x

-- 2 (find second to last element of list
myButLast :: [a] -> a
myButLast xs = last $ take 2 $ reverse xs
myButLast' xs = reverse xs !! 1
myButLast'' = last . init
prop_2 :: [[String] -> String] -> [String] -> String -> String -> Bool
prop_2 [] _ _ _ = True
prop_2 (f:fs) xs x y = f(xs ++ [x] ++ [y]) == x && prop_2 fs xs x y

-- 3 (find k"th element of a list. first element is 1.)
elementAt :: [a] -> Int -> a
elementAt xs x = last $ take x xs

prop_3 :: NonEmptyList String -> Property
prop_3 (NonEmpty xs) = forAll (choose (1, length xs)) test
  where test i = elementAt xs i == (xs !! (i-1))


-- 4 (find the number of elements in a list)
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

prop_4 :: [String] -> Bool
prop_4 xs = myLength xs == length xs

prop_4a :: [String] -> Property
prop_4a xs = forAll arbitrary test 
  where test xs = myLength xs == length (xs::[String])


-- 5 (reverse a list)
-- inefficient, because ++ rebuilds the list.
-- using (:) is faster.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

prop_5a :: [Int] -> Bool
prop_5a xs = myReverse (myReverse xs) == xs
prop_5b :: [Int] -> Bool
prop_5b xs = myReverse xs == reverse xs
prop_5c :: String -> Bool
prop_5c xs = myReverse xs == foldl (flip (:)) [] xs


-- 6 (check to see if a string is a palindrome)
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs
--isPalindrome xs = True

prop_6a :: String -> Bool
prop_6a s = isPalindrome (s ++ reverse s)
prop_6b :: String -> Property
prop_6b s = not (null s) ==> isPalindrome (s ++ (tail.reverse) s)

-- Fails, need to figure out how to randomly generate things that 
-- aren't palindromes.
prop_6c i s = not (null s) ==>  
              i > 0        ==> 
              i < length s ==> 
              i*2 /= length s ==> 
              not (isPalindrome (take i s ++ "•" ++ drop i s))


-- 7 (flatten a nested list structure)
-- TODO review this.  Also, write test for it.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

tests  = [("1",  quickCheck $ prop_1 [myLast, myLast'])
         ,("2",  quickCheck $ prop_2 [myButLast, myButLast''])
         ,("3",  quickCheck prop_3)
         ,("4",  quickCheck prop_4)
         ,("4a", quickCheck prop_4a)
         ,("5a", quickCheck prop_5a)
         ,("5b", quickCheck prop_5b)
         ,("5c", quickCheck prop_5c)
         ,("6a", quickCheck prop_6a)
         ,("6b", quickCheck prop_6b)
         --,("6c", quickCheck $ prop_6c)
         ]

