import Test.QuickCheck
import Text.Printf
import Test.QuickCheck
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

-- 1 (get last element of a list)
-- Each test takes an array and an element to add to the end
-- and checks that myLast returns the element that we added to the end.
myLast lst = last lst

myLast' = head . reverse

myLast'' [x] = x
myLast'' (x:xs) = myLast'' xs

myLast''' = last

myLast'''' xs = head $ reverse xs
-- same as
-- myLast'''' xs = head (reverse xs)

prop_1 :: [([String] -> String)] -> [String] -> String -> Bool
prop_1 [] _ _ = True
prop_1 (f:fs) xs x = f(xs ++ [x]) == x && prop_1 fs xs x

-- 2 (find second to last element of list
myButLast :: [a] -> a
myButLast xs = last $ take 2 $ reverse xs
myButLast' xs = reverse xs !! 1
myButLast'' = last . init
prop_2 :: [([String] -> String)] -> [String] -> String -> String -> Bool
prop_2 [] _ _ _ = True
prop_2 (f:fs) xs x y = f(xs ++ [x] ++ [y]) == x && prop_2 fs xs x y

-- 3 (find k"th element of a list)
element_at xs x = xs !! x
prop_3a xs x = (x < length xs && x >= 0) ==> element_at xs (x::Int) == (xs !! x::Int)


tests  = [("1",  quickCheck $ prop_1 [myLast, myLast', myLast'', myLast''', myLast''''])
         ,("2",  quickCheck $ prop_2 [myButLast, myButLast''])
         --,("3a",                 quickCheckN 1000 prop_3a)
         ]

