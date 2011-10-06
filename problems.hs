import Test.QuickCheck
import Text.Printf
 
-- 99 Haskell Problems

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

prop_1a xs x = myLast  (xs ++ [x::String]) == x
prop_1b xs x = myLast' (xs ++ [x::Int]) == x
prop_1c xs x = myLast'' (xs ++ [x::Int]) == x
prop_1d xs x = myLast''' (xs ++ [x::Bool]) == x
prop_1e xs x = myLast'''' (xs ++ [x::Bool]) == x

tests  = [("1a",                 quickCheck prop_1a)
         ,("1b",                 quickCheck prop_1b)
         ,("1c",                 quickCheck prop_1c)
         ,("1d",                 quickCheck prop_1d)
         ,("1e",                 quickCheck prop_1e)
         ]
