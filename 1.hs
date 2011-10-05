import Test.QuickCheck
import Text.Printf
 
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

-- 1
myLast  lst = last lst
prop_1a xs x = myLast  (xs ++ [x::String]) == x

myLast' = head . reverse
prop_1b xs x = myLast' (xs ++ [x::Int]) == x

myLast'' [] = error "shit! empty list"
myLast'' [x] = x
myLast'' (x:xs) = myLast'' xs
prop_1c xs x = myLast'' (xs ++ [x::Int]) == x

tests  = [("1a",                 quickCheck prop_1a)
         ,("1b",                 quickCheck prop_1b)
         ,("1c",                 quickCheck prop_1c)
         ]
