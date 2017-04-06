module Learnyouahaskell.C05_Recursion where

--
-- 05. Recursion  http://learnyouahaskell.com/recursion
--

sum' [] = 0     -- edge condtion(or edge case) 재귀 탈출 조건(일반적으로 인풋의 조건이 말이안되는 시점)
sum' (x:xs) = x + sum' xs
-- main = print (sum' [1,2,3])

--sum'' :: (Num a) => [a] -> String     -- 이렇게만 하면 show :: Show a => a -> String 라 안됨
sum'' :: (Num a, Show a) => [a] -> String
sum'' list = "sum: " ++ show (innersum list)
    where innersum [] = 0
          innersum (x:xs) = x + innersum xs
--main = print (sum'' [1,2,3])

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last' xs
--main = print(last' "asdf")

takeLast :: Int -> [a] -> [a]
takeLast _ [] = []
takeLast n _
       | n <=0 = []
--takeLast n list@(_:xs) = if length list <= n then list else takeLast n xs  -- 방법 #1
takeLast n list = takeLast (n-1) (init list) ++ (last' list):[]      -- 방법 #2 ...이게 좀더 의미전달이 잘되는듯
--main = print (takeLast 8 [1,2,3])
--main = print (takeLast 2 "asdfqwer")

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [ s | s <- xs, s<x] ++ [x] ++ qsort [ l | l <- xs, l>=x]   -- 퀵소트가 2줄 대박!
--main = print(qsort "sdifjiejfnawef")
--main = print(qsort [5,1,-59,1,5,23,-55,1])
