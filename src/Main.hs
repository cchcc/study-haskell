module Main where

--
-- 06. Higher order functions  http://learnyouahaskell.com/higher-order-functions
--


-- 하스켈은 함수의 기본 동작이 커링임 -> 파라매터를 다 안채워서 호출하면 부분 적용된 함수가 리턴됨
plusXYZ x y z = x + y + z
plus5YZ = plusXYZ 5     -- plusXYZ 의 내용이 부분 적용됨 : 5 + y + z
--main = print (plus5YZ 1 1 )

-- 커링함수 : 함수의 인자갯수가 1개이고 부분적용된 함수를 리턴함
-- 부분적용 함수 : 함수의 인자갯수와 상관없이 부분적용된 함수를 리턴
-- 부분함수 : 스칼라에 있는개념

-- Section of an infix operator. 이건 뭐 별 설명도 없이 갑툭튀야.. https://wiki.haskell.org/Section_of_an_infix_operator
xbyten = (/10)       -- x / 10
tenbyx = (10/)       -- 10 / x
--main = print ( tenbyx 100)
sub10 = (subtract 10)   -- 다만 (-10) 의 경우 단항 연산으로 음수로 판단하므로 이경우에는 (subtract 10) 이렇게 사용함
--main = print (sub10 10)
isUpperAlphanum = (`elem` ['A'..'Z'])   -- 사칙연산 말고도 이렇게도 사용가능


applyTwice :: (a -> a) -> a -> a    -- high order function
applyTwice f x = f (f x)
--main = print (applyTwice (+3) 3)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]    -- (a -> b -> c) 이넘이 f
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
--zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

flip' :: (a -> b -> c) -> (b -> a -> c) -- 인풋이 (a -> b -> c) 리턴이 (b -> a -> c)
flip' f = g where g x y = f y x
flip'' :: (a -> b -> c) -> b -> a -> c  -- 인풋이 (a -> b -> c) 랑 b, a 리턴이 c
flip'' f y x = f x y
--zipWith (flip' div) [2,2..] [10,8,6,4,2]


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
--filter' p (x:xs)
--    | p x       = x : filter p xs
--    | otherwise = filter p xs
filter' p (x:xs) = if(p x) then x : filter' p xs else filter' p xs  -- 위에꺼랑 같음
--main = print(filter' even [1,2,3,4,5,6])

funs = map (*) [0..]  -- 이건 리턴이 함수들의 리스트임 [(0*),(1*),(2*),(3*),(4*),(5*)...
--main = print ( (funs !! 3) 4)  -- (3*) 4


main = print "hello world"
