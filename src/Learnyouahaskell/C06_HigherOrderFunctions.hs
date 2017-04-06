module Learnyouahaskell.C06_HigherOrderFunctions where

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
  flip'' :: (a -> b -> c) -> b -> a -> c  -- 인풋이 (a -> b -> c), b, a 리턴이 c
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

  -- lambda
  -- main = print(zipWith (\a b -> a + b) [1..10] [10,20..100])

  addThree :: (Num a) => a -> a -> a -> a
  -- addThree x y z = x + y + z
  addThree = \x -> \y -> \z -> x + y + z  -- 위에랑 같음. 함수 호출 방식이 기본 커링임을 람다로 표현함. 인풋이 x 리턴이 (\y -> \z -> x + y + z) 인 람다...

  -- flip'' f = \x y -> f y x  -- f 는 인풋이 x, y 리턴이 f y x 인(f로 y f 로 호출함) 람다. 람다를 이용해 새로운 함수를 만들어 냈음을 명시적으로 보여줌
  -- 함수의 어떤 파라매터가 함수로서 사용된다는 것을 명시적으로 보여줄때 위와 같이 람다를 쓰면 적절함.

  -- 리스트를 순회해서 뭔가 얻으려고 한다면 fold 를 사용
  -- foldl (\acc x -> acc + x) 0 [1..10]  -- sum [1..10]  foldl (+) 0 [1..10]
  sum' :: (Num a) => [a] -> a
  sum' = foldl (\acc x -> acc +x) 0

  -- foldl (\acc x -> x:acc) [] [1..10]  -- reverse [1..10]
  reverse' :: [a] -> [a]
  reverse' = foldl (\acc x -> x:acc) []

  -- foldl (\acc x -> acc ++ [x + 1]) [] [1..10]  -- map (\x -> x +1) [1..10]
  -- foldr (\x acc -> (x + 1):acc) [] [1..10] -- map (\x -> x + a) [1..10] 위에 ++ 연산자보다 적은 비용
  map' f xs = foldr (\x acc -> f x:acc) [] xs

  -- foldr (\x acc -> if odd x then x:acc else acc) [] [1..10]  -- filter odd [1..10]
  -- filter' f xs = foldr (\x acc -> if f x then x:acc else acc) []

  -- foldl (\_ x -> x) 0 [1..10]  -- head [1..10]
  head' :: [a] -> a
  head' = foldl1 (\acc x -> x)

  -- foldr (\x _ -> x) 0 [1..10]  -- last [1..10]
  last' :: [a] -> a
  last' = foldr1 (\x acc -> x)


  -- fold 를 하는데 그 진행되는 과정을 남길때 scan 을 사용
  -- scanl (\acc x -> acc + x) 0 [1..10]  -- 1~10 까지 더해지는 과정
  -- scanr (\x acc -> acc + x) 0 [1..10]


  -- $ 함수 호출(function application)시 연산 우선순위를 최하로 낮춰주는 함수
  -- ($) :: (a -> b) -> a -> b
  -- f $ x = fx
  -- () 이거로 우선순위 조절 했던거를 $로 대체 가능
  main = print $ zipWith (\a b -> a + b) [1..10] [10,20..100]  -- print 문은 앞으로 이렇게!

  -- . 함수 합성(function composition) 연속 함수 호출을 좀더 간결하게 표현 가능
  -- (.) :: (b -> c) -> (a -> b) -> a -> c
  -- f . g = \x -> f (g x)
  -- succ . negate $ 3  -- succ(negate 3)
  -- sum . replicate 5 . max 6.7 $ 8.9  -- 파라매터가 여러개인 경우는 부분적용 + 커링을 이용
  -- 함수 호출하면서 ) 이거로 닫는게 많아진다 싶으면 사용자

  -- point free style(pointless style)
  -- fn x = ceiling (negate (tan (cos (max 50 x))))
  -- fn = ceiling . negate . tan . cos . max 50 -- = 을 기준으로 양쪽의 x는 생략 가능
  -- 함수 호출 체인이 길어지면 그냥 let binding 으로 의미에 맞게 네이밍해서 가독성을 높여보자
