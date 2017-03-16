module Learnyouahaskell.C03_TypesAndTypeclasses where

--
-- 03. Types and Typeclasses http://learnyouahaskell.com/types-and-typeclasses
--


-- Haskell has static type system and type inference.

addThree :: Int -> Int -> Int -> Int    -- 함수의 타입 선언부. 함수도 타입이 있음. addThree 의 타입은 Int -> Int -> Int -> Int
addThree x y z = x + y + z


head' :: [a] -> a   -- type variable. 모든(혹은 제한된)타입이 가능함을 의미. type variable이 있는 함수를 polymorphic function 이라 함.
head' (x:_) = x
--main = print( head' [1,2,3] )


-- typeclass. 뭔가 타입이 어떤 특정형태 기능구현이 되있음을 명시. interface 비슷
-- (==) :: (Eq a) => a -> a -> Bool
-- 여기서 '(Eq a) =>' 이부분을 a에 대한 class constraint 라 함. , 로 구분해서 여러개 가능


--main = print (read "3")     -- 컴팔 에러! 앰비겨스 타입. read 의 리턴 타입이 type variable 인데 "3" 을 뭘로 변환할지 모름.
--main = print (read "3"::Int)  -- 리턴값의 타입이 Int임을 명시함. type annotation
--main = print ([ 1 | _ <- _::[Char]])    -- 이건 이래도 안되네 ㅋ

-- Num typeclass 는 숫자형(Int, Integer, Float, Double)의 상위 typclass 임(has)
-- Integral  는 Int, Integer 의 상위
-- Floating  는 Float, Double 의 상위
