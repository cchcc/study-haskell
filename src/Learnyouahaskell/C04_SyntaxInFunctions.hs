module Learnyouahaskell.C04_SyntaxInFunctions where

--
-- Syntax in Functions  http://learnyouahaskell.com/syntax-in-functions
--

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
--main = print (factorial 5)

addZeroToSecond :: (Num a) => [a] -> a
--addZeroToSecond [] = 0  -- 패텅매칭이 중간에 다른 함수선언이 들어가면 컴팔에러!
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
addZeroToSecond (x0:x1:x3:_) = x0 + x1 + x3     -- 근데 함수 타입 선언을 먼저해두고 나중에 패턴매칭 내용을 쓰면 에러안남.
addZeroToSecond (x0:x1:_) = x0 + x1     -- 리스트 :로 구분한 갯수가 안맞으면 매칭이 안됨
addZeroToSecond (x:_) = x       -- 패턴매칭은 위에서부터 매칭함. 만약 이 구문이 맨 위에 있었으면 위에 2줄은 쓸모없어짐.
addZeroToSecond [] = 0


head :: [a] -> a
head [] = error "여기다 에러내용..."  -- 런타임 에러생성은 이렇게
head (x:_) = x

--main = do
--    print (addZeroToSecond [1,1])
--    print (addZeroToSecond [])
--    Main.head []    --  뭐지? 이거 실행 할때마다 에러 구문 나오는 output 순서가 틀린데???
--    print (charName 'a')


hi :: String -> String
hi target@(x:_) = "hi " ++ target ++ ", start with " ++ x:[]   -- 패턴매칭의 닉네임
--main = print (hi "123")

hi2 :: (Show a, Show b) => (a,b) -> String
hi2 target@(f,s) = "hi " ++ show target     -- 패턴 매칭의 닉네임
--main = do
--    print (hi2 (1,2))
--    print (hi2 (1,"aa"))



sel2 (_,x,_) = x    -- _ 는 신경안쓰겠다는 의미

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
--main = print (bmiTell 10.1)

--bmiTell :: (RealFloat a) => a -> a -> String    -- 컴팔에러! 함수명이 동일하고 함수의 타입이 틀려도 같은 함수 시그니쳐로 인식함
bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                 = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2     -- bmiTell2 의 스콥에서만 쓸수있는 함수
--main = print (bmiTell 28.1)


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname     -- 패턴매칭도 가능
          (l:_) = lastname
--initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."
--main = print (initials "123" "asdf")


cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea      -- let <bindings> in <expression> : let 바인딩 구문을 in 영역에서 사용
--where 바인딩은 함수내에서 사용, let 바인딩은 함수내 뿐만 아니라 거의 모든 구문에서 사용가능함
--main = print [let square x = x * x in (square 5, square 3, square 2)]


--case expression of pattern -> result
--                   pattern -> result
--                   pattern -> result
--                   ...
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
--main = print (describeList "a")