module Main where

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
main = do
    print (hi2 (1,2))
    print (hi2 (1,"aa"))





--main = print "hello world"