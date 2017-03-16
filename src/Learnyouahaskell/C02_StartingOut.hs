module Learnyouahaskell.C02_StartingOut where  -- 모듈명은 첫글자 소문자/숫자가 안되는듯

--
-- 02. Starting Out http://learnyouahaskell.com/starting-out
--

-- main = do   -- 이거 함수 하나에서 여러줄 쓸때 씀
--    print (3 == 3)
--    print (3 /= 3)      -- 하스켈에서 not 은 /=


--DoubleMe x = x + x      -- 컴팔에러! 함수명 첫글자는 대문자로 못씀.. ㅠㅠ
--main = print (DoubleMe 8)


conanO'Brien = "It's a-me, Conan O'Brien!"  -- 함수명에 ' apostrophe(작은따옴표) 이거는 그냥 일반문자취급함.
--main = print (conanO'Brien)
-- 주로 원본함수가 있으면 그거를 뭔가 살짝바꿧다는 의미로 원본함수명 뒤에 붙여서 사용함 예를들면
hi = print "hi"
hi' = print "hi!"  -- hi 함수를 살짝 바꿧다
--main = do
--    hi
--    hi'

-- if 문도 하나의 평가식
doubleSmallNumber x = if x > 100 then x else x*2

-- lists are a homogenous data structure
-- 호모... 호모지녀스? 이게 뭔말이냐면 리스트안에 들어가는 모든 항목은 동일한 타입이라는 말임
-- main = print [1,'a']    -- 즉 이런건 안됨. 컴팔에러!

--main = print ['h','e','l','l','o']  -- 문자열(string)은 사실 character 의 리스트임
--main = print (['h','e'] ++ ['l','l','o'])  -- 리스트 끼리 합칠때
--main = print (0:[1,2,3])  -- 리스트가 아닌 놈을 리스트랑 합칠때. [1,2,3] 은 사실 1:2:3:[] 이거의 싱텍스 슈거
--main = print ([0,1,2]:1)  -- 컴팔에러! 반대로는 안되넹
--main = print ("hello" !! 3)   -- 리스트의 특정 인덱스 접근할때. 0부터 시작

--main = do
--    print ([1,2,3] == [1,2,3])      -- 리스트끼리 비교 가능함
--    print ([1,2,3] < [1,3,3])       -- 앞에꺼부터 순서대로 비교하는데 만약 같으면 다음 항목꺼를 비교
--    print ([1,2,3] < [0])


--main = do
--    print (take 3 [1,2,3,4,5])
--    print (3 `take` [1,2,3,4,5])    -- infix function. 첫 파라매터를 제일 앞에 두고 싶으면 이렇게 쓰나봄(?). 이건 뭐 자세한 설명이 없네?
--                                     영문법 어순 주+동+목 이렇게 표현하고 싶으면 쓰나봄..  이건뭐 비영어권에서는 무쓸모?


--main = do
--    print [1..20]
--    print [2,4..20]
--    print [0.2,0.4..2.0]    -- 결과가?? range 는 소수점에는 쓰지 말자.. (이럴거면 컴팔도 안되도록 해주던가 하지..)
--    print [1..]         -- 하스켈은 무한대 표현이 가능함


-- list comprehension
-- 왠 수학식같은게 나와서 쫄았더니.. 별거 아님.
-- [ output  |  binding  ,  predicate ]
--main = print ([x | x <-[1] ])
--main = print ([x | x <-1 ]) -- 컴팔에러! 바인딩할때 인풋은 리스트만 되는듯

--main = print ([ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2])  -- 각변의 길이가 10이하인 직각 삼각형들


length' xs = sum [1 | _ <- xs]      -- _ 이거는 뭐가됬던 신경 안쓰겠다는 의미
--main = print (length' [1,2,3])
--main = print ([ 1 | _ <- _])    -- 컴팔에러!  앰비겨스 타입. _ 얘는 타입이 [t0] 이라고 나오는데 이게 뭐임??


-- tuple
--main = print (fst (1,"hi")) -- not homogenous

--sel4 (_,_,_,x) = x        -- 3개이상 튜플은 어떻게 항목을 가져오나.. 찾아보니 이렇게 하거나 패턴매칭으로 함.
--main = print ( sel4 (1,'a',"hi",0.1) )
-- https://hackage.haskell.org/package/tuple-0.3.0.2/docs/Data-Tuple-Select.html
