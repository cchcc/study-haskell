module Learnyouahaskell.C13_For_a_Few_Monads_More where

--
-- 13. For a Few Monads More  http://learnyouahaskell.com/for-a-few-monads-more
--

import Data.Monoid  -- Sum
import Control.Monad.Writer

-- monad 는 mtl 패키지 안에 있다.(커맨드 : ghc-pkg list)

-- Writer monad

-- 뭔가 로그같은걸 붙이는 함수를 만들어 봄
isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")

-- 위 함수에 (3, "Smallish gang.") 를 적용 시켜보고 싶은데..
-- 일반 값이 있고 일반 값을 어떤 컨택스트가 있는 값으로 변환하는 함수를 적용해 보고 싶다? -> 이것은 Monad!

applyLog' :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog' (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

-- main = print $ (3, "Smallish gang.") `applyLog'` isBigGang
-- main = print $ ("Tobin","Got outlaw name.") `applyLog'` (\x -> (length x, "Applied length."))

-- applyLog 을 좀더 일반화 시켜보자. 
-- 아래처럼 '로그' 역할 하는 값을 그냥 리스트로 바꾸면, 이제 리스트면 ++ 연산으로 적용하는데 문제 없어 보인다.
-- applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])

-- 그런데.. bytestream 도 가능하게 해보려면?? bytestream 도 리스트의 ++ 처럼 둘이 합칠수 있는 연산이 필요하다.
-- ByteString typeclass 구현들을 살펴보니 Monoid 가 있네.. mappend 를 이용하자.
-- 그러면 '둘이 합칠수 있는 동작이 되는 뭐든' 이라는 개념으로 추상화를 해보면 리스트를 Monoid 로 일반화 시켜볼수 있다
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

-- 샘플을 짜보면
type Food = String
type Price = Sum Int  -- Sum 은 Data.Monoid 모듈에 있는 Num 을 합칠수 있는 newtype

addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)

testAddDrink = do
    print $ ("jerky", Sum 25) `applyLog` addDrink
    print $ ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink

-- main = testAddDrink

-- 지금까지 monoid 를 monadic value 처럼 행동하게 해봄 이제 Control.Monad.Writer 모듈에 있는 Writer 를 살펴보자
-- newtype Writer w a = Writer { runWriter :: (a, w) } -- a 가 값, w 는 monoid 이거 근데 왜 튜플 순서를 반대로 했을까

-- instance (Monoid w) => Monad (Writer w) where  
    -- return x = Writer (x, mempty)  -- default minimal context 는 monoid 의 mempty로
    -- (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v') -- applyLog

-- main = do
--     print $ runWriter (return 3 :: Writer String Int)
--     print $ runWriter (return 3 :: Writer (Sum Int) Int)
--     print $ (writer (1, [""]) :: Writer [String] Int)  -- Writer 가 아니라 소문자 writer
-- Writer 는 fail 구현이 없어서 do 에서 패턴매칭에 실패하면 error 가 호출된다

-- 본문 예제대로 하면 에러남.. 아래 링크를 참고하자
-- https://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

-- do 를 써본다면 아래처럼..
multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]  -- 그리고 중간에 monoid 값에만 뭔가 붙이고 싶은경우 tell 을 사용
    return (a*b)
    -- return (tell ["Gonna multiply these two"])  -- tell 은 monoid 처리만 하고 더미값 m () 을 리턴하므로 마지막줄에 쓸수없다

-- main = print $ runWriter multWithLog

-- 최대 공약수 구하는 유클리드 호제법을 짜보자
-- gcd 는 이미 있는 함수이므로
gcd' :: Int -> Int -> Int  
gcd' a b   
    | b == 0    = a  
    | otherwise = gcd' b (a `mod` b)


gcd'' :: Int -> Int -> Writer [String] Int  
gcd'' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]
        return a
    -- | b == 0 = writer (a,["Finished with " ++ show a])  -- 이렇게도 쓸수 있지만 do 스타일이 가독성이 좀 더 좋은듯
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd'' b (a `mod` b)

testGcd = do
    print $ gcd' 120 16
    print $ runWriter (gcd'' 120 16)
    print $ fst (runWriter (gcd'' 120 16))
    mapM putStrLn $ snd $ runWriter (gcd'' 120 16)

-- 일반 값들을 Writer (monadic value) 로 바꾸고 일반 함수 적용을 >>= (monadic function) 로 바꿔서
-- 로깅 매카니즘을 추가 할수 있다.
-- main = testGcd

-- Writer 에서 list monoid 를 쓸때 특정 상황에서 느릴때가 있다
-- 위에서 짠 gcd 는 아래처럼 연산 방향이 좌->우 인거는 괜찮은데
-- a ++ (b ++ (c ++ (d ++ (e ++ f))))
-- 아래처럼 연산 방향이 우->좌 인거는 매번 우측을 좌측에 ++ 할때마다 좌측부터 새로 만들어야 하니 느리다.
-- (++ 함수는 처리할때 왼쪽 리스트를 처음부터 끝까지 읽은다음에 마지막에 오른쪽걸 붙이기 때문)
-- ((((a ++ b) ++ c) ++ d) ++ e) ++ f

-- gcd 랑 연산순서를 바꿔서 짜보면
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)  -- 재귀를 먼저함
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  -- 그러면 여기가 재귀 풀리면서 반대로 호출됨
        return result

main = mapM putStrLn $ snd $ runWriter (gcdReverse 120 16)

-- 비효율적으로 리스트를 붙이는 문제를 Difference lists 를 이용해 개선해보자.
-- difference list 는 리스트를 하나 받아서 그 앞에다 다른 리스트를 붙이는 동작을 하는 함수!다.
-- 이런 형태 \xs -> [1,2,3] ++ xs , \xs -> [] ++ xs