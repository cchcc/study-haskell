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

main = do
    print $ runWriter (return 3 :: Writer String Int)
    print $ runWriter (return 3 :: Writer (Sum Int) Int)
-- Writer 는 fail 구현이 없어서 do 에서 패턴매칭에 실패하면 error 가 호출된다
