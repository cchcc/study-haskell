module Learnyouahaskell.C12_A_Fistful_of_Monads where

--
-- 12. A Fistful of Monads  http://learnyouahaskell.com/a-fistful-of-monads
--

-- functor
-- fmap :: (Functor f) => (a -> b) -> f a -> f b

-- applicative functor : functor 의 확장
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  -- f 는 functor 이기도 함
-- applicative value : 부분 적용된 함수(함수도 값으로 취급)
-- (*) <$> Just 2 <*> Just 8  -- 여기서 Just (*2) 의 (*2) 를 말함

-- monad : applicative functor 의 확장
-- 감싸져 있는것(m a)에 일반 값을 감싸져 있는것으로 바꾸는 어떤 함수(a -> m b) 를 적용해서 감싸져 있는 변경된 값으로(m b) 만들고 싶다
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b  -- m 은 applicative functor 이기도 함
-- 이때 (>>=) 를 'bind' 라고 부른다
-- applicative functor 가 (>>=) 가 되면(지원하면) 걔는 monad
-- m a 를 monadic value 라고 하기도 함

-- Maybe 로 개념만 짜보자면
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x

-- main = print $ Just 3 `applyMaybe` \x -> Just (x+1)
-- main = print $ Nothing `applyMaybe` \x -> Just (x+1)
-- main = print $ Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing

-- 여기까지만 보면 뭐 그닥... Monad type class 를 살펴보자
-- class (Applicative m) => Monad (m :: * -> *) where
--     return :: a -> m a   -- 이건 applicative 의 pure 랑 동일
--     (>>=) :: m a -> (a -> m b) -> m b  
--     (>>) :: m a -> m b -> m b  -- 이건 뒤에 외줄타기에서 설명...
--     x >> y = x >>= \_ -> y  
--     fail :: String -> m a  -- 뒤에서..
--     fail msg = error msg

-- return 이랑 (>>=) 이거 두가지는 직접구현해야 함

-- 그러면 이제 Maybe 의 Monad 구현을 보자
-- instance Monad Maybe where  
--     return x = Just x  
--     Nothing >>= f = Nothing  
--     Just x >>= f = f x  
--     fail _ = Nothing

-- main = print $ pure "WHAT" :: Maybe String
-- main = print $ return "WHAT" :: Maybe String  -- pure 랑 동일
-- main = print $ Just 9 >>= \x -> return (x*10)  -- return 으로 감쌈
-- main = print $ Nothing >>= \x -> return (x*10)

-- Monad Maybe 의 응용 예제...
-- 장대를 들고 외줄타기하는 사람이 있음
-- 장대의 왼쪽, 오른쪽에는 새가 앉을수가 있는데 왼/오 에 앉는 새들 수의 차이가 4이상 나면 사람이 줄에서 떨어짐

type Birds = Int
type Pole = (Birds, Birds)

landLeft::Birds -> Pole -> Maybe Pole
landLeft b (l,r) = let dist = abs((l+b) - r) in if dist < 4 then Just (l+b, r) else Nothing
-- 파라매터 순서가 Birds 가 먼저오는 이유는 부분적용(landLeft 3)을 통해 (>>=) 에서 함수 (a -> m a) 타입을 맞춰주기 위함

landRight::Birds -> Pole -> Maybe Pole
landRight b (l,r) = let dist = abs(l - (r+b)) in if dist < 4 then Just (l, r+b) else Nothing

-- 그리고.. 사람이 외줄타다가 바나나를 밟아 넘어질수 있음 ㅋ
banana::Pole -> Maybe Pole
banana _ = Nothing

walkLine1 = do
  print $ landLeft 3 (0,0)
  print $ landRight 3 (0,0)
  print $ landLeft 4 (0,0)
  print $ landRight 4 (0,0)
  -- monad 이용
  print $ return (0,0) >>= landLeft 3   -- landLeft 3 의 타입은 Pole -> Maybe Pole
  print $ return (0,0) >>= landRight 3
  print $ return (0,0) >>= landLeft 4
  print $ return (0,0) >>= landRight 4
  -- >>= 를 이용해서 아래처럼 연속 호출할수 있음
  print $ return (0,0) >>= landLeft 3 >>= landRight 3 >>= landLeft (-1) >>= landRight (-2)
  -- 연속 호출하는 중간에 사람이 떨어져도 끝까지 호출되는 것에는 문제 없음!
  print $ return (0,0) >>= landLeft 3 >>= landRight 3 >>= landLeft 4 >>= landRight 1
  print $ return (0,0) >>= landLeft 3 >>= banana >>= landLeft 4 >>= landRight 1
  
-- main = walkLine1

-- banana 를 살펴보면 (>>) 임.
-- (>>) :: (Monad m) => m a -> m b -> m b  -- 감싸져 있는것(m a), 리턴하고 싶은것(m b)
-- m >> n = m >>= \_ -> n  -- >>= 로 m 과 상관없이 그냥 n 을 리턴하는 함수(\_ -> n) 를 이용해 (m b) 를 만듬

-- >>= banana 를 >> Nothing 으로 대체
-- main = print $ return (0,0) >>= landLeft 3 >> Nothing >>= landLeft 4 >>= landRight 1
-- monad 체인중에 중간에 그냥 특정값 Just (0,0) 으로 바꿈
main = print $ return (0,0) >>= landLeft 3 >> return (0,0) >>= landLeft 1 >>= landRight 1

-- Maybe 에 monad (>>=) 를 이용한 함수 체인을 만들면 실패 처리(Nothing) 가 매우 편하당..