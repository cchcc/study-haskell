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
-- main = print $ return (0,0) >>= landLeft 3 >> return (0,0) >>= landLeft 1 >>= landRight 1

-- Maybe 에 monad (>>=) 를 이용한 함수 체인을 만들면 case 로 분기처리하는거 보다 실패 처리(Nothing) 가 매우 편하당..

-- do notation
-- monadic value 에도 do notaion 이 사용 가능하다
-- 하는 일은 IO 의 do 가 여러 IO action 을 한군데로 모아서 처리 가능 하도록 해준것처럼 
-- monadic value 를 한군데로 모아서 처리 가능 하도록 해준다.

-- 단계별로 생각해보자. 아래와 같은식이 있다고 해보면...
-- Just 3 >>= (\x -> Just (show x ++ "!"))
-- 람다안에 람다로 monad 체인
-- Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))
-- 위에꺼를 함수로 만들어보면
foo :: Maybe String  
foo = Just 3   >>= (\x -> 
      Just "!" >>= (\y -> 
      Just (show x ++ y)))

-- 위에꺼는 람다식을 저렇게 쓰는게 좀 안이쁘다. 이걸 do 로 이용해서 바꾸면..
foo2 :: Maybe String  
foo2 = do  -- do 안에서 모든줄은 monadic value
    x <- Just 3    -- Maybe 에서 값을 꺼냄, <- 이거는 monadic value 가 맞는지를 검사함
    y <- Just "!"  
    Just (show x ++ y)  -- 마지막 줄은 모든 monadic value 의 결과. <- 를 쓸수 없다. 

-- do 는 monadic value 의 체인의 또다른 방법이다. 
-- 체인 1개짜리로 비교해 보면 눈에 좀더 잘들어 온다.
-- Just 9 >>= (\x -> Just (x > 8))
-- 위랑 아래랑 같은거임 : 'x ->' 이거가 'x <- Just 9' 로 바뀜
marySue :: Maybe Bool  
marySue = do   
    x <- Just 9  
    Just (x > 8)

-- 위에 예제 외줄타기 >>= 이거를 do 로 바꿔보자
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start
    _ <- Just (0,0)  -- 이거는 >> 를 사용한것과 같다.
    Just (1,1)
    Nothing
    second <- landRight 2 first  
    landLeft 1 second

-- main = print routine
-- >>= 쓰던 do 를 쓰던 맘대로 하면 되는데 이 외줄타기 예제는 >>= 를 쓰는게 적합한거 같다.
-- 왜냐하면 이전 상태(Pole)를 계속 인풋으로 받아오니 >>= 로 표현하는게 더 깔끔하다.

justH :: Maybe Char  
justH = do  
    -- (x:xs) <- Just "hello"  
    (x:xs) <- Just ""  
    -- (x:xs) <- Nothing
    return x 

-- main = print justH
-- 패턴매칭하다 맞는 패턴이 없으면 런타임 오류가 나는데 do 안에서 패턴매칭하다가 패턴이 없으면 fail 을 호출한다
main = print $ fail "no matching" :: Maybe String  -- Maybe 의 fail 은 그냥 Nothing


-- list monad
-- instance Monad [] where  
--     return x = [x]  
--     xs >>= f = concat (map f xs)  
--     fail _ = [] 
