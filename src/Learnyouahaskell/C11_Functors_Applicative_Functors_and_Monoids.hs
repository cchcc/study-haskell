module Learnyouahaskell.C11_Functors_Applicative_Functors_and_Monoids where

--
-- 11. Functors, Applicative Functors and Monoids  http://learnyouahaskell.com/functors-applicative-functors-and-monoids
--

import Control.Applicative -- ZipList

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter+1) (f x)

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA xs

main = do
    -- Functor 감싸져 있는 것(box)에 매핑을 하는것.(can be mapped over). computational context
    -- kind 는 (* -> *) -> Constraint 타입 파라매터로 * -> * 를 받음

    print $ map (+1) [1,2,3]  -- (a -> b) -> [a] -> [b]
    print $ fmap (+1) [1,2,3]  -- (a -> b) -> f a -> f b  -- f 가 감싸고 있는것
    -- Maybe 나 Either 같이 추가로 타입 파라매터를 받는경우 최종 Functor 에는 타입 파라매터가 한개만 받도록 부분적용된 타입생성자 이용
    print $ fmap (+1) (Just 3)
    -- line <- fmap length getLine  -- IO 도 fmap 가능
    -- print line

    -- -> 이거도 하나의 타입임 (->) t1 t2
    -- Functor 에 (->) 이걸 부분적용하게 되면
    -- fmap :: (a­ -> b) -­> ((­->) r a) -­> ((-­>) r b)
    -- fmap :: (a­ -> b) -­> (r -> a) -­> (r -> b)  이건 (.) 이거와 동일함(function composition)
    -- 즉 Functor 를 그냥 함수에다 적용하면 함수 합성 (.) 이 됨
    print $ fmap (+1) (+100) 1
    -- 커링 관점에서 보면 (a -> b) -> (f a -> f b) -- lifing

    -- functor 를 적용한다는걸 요런식으로 표현함 : functor over number. functor that has numbers in it.

    -- functor 가 되기위한 2가지 법칙
    -- 1. fmap id 적용한거랑 그냥 id 만 한거랑 같아야함. fmap id = id
    print $ fmap id (Just 1)
    print $ id (Just 1)
    -- 2. 함수 합성(.)한거에 fmap 적용한거랑 따로따로 적용해서 합성한거랑 같은 결과가 나와야함. fmap (f . g)  = fmap f . fmap g
    print $ fmap ((+1) . (+1)) (Just 1)
    print $ ((fmap (+1)) . (fmap (+1))) (Just 1)
    -- types act like containers of functions

    print $ id (CJust 0 "hi")
    print $ fmap id (CJust 0 "hi")  -- functor 법칙에 안맞음 1번법칙 X
    -- 사실 functor fmap 은 어떻게든 구현은 가능하겠지만 반드시 functor 법칙을 따르자.
    -- 이유는 그래야 fmap 이 매핑외에 딴짓거리 안함을 보장함으로서 이후 결과를 정확하게 추측이 가능하고 그걸 기준으로 확장구현이 가능하다.
    -- functor 를 다루면서 이 법칙이 잘지켜지고 있는지 감이 안오면 일단 위에 CMaybe 처럼 직접 짜보자.

    -- Applicative Functors
    -- 하스켈의 함수는 기본이 커링, 즉 함수 호출시 파라매터를 덜 넣고 호출이 가능함. 이를 Functor 에 적용해보자면
    -- fmap (+) (Just 3)  -- 이건 Just (+3) 이렇게 됨, 즉 컨텍스트(박스)안에 있는거를 부분 적용된 함수로 만들어 버림
    -- 그러면 Just (+3) 을 꺼내서 Just 5 에 적용하고 싶다면??

    -- class (Functor f) => Applicative f where
    -- pure :: a -> f a     -- f 로 다시 감쌈(박싱)
    -- (<*>) :: f (a -> b) -> f a -> f b   -- 박싱되있는 함수를 꺼내서 다른 파라매터에 적용시킴. 첫 파라매터가 박싱되있는 함수
    print $ Just (+3) <*> Just 5  -- Applicative 를 이용
    print $ pure (+3) <*> Just 5
    -- print $ (Just (+)) <*> Just 5

    -- pure 를 좀더 살펴보면 pure f <*> x 와 fmap f x 는 같음거임
    print $ pure (+) <*> Just 3 <*> Just 5
    print $ fmap (+) (Just 3) <*> Just 5
    print $ (+) <$> Just 3 <*> Just 5  -- 그리고 저기능을 하는 함수가 따로 있음 <$>
    -- pure 는 어떤 값을 하나 받아서 가장 기본(디폴트) 컨텍스트(박스)에 넣는 일을 한다. minimal context
    -- 여기서 말하는 가장 기본 컨텍스트는 그 결과가 '값이 없음' 은 아니다 (ex: Maybe 의 Nothing, List 의 [])

    print $ [ x*y | x <- [2,5,10], y <- [8,10,11]]
    print $ (*) <$> [2,5,10] <*> [8,10,11] -- 위에 list comprehensions 과 같음

    -- k <$> f <*> g 여기서 k,f,g 가 다 함수라고 한다면 이거의 의미는 어떤 값에 f 를 적용할 결과와 g 를 적용한 결과를 k 로 적용하는 함수를 만듬
    print $ (*) <$> (+10) <*> (subtract 10) $ 0  -- 여기서는 감싸고 있는 놈이 (Num a) => a -> b 적용할 놈이 (*)

    print $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  -- (,,) 이거는 원소 3개짜리 튜플을 만들어내는 함수

    -- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c -- 이거는 k <$> f <*> g 이거랑 동일
    print $ (*) <$> (+10) <*> (subtract 10) $ 0
    print $ liftA2 (*) (+10) (subtract 10) $ 0

    -- sequenceA :: (Applicative f) => [f a] -> f [a]  -- 이거는 foldr (:) (pure []) [f a] 이거랑 동일
    print $ (:) <$> (Just 3) <*> (Just [4])
    print $ sequenceA [Just 3, Just 4]

    print $ sequenceA [[1,2],[3,4]]  -- 이거를 쭉 풀어보면 (:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])


    print $ sequenceA' [(+1),(+2)] $ 3  -- 타입은 sequenceA [(+1),(+2)] :: Num a => a -> [a]
    -- 쭉 풀어보면
    print $ (:) <$> (+1) <*> ((:) <$> (+2) <*> sequenceA []) $ 3
    print $ (:) <$> (+1) <*> ((:) <$> (+2) <*> pure []) $ 3
    -- print $ (:) <$> (+1) <*> ((:) <$> (+2) <*> [) $ 3 -- 이거는 왜 안되는지 모르겟네...
