module Learnyouahaskell.C11_Functors_Applicative_Functors_and_Monoids where

--
-- 11. Functors, Applicative Functors and Monoids  http://learnyouahaskell.com/functors-applicative-functors-and-monoids
--

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter+1) (f x)


main = do
    -- Functor 감싸져 있는 것(box)에 매핑을 하는것.(can be mapped over). computational context

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
