module Learnyouahaskell.C11_Functors_Applicative_Functors_and_Monoids where

--
-- 11. Functors, Applicative Functors and Monoids  http://learnyouahaskell.com/functors-applicative-functors-and-monoids
--

import Control.Applicative -- ZipList, lift2, sequenceA
import qualified Data.Foldable as F
-- import Data.Monoid

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter+1) (f x)

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA xs

newtype ZipList' a = ZipList' {getZipList' :: [a]} deriving (Show)

part1::IO() -- functor, applicative functor
part1 = do
    -- Functor 감싸져 있는 것(box)에 매핑을 하는것.(can be mapped over). computational context
    -- kind 는 (* -> *) -> Constraint 타입 파라매터로 * -> * 를 받음

    print $ map (+1) [1,2,3]  -- (a -> b) -> [a] -> [b]
    print $ fmap (+1) [1,2,3]  -- (a -> b) -> f a -> f b  -- f 가 감싸고 있는것
    -- Maybe 나 Either 같이 추가로 타입 파라매터를 받는경우 최종 Functor 에는 타입 파라매터가 한개만 받도록 부분적용된 타입생성자 이용
    print $ fmap (+1) (Just 3)
    -- line <- fmap length getLine  -- IO 도 fmap 가능
    -- print line

    -- 일반함수 r -> a 이거를 다시 써보면 (->) r a 이거임. 여기서 앞에 (->) r 이거만 따로 떼서 감싸져 있는것(박스)로 취급 가능함..!
    -- fmap :: (a­ -> b) -­> ( (->) r a) -­> ( (-­>) r b)
    -- fmap :: (a­ -> b) -­> (r -> a) -­> (r -> b) 위에걸 풀어보면, 이렇게 되는데 이건 (.) 와 동일함(function composition)
    -- 즉 Functor 를 그냥 함수에다 적용하면 함수 합성 (.) 이 됨 (구현은 fmap f g = (\x -> f (g x)))
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

    -- pure 는 어떤 값을 하나 받아서 가장 기본(디폴트) 컨텍스트(박스)에 넣는 일을 한다. minimal context
    -- 여기서 말하는 가장 기본 컨텍스트는 그 결과가 '값이 없음' 은 아니다 (ex: Maybe 의 Nothing, List 의 [])
    print $ pure (+) <*> Just 3 <*> Just 5
    print $ fmap (+) (Just 3) <*> Just 5

    -- fmap 의 축약형 infix 함수로서 <$> 가 있음 (<$> 의 구현 : f <$> x = fmap f x)
    -- f a b c 비스무리하게 f 를 박싱된것에 연쇄적으로 적용한다는 의미로 f <$> a <*> b <*> c 요런식으로 표현 가능
    print $ (+) <$> Just 3 <*> Just 5
    
    -- [] 의 Applicative 구현은 fs <*> xs = [f x | f <- fs, x <- xs] 임
    print $ [ x*y | x <- [2,5,10], y <- [8,10,11]]
    print $ (*) <$> [2,5,10] <*> [8,10,11] -- 위에 list comprehensions 과 같음
    print $ filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
    print $ [(+),(*)] <*> [1,2] <*> [3,4]

    -- IO 의 Applicative 구현은 <- 이걸로 꺼내서 f 적용후 return 으로 감쌈
    -- a <*> b = do  
    --   f <- a  
    --   x <- b  
    --   return (f x)
    -- (++) <$> getLine <*> getLine

    -- (->) r 의 Applicative 구현은 f <*> g = \x -> f x (g x)
    print $ (+) <$> (+3) <*> (*100) $ 5
    -- k <$> f <*> g 여기서 k,f,g 가 다 함수라고 한다면 이거의 의미는 어떤 값에 f 를 적용한 결과와 g 를 적용한 결과를 k 로 적용하는 함수를 만듬
    print $ (*) <$> (+10) <*> (subtract 10) $ 0  -- 감싸고 있는 놈이 (Num a) => a -> b 적용할 놈이 (*)
    -- 대충 (*) (\x -> x+10) (\x -> x-10) 이런형태에 x 에 0 이 들어감. (*) -10 10 즉, -10 * 10 
    print $ (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5

    -- [] 의 Applicative 구현 <*> 는 앞 파라매터의 함수를 뒤 파라매터의 모든 값에 적용하는데
    -- 이거를 앞 첫번째는 뒤 첫번째 앞 두번째는 뒤 두번째 처럼 적용하고 싶으면 ZipList 를 사용(zipWith 함수와 비슷함)
    print $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
    print $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  -- (,,) 이거는 원소 3개짜리 튜플을 만들어내는 함수

    -- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c -- 이거는 f <$> a <*> b 이거랑 동일
    print $ liftA2 (+) (Just 1) (Just 2)  -- (+) <$> Just 3 <*> Just 5
    print $ (*) <$> (+10) <*> (subtract 10) $ 0
    print $ liftA2 (*) (+10) (subtract 10) $ 0

    -- sequenceA :: (Applicative f) => [f a] -> f [a]
    -- sequenceA [] = pure []
    -- sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
    print $ sequenceA [Just 3, Just 4]
    print $ sequenceA [Just 3, Nothing, Just 4]
    
    -- sequenceA 를 함수들의 리스트에다 사용할때, 똑같은 인풋 을 모든 함수에 적용해보는데 쓰면 유용함
    print $ sequenceA [(+1),(+2)] 3  -- 타입은 sequenceA [(+1),(+2)] :: Num a => a -> [a]
    -- 쭉 풀어보면
    print $ (:) <$> (+1) <*> ((:) <$> (+2) <*> sequenceA []) $ 3
    print $ (:) <$> (+1) <*> ((:) <$> (+2) <*> pure []) $ 3
    -- print $ (:) <$> (+1) <*> ((:) <$> (+2) <*> []) $ 3 -- 이거는 왜 안되는지 모르겟네...
    
    -- sequenceA 를 리스트의 리스트([[]]) 에 사용하면 조합가능한 모든 원소들이 나옴
    print $ sequenceA [[1,2],[3,4]]  -- 이거를 쭉 풀어보면 (:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])
    print $ sequenceA [[1,2],[3,4],[5,6],[7,8]]

    -- sequenceA 를 IO 에 쓰면.. 그냥 [IO a] 가 IO [a] 됨
    -- sequenceA [getLine, getLine]

    -- applicative functor law
    -- pure f <*> x = fmap f x
    -- pure id <*> v = v
    -- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- pure f <*> pure x = pure (f x)
    -- u <*> pure y = pure ($ y) <*> u

-- main = part1



-- newtype
-- data 를 정의할때 만약 단순히 타입을 한번 래핑하는 용도이면 data 보다 newtype 을 쓰는게 성능면에서 더 낫다.
-- newtype 을 쓰려면 한개의 생성자에 한개의 필드만 사용가능하다
-- newtype Race = Human | Elf | Orc | Goblin -- 이건 생성자가 4개라 안됨
part2::IO() -- newtype
part2 = do
    -- ZipList 를 살펴보면 ZipList 는 [a] -> ZipList, getZipList 는 ZipList -> [a] 이런식으로 변환하는 개념으로 생각
    -- 이런스타일로 하는게 newtype 의 관례인듯...
    print $ ZipList' [1,2,3]  -- newtype ZipList' a = ZipList' {getZipList' :: [a]}
    print $ getZipList' (ZipList' [1,2,3])

-- main = part2

-- newtype 용도 - type class instances 를 만들때 사용
-- 만약 Functor (a,b) 이런식으로 튜플을 쓰고 싶은데 Functor 에 파라매터는 한개라 안됨
-- 이럴땐 newtype 으로 래핑하는 단순한 새로운 타입을 만들어서 적용이 가능하다
newtype Pair a b = Pair { getPair::(b, a) } deriving (Show)

instance Functor (Pair a) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- main = print $ fmap (+1) (Pair (1, 'a'))

-- newtype 특징 - 패턴매칭에서 lazy 처리 가능
-- 이게 안되는 이유는 data 는 (CoolBool _) 패턴매칭이 되는지 보기위해 생성자 호출하면서 undefined 를 호출함 (파람으로 들어오는 undefined 까지 호출해봐야 패턴을 알수 있음)
-- data CoolBool = CoolBool { getCoolBool :: Bool }
-- 이게 되는 이유는 newtype 의 생성자와 필드는 한개로 고정됬음을 이미 알고 있음으로 걍 ㄱ
newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello" -- newtype 값을 패턴매칭하면 값을 꺼내는게 아니라 직접적으로 교환하는

-- main = print $ helloMe undefined

-- type : 이건 그냥 동일한 타입인데 이름만 하나 더 생기는것. 가독성이나 개념상 좀더 명확하게 하고싶으면 사용
-- newtype : 기존 타입을 새로 타입을 만들어서 typeclass instance 를 만들고 싶다면(혹은 생성자,필드가 한개씩 밖에 없는거면) 이걸로 사용
-- data : 그외 다른 일반적인 경우


-- monoids

-- binary function(이항 연산) : 인풋이 2개를 받아 결과 하나를 리턴하는 함수. ex: *, ++
-- associativity(결합 법칙) : 바이너리 함수를 연속으로 적용할때 우선순위 연산에 상관없이 결과 같은경우. ex: 1+(2+3) 이거나 (1+2)+3 이거는 결과가같으므르 이런경우 성립
-- identity value(항등원) : 바이너리 함수에 특정값 과 다른 어떤값을 적용했을때 그 결과가 그대로 나오게되는 특정값. ex: 1*2=2, 1*10=10, *의 identity value 는 1

-- class Monoid m where -- 여기서 m 은 concreate type 이다
--     mempty :: m   -- identity value
--     mappend :: m -> m -> m  -- associative binary function 여기에 뭘 할건지 구현
--     mconcat :: [m] -> m
--     mconcat = foldr mappend mempty     -- 리스트를 위에껄로 리듀스

-- monoid law
-- mempty `mappend` x = x  -- identity value
-- x `mappend` mempty = x  -- identity value
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)  -- associativity

-- main = print $ mconcat [[1],[2],[3]]
-- main = print $ mconcat ["a","b","c"]

-- Data.Monoid

-- 리스트 원소들을 다 곱해보기
newtype Product a =  Product { getProduct :: a } deriving (Eq, Ord, Read, Show, Bounded)

-- 아래의 Monoid 구현 예제 코드는 에러남 : Could not deduce (Semigroup (Product a)) ...
-- instance Num a => Monoid (Product a) where  
  -- mempty = Product 1  
  -- Product x `mappend` Product y = Product (x * y) 

-- 좀 찾아보니 4.11.1.0 부터 Monoid 는 Semigroup 의 하위 클래스라 Monoid 를 구현 하려면 Semigorup 부터
-- 구현을 해줘야 함
-- http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Monoid.html
-- Semigroup 은 (<>) 함수 하나를 가지는 class. 구현체는 결합법칙이 성립 되야함(associative binary function)
instance (Num a) => Semigroup (Product a) where
  Product x <> Product y = Product (x * y) -- 예제의 mappend 구현
  
instance (Num a) => Monoid (Product a) where
  mempty = Product 1
  -- mappend = (<>) -- 이거 구현 생략해도 됨

-- main = print $ getProduct . mconcat . map Product $ [3,4,2]

-- 리스트 원소중 하나라도 True 이면 Ture
newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Any where
  (<>) (Any x) (Any y) = Any (x || y)

instance Monoid Any where
  mempty = Any False

-- main = print $ getAny . mconcat . map Any $ [True, False]

-- 리스트 원소중 모두 True 여야 Ture
newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup All where
  (<>) (All x) (All y) = All (x && y)

instance Monoid All where
  mempty = All True

-- main = print $ getAll . mconcat . map All $ [True, False]

-- 리스트 원소중 가장 첫번째로 존재하는 값
newtype First a = First { getFirst :: Maybe a } deriving (Eq, Ord, Read, Show)

instance Semigroup (First a) where
  First (Just x) <> _ = First (Just x)  
  First Nothing <> x = x 

instance Monoid (First a) where  
  mempty = First Nothing  
  -- First (Just x) `mappend` _ = First (Just x)  
  -- First Nothing `mappend` x = x  

-- main = print $ getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]

-- 반대로 Last 도 있음
-- main = print $ getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]

-- fold data structures
-- import qualified Data.Foldable as F
-- 위모듈에 보면 Foldable 이라는 type class 가 있음.
-- Data.Foldable 모듈에 foldr foldl 등이 잇는데 이게 prelude 함수의 foldr foldl 과 다른점은 리스트가 아니라는거
-- foldr :: (a -> b -> b) -> b -> [a] -> b  -- prelude 꺼
-- foldr :: (Data.Foldable t) => (a -> b -> b) -> b -> t a -> b  -- Data.Foldable 꺼

-- main = print $ F.foldl (+) 2 (Just 9)  -- Maybe 는 Foldable 임

-- 트리에다가 monoid 를 이용해서 fold 를 해보자
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- 아래 foldMap 을 구현하면 foldr foldl 이 사용가능함
-- Data.Foldable.foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- m 이 Monoid 즉 첫번째 파람인 mapping 함수는 Monoid 타입을 리턴해야됨

-- foldMap 구현할때 f 를 어디다 적용하고 그 결과인 monoid 값을 어떻게 합칠건지를 생각해보자
instance F.Foldable Tree where  
  foldMap f Empty = mempty  
  foldMap f (Node x l r) = F.foldMap f l `mappend`  
                           f x           `mappend`  
                           F.foldMap f r

testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )

-- main = print $ F.foldl (+) 0 testTree
-- main = print $ F.foldMap (\x -> [x]) testTree

