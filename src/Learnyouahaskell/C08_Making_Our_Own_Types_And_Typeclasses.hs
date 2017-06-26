module Learnyouahaskell.C08_Making_Our_Own_Types_And_Typeclasses where

--
-- 08. Making Or Own Types And Typeclasses  http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--

import qualified Data.Map as DM

-- Algebraic data type, data declaration
-- = 뒤로 나오는 부분을 value constructors 라 함. 그냥 일반 함수이며 밸류를 받아 뭔가 새로운 밸류를 리턴
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving(Show)

-- main = print $ Circle 1.0 1.0 1.0

-- 면적구하기. 이런식으로 패턴매칭이 가능함
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle ltx lty rbx rby) = abs(rbx - ltx) * abs(rby - lty)

-- main = print $ surface (Circle 1 1 2)
-- main = print $ surface (Rectangle 0 10 10 0)

-- module 을 만들때 value constructors 를 숨기면 구현이 숨고 좀더 추상화된다.


-- Record syntax
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
-- value constructors 에 필드명같은거를 추가 가능, 사실은 아래의 함수
-- company :: Car -> String
-- company (Car c _ _) = c
-- main = let car = Car "tesla" "sss" 3000 in print $ (show car) ++ company car -- 필드이름도 출력됨

-- type constructors 타입을 파라매터로 받아 타입을 리턴
data Maybe a = Nothing | Just a   -- a 가 type parameter

-- data (Ord k) => Map k v = ... -- 타입 파라매터 k를 특정 타입으로 제한한때 이렇게 씀
-- 근데 data 를 선언할때 타입 제한을 거는거 보다 해당 data 를 최종 사용하는 함수에다 타입제한을 걸어서 사용하는게 좀더 의미가 맞다.

data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t  -- 함수에 Num 제약
(Vector a b c) `vplus` (Vector a2 b2 c2) = Vector (a+a2) (b+b2) (c+c2)

-- main = print $ (Vector 1 1 1) `vplus` (Vector 2 2 2)
-- main = print $ vplus (Vector 2 2 2) (Vector 1 1 1)

-- Derived instances
-- typeclass 는 어떤 행동을 정의한 것이고 이거는 상속이 가능함. data 의 모든 필드가 해당 typeclass 의 행동이 가능해야함.
-- Show : 문자열로 변경가능, Read : 문자열에서 어떤 데이터 타입으로 변경가능
-- Ord 를 상속할경우 생성자의 선언 순서가 뒤로 갈수록 큰거임.
data Bool = False | True deriving (Eq, Ord)  -- False 와 True 를 비교해보면 True 가 크다.

-- Type synonym
-- data 는 새로운 타입을 만드는것이고 type 은 기존타입에 이름을 하나 더 지어주는것.
-- 한눈에 안들어오는 로우 타입을 줄줄이 쓰는거보다 좀 더 의미가 명확한걸로 지어서 가독성을 높이자.
type PhoneBook = [(String, String)]
type PhoneBook2 k v = (k,v) -- 제너럴하게도 사용가능
-- 부분적용 함수처럼 부분적용 타입(!)도 가능함

type IntMap = DM.Map Int -- type IntMap v = DM.Map Int v   v를 생략

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- 에러가 났을때 에러에 관한 추가적인 정보를 넣고 싶을때 사용. Left 가 에러, Right 가 성공

-- Recursive data structures. data 선언중에 같은 data를 선언가능
data List a = Empty | Cons a (List a) deriving (Eq, Ord, Read, Show)
-- main = print $ 1 `Cons` (2 `Cons` Empty)

-- fixity declaration. infix, infixr, infixl  특수문자 연산자의 결합 방향과 결합정도 (0~9) 를 선언함.
infixr 5 :! -- 우측 결합. 9로 갈수록 같은 infixr 연산자끼리의 우선순위가 올라감
data List2 a = Empty2 | a :! (List2 a) deriving (Eq, Ord, Read, Show)  -- 우측 결합은 :! 를 a 앞에
-- main = print $ 1 :! 2 :! Empty2

-- typeclass. type 을 만들어내기 위한 기본 틀.  typeclass 를 인스턴스화 하면 type instance 가 됨.
class YesNo t where
  yesno :: t -> Prelude.Bool  -- 얘는 함수 선언만 하고 구현은 instance 에서
  yes :: t -> Prelude.Bool  -- 함수 선언과 구현도 같이
  yes _ = Prelude.True

instance YesNo [a] where  -- [a] 에 YesNo 타입클래스를 구현함.
  yesno [] = Prelude.False
  yesno _ = Prelude.True
  -- yes _ = Prelude.False  -- typeclass 에서 구현이 있더라로 instance 에서 구현가능

-- 클래스 선언시 타입 제약 : 개념상 서브클래스를 만드는 방법임
-- class (Eq a) => Num a where
-- 인스턴스 선언시 타입제약 : Maybe 가 같은지 비교하려면 a 도 비교가 가능해야함. a 의 필수적인 요소를 명시.
-- instance (Eq a) => Eq (Maybe a) where

-- main = do
  -- print $ yesno ""
  -- print $ yesno "1"
  -- print $ yes ""


-- functor typeclass.
class Funct f where  -- class Functor (f :: * -> *) where
  fmap1 :: (a -> b) -> f a -> f b  -- f 의 타입은 함수(생성자)이고 f a 는 a 에 f 를 적용한 거임
-- (a -> b) 는 그냥 함수 파라매터.   f a 는 (타입 변수 하나를 받는)타입 생성자

instance Funct [] where   -- [] 는 type variable 하나를 받는 리스트 생성자, 즉 함수임
  -- fmap1 f [] = []  -- #1
  -- fmap1 f (x:xs) = (f x):fmap f xs
  -- fmap1 f xs = foldr (\x acc -> (f x):acc) [] xs  -- #2
  fmap1 = map  -- #3

-- main = print $ fmap1 (+1) [1,2,3]

instance Funct (Either a) where  -- f 는 타입 파라매터 하나만 받는놈이라 이경우에는 부분적용 을 이용함
  fmap1 f (Right x) = Right (f x)
  fmap1 f (Left x) = Left x

-- kind.
-- * 의 의미는 다른 밸류나 타입 파라매터를 인풋으로 받지않는 실체화된(concrete) 타입을 말함
-- 생소한 개념인데 Int type has a kind of * 이런식으로 표현한다.
-- type constructor 의 타입이 뭔지 표현할때 사용
-- Int 는 * 인 kind 를 가짐
-- Maybe a 는 * -> * 인 kind 를 가짐
-- Maybe Int 는 * 인 kind 를 가짐 (타입 파라매터를 Int 로 실체화)

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank{frankField::b a} deriving(Show)

instance Tofu Frank where
  tofu a = Frank a

-- 위에꺼 한눈에 이해가 잘안가는데 하나씩 뜯어보자.
-- j a : j 는 타입 파람 하나를 받음, a 는 *, kind 는 * -> *
-- t a j : t 는 타입 파람 2개를 받음, a 와 j 는 위에꺼 그대로 따라감,  kind 는 * -> (*->*) -> *
-- Frank : 타입 파람 2개를 받는 타입 생성자임. 타입 변수 2개중 b 는 또다시 타입변수 1개를 받는 타입 생성자, 즉 kind 는 * -> (*->*) -> *
--                                                                                               a       b
-- main = print (tofu [1] :: Frank Int [])
