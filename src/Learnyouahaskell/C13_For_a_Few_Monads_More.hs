module Learnyouahaskell.C13_For_a_Few_Monads_More where

--
-- 13. For a Few Monads More  http://learnyouahaskell.com/for-a-few-monads-more
--

import Data.Monoid  -- Sum
import Control.Monad.Writer
import Control.Monad.State
import System.Random

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

-- main = mapM putStrLn $ snd $ runWriter (gcdReverse 120 16)

-- 비효율적으로 리스트를 붙이는 문제를 Difference lists 를 이용해 개선해보자.
-- difference list 는 리스트를 하나 받아서 그 앞에다 다른 리스트를 붙이는 동작을 하는 함수!다.
-- 이런 형태 \xs -> [1,2,3] ++ xs , \xs -> [] ++ xs
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  -- prepend! 
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    -- (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcdReverse' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

-- main = mapM putStrLn $ fromDiffList $ snd $ runWriter (gcdReverse' 120 16)

-- main = print $ fromDiffList $ toDiffList ["finish~"] <> toDiffList ["16~"] <> toDiffList ["120~"]
-- 이게 한번에 이해가 안되는데 재귀 풀리는 순서대로 toDiifList 적용을 하나씩 하나씩 풀어서 써보자
-- toDiffList ["Finish~"] <> toDiffList ["16~"] <> toDiffList ["120~"]
-- 1: "Finish~"++ <>  "16~"++ <> "120~"++ []
-- 2: "Finish~" ++ ("16~"++ ("120~" ++ ([])))
-- 아래 글도 참고
-- https://wiki.haskell.org/Difference_list

-- 아래 처럼 일반 리스트와 DiffList 연산을 세는 함수를 만들어서 비교해보면 뭐가더 빠른지 볼수 있다
finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])

finalCountDown' :: Int -> Writer [String] ()  
finalCountDown' 0 = do  
    tell ["0"]  
finalCountDown' x = do  
    finalCountDown' (x-1)  
    tell [show x]

-- main = mapM putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000
-- main = mapM putStrLn . snd . runWriter $ finalCountDown' 500000

-- Reader Monad(function monad)
-- (->) 는 applicative 임. 그럼 얘를 monad 로 만들어보자면
-- instance Monad ((->) r) where  
--     return x = \_ -> x  -- pure 랑 같은거
--     h >>= f = \w -> f (h w) w   -- 이걸 차근차근 뜯어보자
-- 일단 결과인 monadic value 는 함수가 되야하니 람다. h 에서 값을 꺼내 f 를 적용해야 되는데...
-- h 는 분리가 안되는 함수이므로 (h w) 로 f 가 적용가능한 값을 먼저 만들어서 거기다 f 를 적용
-- f 를 적용 하면, f (h w), 다시 함수가 나오므로 이걸 w 에 적용, 그리고 그걸 적용하는 람다가 결과

addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)

addStuff' :: Int -> Int  
addStuff' x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b

-- main = do
    -- print $ addStuff 3  -- 이건 applicative 랑 같다
    -- print $ (+) <$> (*2) <*> (+10) $ 3
-- 각 함수들이 하나의 공통 소스를 이용해서 적용되므로 reader monad 라고도 불린다
-- 어떤 많은 함수들이 있는데 공통으로 적용될 하나의 파라매터가 빠져있는 상태. 
-- 이걸 reader monad 를 이용해 최종 값을 뽑아보고, >>= 구현을 통해서 작 동작하는지를 확인해볼수 있다.

-- State Monad
-- 다른 언어에서 대입문 x = 5 같이 상태를 가지도록 계산하는 식을 함수형으로 생각해 보자면
-- 이전 상태(x)를 파라매터로 받고 상태를 가지는 계산을 한뒤 그 결과(5)와 새로운 상태(x)를 돌려주는 함수로 생각해볼수 있다
-- 이를 타입으로 만들어 보면 아래와 같다
-- s -> (a,s)  -- s 는 상태, a 는 상태 계산 결과

-- 리스트로 pop 과 push 를 하는 스택을 하나 만들어보자
type Stack = [Int]
  
pop :: Stack -> (Int,Stack)
pop (x:xs) = (x,xs)
  
push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs)   -- 결과가 필요 없더라도 () 를 반환

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((),newStack1) = push 3 stack
    (a ,newStack2) = pop newStack1
    in pop newStack2

-- main = print $ stackManip [5,8,2,1]

-- newtype State s a = State { runState :: s -> (a,s) }

-- instance Monad (State s) where  
--     return x = State $ \s -> (x,s)   -- 상태를 가지는 것에 대한 최소의 컨텍스트. 항상 이거부터 생각하자
--     (State h) >>= f = State $ \s -> let (a, newState) = h s  -- 일단 일반값을 뽑아내기 위해 기존 h 를 상태 s 에 적용함
--                                                              -- 타입이 함수인 모나드들은 값을 추출하기 위해 함수적용부터 하는듯
--                                         (State g) = f a  -- 그리고 그결과에 f 를 적용(f 의 결과 타입은 State)
--                                     in  g newState  -- 그리고 g 을 newState 에 적용해서 State 타입을 만듬

-- 위에걸 State monad 로 다시 짜보면.. 에러나는데 State 감싸줄때 소문자 s 로 변경해야한다
pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x,xs)  -- 결과 타입이 State 이므로 State 로 감싸준다
  
push' :: Int -> State Stack ()
push' a = state $ \xs -> ((),a:xs)

-- 그리고 위에 함수를 do 를 이용해 아래처럼 이쁘게 쓸수 있다
stackManip' :: State Stack Int
stackManip' = do
    push' 3
    -- a <- pop'  -- a 를 안쓰니 굳이 바인딩 할필요는 없다
    pop'
    pop'

-- main = print $ runState stackManip' [5,8,2,1]

-- MonadState 라는 type class 가 있음. 그 맴버 함수가
-- get = State $ \s -> (s,s)  -- 현재 상태를 그대로 State 타입으로 결과냄
-- put newState = State $ \s -> ((),newState)  -- 기존 상태를 버리고 새로운 상태를 받아서 State 타입으로 결과냄

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

-- main = print $ runState stackyStack [1,2,3,1]

-- State 의 >>= 타입을 살펴보면
-- (>>=) :: State s a -> (a -> State s b) -> State s b
-- 잘보면 a 를 f 적용후 b 로 바꿔주는거고 이경우 monad 는 State s

-- 이전의 랜덤 문제를 State monad 로 풀어보자. 일단 random 타입을 살펴보면
-- random :: (RandomGen g, Random a) => g -> (a, g)  -- 딱 State type 임

-- random 을 State monad 로 만드는 함수를 만들고
randomSt :: (RandomGen g, Random a) => State g a  
randomSt = state random 

-- 아래처럼 쓸수 있다
threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)

-- main = print $ runState threeCoins (mkStdGen 100)

-- Either e a 를 살펴보자. Either 는 실패경우 특정 값을 넣어줄수 있는 Maybe 의 강화판이다.
-- 따라서 실패 가능성의 컨텍스트를 가질수 있다 보고 (Either e) 로 묶어 monad 를 만들수 있다.
-- instance (Error e) => Monad (Either e) where  
--     return x = Right x   
--     Right x >>= f = f x   -- Maybe 랑 동일
--     Left err >>= f = Left err   -- 실패인 경우에는 그냥 Left
--     fail msg = Left (strMsg msg)

testEither = do
    print $ Left "boom" >>= \x -> return (x+1)
    print $ Right 100 >>= \x -> Left "no way!" :: Either String Int  -- 엥 이거도 타입 명시해줘야 됨..?
    -- print $ Right 3 >>= \x -> return (x + 100) -- 이거는 수식에서 e 의 타입을 알수 없으므로 타입 명시가 필요하다
    print $ Right 3 >>= \x -> return (x + 100) :: Either String Int
    
-- main = testEither

-- useful monadic functions

-- liftM 은 Functor 의 fmap 과 동일한데 대상이 monadic value 이다.
-- liftM :: (Monad m) => (a -> b) -> m a -> m b  
-- liftM f m = m >>= (\x -> return (f x))   -- 일반값에 f 를 적용하고 return 적용

testLiftM = do
    print $ liftM (+1) (Just 1)
    print $ liftM (+1) [1,2,3]
    print $ runState (liftM (+1) pop') [1,2,3]  -- pop' 은 State, State 는 함수

-- main = testLiftM

-- ap 는 Applicative 의 <*> 와 동일하다
-- ap :: (Monad m) => m (a -> b) -> m a -> m b  
-- ap mf m = do  
--     f <- mf    -- 이걸 꺼내면 함수다(a -> b)
--     x <- m  
--     return (f x)

-- 그리고 liftM2 는 LiftA2 와 같다 (liftM3, liftM4, liftM5)

testAp = do
    print $ ap (Just (+1)) (Just 1)
    print $ ap [(+1),(subtract 1)] [3,4]

-- main = testAp

-- 결론은 monad 는 functor 나 applicative 의 기능을 동일하게 쓸수있다

-- join 은 monad 안에 monad 를 flatten 시켜주는 함수
-- join :: (Monad m) => m (m a) -> m a  
-- join mm = do  
--     m <- mm  -- monad 를 꺼내서
--     m  -- return 으로 감싸지 않고 그대로 결과 처리

testJoin = do
    print $ join [[1,2,3],[4,5,6]]  -- flatten 시켜주는 함수 concat 와 같다
    print $ [[1,2,3],[4,5,6]] >>= (\x -> x)  -- 함수가 감싸지 않고 그대로 결과처리
    print $ join (fmap (\x -> x) [[1,2,3],[4,5,6]])  -- m >>= f 와 join (fmap f m) 이거는 같다
    -- print $ join [1,2,3]  -- 근데 이건 안됨
    print $ join (Just (Just 9))
    print $ join (Just (Nothing :: Maybe Int))  -- Maybe 의 경우 Nothing 이 껴있으면 뭐가 됬던 Nothing
    -- print $ join (Nothing::Maybe Int)
    -- print $ join Nothing -- 이거 ghci 는 되는데 컴파일로는 안되네..
    print $ runWriter $ join (writer (writer (1,"aaa"),"bbb"))
    print $ runWriter (writer (writer (1,"aaa"),"bbb") >>= (\x -> x))
    print $ runState (join (state $ \s -> (push' 10,1:2:s))) [0,0,0]
    print $ runState ((state $ \s -> (push' 10,1:2:s)) >>= (\x -> x)) [0,0,0]

-- main = testJoin

-- filterM
-- predicate 가 monadic function. 리스트의 각 원소가 True 인지 False 인지 판단하면서 추가적으로 컨텍스트를 껴넣는 연산이 가능하다.
-- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a] 

keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False

-- main = mapM putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

-- filterM 을 직접 짜보면..
filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a] 
filterM' _ [] = return []
filterM' f (x:xs) = do
    ok <- f x
    xss <- filterM' f xs
    return (if ok then x:xss else xss)

filterM'' :: (Monad m) => (a -> m Bool) -> [a] -> m [a] 
filterM'' _ [] = return []
filterM'' f (x:xs) = f x >>= 
    (\ok -> if ok then (filterM'' f xs) >>= (\xss -> return (x:xss)) else filterM'' f xs)

-- 멱집합을 filterM 으로 짜보면
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs  -- 헐...?
-- 이거 이해가 잘 안되는데 일단 list monad 구현(concat map) 과 filterM 구현을 좀 보고
-- 멱집합을 만들때 빈거 [[]] 부터 시작해서 인풋의 각 원소들을 존재하는(True) 그룹, 없는(False) 그룹으로 계속 중첩 계산하는 방식으로 구현한다.
-- [ [] ] =(인풋 3)=>  [ [3] , [] ]  =(인풋 2)=> [ [2,3],[2] , [3],[] ]

-- main = print $ powerset [1,2,3]
-- main = print $ filterM'' (\x -> [True, False]) [1,2,3]

-- foldM
-- foldl 의 monad 판
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing    
    | otherwise = Just (acc + x)
-- 의미를 생각해보면 9보다 큰 원소가 하나라도 있으면 Nothing 으로 처리

-- main = do
    -- print $ foldM binSmalls 0 [2,8,3,1]
    -- print $ foldM binSmalls 0 [2,11,3,1]

-- 챕터 10의 RPN 문제를 monad 로 풀어보자
readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x   -- reads 는 실패하면 [] 가나옴
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

testFoldingFunction = do
    print $ foldingFunction [3,2] "*"
    print $ foldingFunction [] "*"  -- 실패하면 Nothing 이 나옴
    print $ foldingFunction [] "1"
    print $ foldingFunction [] "1 wawawawa"

-- main = testFoldingFunction

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)  -- foldl 대신 foldM 을 사용
    return result

-- Composing monadic functions
-- 함수 합성을 리스트와 fold 를 이용해서 아래처럼 짜볼수 있다
-- let f = foldr (.) id [(+1),(*100),(+1)]  -- 초기값이 id

-- monad 함수 합성을 위와 비슷하게 해보면

-- 앞 챕터에서 만든 monad 합성
-- (<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
-- f <=< g = (\x -> g x >>= f)

-- inMany :: Int -> KnightPos -> [KnightPos]  
-- inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)
-- moveKnight 를 x 개 들어있는 리스트, 그리고 초기값이 return


map multAll [( Prob [('a',1%2),('b',1%2)] , 1%4 )  ,( Prob [('c',1%2),('d',1%2)] , 3%4)]

multAll:: (Prob a, Rational) -> [(a, Rational)]
multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs