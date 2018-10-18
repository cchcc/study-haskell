module Learnyouahaskell.C14_Zippers where

--
-- 14. Zippers  http://learnyouahaskell.com/zippers
--

-- Haskell 은 순수언어라 다른언어와는 다른 방식으로 문제를 해결해야 한다
-- 트리에서 특정 노드의 값을 바꿔 본다고 가정해보자.
-- 그러면 haskell 에서는 기존 트리의 값을 바꾸는게 아니라 값을 바꾼 새로운 트리를 만들어 내야 된다.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)  

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )

-- 위 트리에서 W 를 P 로 바꿔본다고 하면        
changeToP :: Tree Char -> Tree Char  
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)  

-- main = print $ changeToP freeTree

data Direction = L | R deriving (Show)  
type Directions = [Direction]  -- 루트부터 어디로 이동하면 되는지를 리스트로 만듬
  
changeToP' :: Directions-> Tree Char -> Tree Char  
changeToP' (L:ds) (Node x l r) = Node x (changeToP' ds l) r  
changeToP' (R:ds) (Node x l r) = Node x l (changeToP' ds r)  
changeToP' [] (Node _ l r) = Node 'P' l r 

-- main = print $ changeToP' [R,L] freeTree

elemAt :: Directions -> Tree a -> a   -- Directions 를 이용해서 특정 위치의 값이 뭔지 알아낼수 있다
elemAt (L:ds) (Node _ l _) = elemAt ds l  
elemAt (R:ds) (Node _ _ r) = elemAt ds r  
elemAt [] (Node x _ _) = x 

testWtoP = do
    print $ elemAt [R,L] freeTree
    let newTree = changeToP' [R,L] freeTree
    print $ elemAt [R,L] newTree

-- main = testWtoP

-- 이제 밑으로 내려가는것 뿐아니라 옆으로나 위로 올라갈수 있게 만들어보자
-- 움직여온 위치를 어떤 빵조각을 남기는것으로 비유해 볼수 있다

type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
goRight (Node _ _ r, bs) = (r, R:bs)

x -: f = f x

-- main = do
    -- print $ goLeft (goRight (freeTree, []))
    -- print $ (freeTree, []) -: goRight -: goLeft -- -: 를 하나 만들어서 좀더 가독성있게 만들어 줄수 있다

-- 역으로 부모쪽으로 올라가는 함수를 만들려면 어떻게 할까?
-- 이동할때 부모와 형제 정보도 같이 들고 다녀야한다.
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show) -- a 가 부모값, (Tree a) 가 형제 노드

type Breadcrumbs' a = [Crumb a]

goLeft' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goLeft' (Node x l r, bs) = (l, LeftCrumb x r:bs)  

goRight' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goRight' (Node x l r, bs) = (r, RightCrumb x l:bs)  

goUp :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)  -- Breadcrumbs 의 제일 앞에껄로 부모노드 재구성
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

goSibling :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goSibling (t, br@(LeftCrumb x r:bs)) = (t, br) -: goUp -: goRight'
goSibling (t, br@(RightCrumb x l:bs)) = (t, br) -: goUp -: goLeft'

-- main = print $ (freeTree, []) -: goRight' -: goLeft' -: goUp -: goUp

-- 위에서 해당 노드와 그 주변정보를 묶은것을 zipper 라 해보자
type Zipper a = (Tree a, Breadcrumbs' a)

