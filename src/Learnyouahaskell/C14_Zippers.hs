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

-- 관심이 있는 원소(Tree a) 와 이걸 뺀 나머지 모든 주변정보(Breadcrumbs a) 를 합친것 이를 zipper 라 해보자
type Zipper a = (Tree a, Breadcrumbs' a)

modify :: (a -> a) -> Zipper a -> Zipper a  
modify f (Node x l r, bs) = (Node (f x) l r, bs)  
modify f (Empty, bs) = (Empty, bs)

testModify = do
    let newFocus = (freeTree,[]) -: goLeft' -: goRight' -: modify (\_ -> 'P') -- 위에 W 를 P 로 바꾸기
    print $ newFocus
    print $ newFocus -: goUp -: modify (\_ ->'X')

-- main == testModify

-- 현재 focus 를 새로운 tree 로 바꿈.
attach :: Tree a -> Zipper a -> Zipper a  
attach t (_, bs) = (t, bs)

testAttach = do
    let farLeft = (freeTree,[]) -: goLeft' -: goLeft' -: goLeft' -: goLeft'  
    let newFocus = farLeft -: attach (Node 'Z' Empty Empty)
    print newFocus

-- main = testAttach

-- 루트로 이동하기. Breadcrumbs 가 [] 가 될때까지 재귀
topMost :: Zipper a -> Zipper a  
topMost (t,[]) = (t,[])  
topMost z = topMost (goUp z)

-- 리스트용 Zipper 를 생각해보자. 리스트는 자식 노드가 하나뿐인 트리로 생각해볼수 있다.
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- (관심정보, 그외 모든정보)
-- 관심있는 정보는 해당 위치부터 나머지 리스트를 다 가진것 이고,
-- 그외 모든정보는 트리처럼 방향정보나 그외 노드(부모,형제)가 없고 이전 정보만 필요하다. Crumbs 처럼 타입 따로 안만들고 그냥 [a]
type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)
  
goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

goFirst :: ListZipper a -> ListZipper a
goFirst (xs, []) = (xs, [])
goFirst z = goFirst $ goBack z

testListZipper = do
    let xs = [1,2,3,4]
    let xs2 = (xs,[]) -: goForward
    print $ xs2
    let xs3 = xs2 -: goForward -: goForward -: goBack
    print $ xs3
    print $ goFirst xs3

-- main = testListZipper

-- 아래 같은 파일 시스템의 zipper 를 만들어보자.
type Name = String  
type Data = String  
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem  
myDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]

-- 트리와 비슷한데 다만 아래로 내려가는것은 Folder 만 가능하다. 즉 어떤 원소의 부모는 Folder 만 됨. 
-- File 은 트리의 Empty 와 비슷
-- Breadcrumb 을 생각해보면.. 
-- 해당원소 외에 모든 주변정보는 Folder 에 들어있다.  Folder 의 이름과 리스트에서 해당 원소만 빼서 왼/오 로 나눠서 저장해볼수 있다.
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper  
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs) -- 부모 재생성, bs 의 맨앞에꺼 뺌
