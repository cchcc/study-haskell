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

main = do
    print $ elemAt [R,L] freeTree
    let newTree = changeToP' [R,L] freeTree
    print $ elemAt [R,L] newTree