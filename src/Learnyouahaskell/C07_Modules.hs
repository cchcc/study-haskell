module Learnyouahaskell.C07_Modules where

--
-- 07. Modules  http://learnyouahaskell.com/modules
--


-- 모듈 쓸때
-- import Data.List

-- 모듈에서 함수 몇가지만 쓰고플때
-- import Data.List (nub, sort)

-- 모듈에서 특정 함수는 빼고 쓰고플때
-- import Data.List hiding (nub)

-- 모듈사용시 이미 있는 같은이름의 함수가 중복 되지 않도록
-- import qualified Data.Map
-- Data.Map.nub [1,2,3,3] -- 이렇게 사용

-- 모듈사용시 닉네임으로 사용
import qualified Data.List as DL
-- DL.nub [1,2,3,3]

-- http://haskell.org/hoogle

import qualified Data.Function as DF
import qualified Data.Char as DC
import qualified Data.Map as DM -- 이거하면 Data.List 도 import 됨
import qualified Data.Set as DS -- 이거하면 Data.List 도 import 됨
-- import qualified MyModule as MM

main = do

  -- Data.List
  print $ DL.intersperse 0 [1,2,3] -- 사이에 껴넣음
  print $ DL.intercalate [0] [[1,1],[2,2],[3,3]]  -- 사이에 껴넣음
  print $ DL.transpose [[1,2],[10,20,30],[100]] -- 행과 열을 바꿈
  print $ DL.foldl' (\acc x -> acc + x) 0 [1..10] -- foldl 의 strict 버전 lazy 연산하면서 스택오버플로 나면 이걸쓰자
  print $ DL.foldl1' (\acc x -> acc + x) [1..10]
  print $ DL.concat [[1], [2], [3]] -- 2차 배열을 1차 배열로
  -- and or any all 등
  print $ DL.span (/= ' ') "hello world"  -- 조건에 일치 안하는것 기준으로 앞,뒤 분리
  print $ DL.break (== ' ') "hello world"  -- 윗줄이랑 같은 결과
  print $ DL.group [1,1,2,2,1,1,3,4,4] -- 연속으로 같은것이 있으면 그것들끼리 그룹화
  print $ DL.groupBy (==) [1,1,2,2,1,1,3,4,4]
  print $ DL.partition (`elem` ['0'..'9']) "$ 1,000,000"  -- 조건에 (일치,일치안함) 으로 분리
  print $ DL.find (>4) [1,2,3,4,5,6]  -- 조건에 맞는 최초 항목 Maybe 없으면 Nothing 리턴
  -- words, unwords 등
  print $ DL.nub [1,1,2,4,4]  -- 중복없이 원소 하나씩 뽑아냄
  print $ DL.delete 'w' "hello world"
  print $ "hello world" DL.\\ "lwr" -- delete 'l' . delete 'w' . delete 'r' $ "hello world"
  print $ DL.union [1,2,3] [2,3,4] -- 두 리스트의 합집합
  print $ DL.intersect [1,2,3] [2,3,4] -- 두 리스트의 교집합
  print $ DL.insert 2 [1,2,3]

  -- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
  -- f `on` g = \x y -> f (g x) (g y)
  print $ DF.on (+) (+1) 1 1 -- Data.Function
  -- compare `on` something 이런식으로 종종 사용
  print $ DL.sortBy (compare `DF.on` length) [[1],[2,2,2,2],[3,3],[4,4,4]]

  -- Data.Char
  -- isSpace isNumber isLetter 등
  print $ map DC.generalCategory "1a- \n"

  -- Data.map
  -- association list : [("a",1),("b",2),("c",3)]
-- findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
-- findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
-- 이걸 recursion 으로 구현해도 되지만 fold 로 구현하는게 좀더 가독성 있다.

  print $ DM.fromList [(1,"a"),(2,"b"),(1,"1")] -- association list 로 map 만들기
  -- print $ DM.empty  -- TODO : 이거 출력안되는거 나중에 찾아보기
  print $ DM.insert 1 "a" DM.empty
  print $ DM.size DM.empty
  print $ DM.lookup 1 $ DM.fromList [(1,"a"),(2,"b")] -- 위에 findKey와 같음
  print $ DM.toList $ DM.singleton 1 1
  print $ DM.fromListWith (\v1 v2 -> v1 ++ v2) [(1,"a"),(2,"b"),(1,"1")]  -- 맵으로 만들때 중복키가 있을경우 어떻게 할건지를 정함

  -- Data.Set
  print $ DS.fromList [1,2,3]
  print $ DS.member 2 $ DS.fromList [1,2,3]
  -- union, size, toList 등

  -- 모듈 만들어 보기 MyModule.hs
  -- print $ MM.plus 1 1
