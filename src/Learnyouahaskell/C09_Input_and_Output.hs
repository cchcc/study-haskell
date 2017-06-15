module Learnyouahaskell.C09_Input_and_Output where

--
-- 09. Input and Output  http://learnyouahaskell.com/input-and-output
--

-- I/O action
-- I/O action 은 main 에서만 동작함. main 역시 I/O action 임

import Control.Monad
import System.Random
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Exception
import System.IO.Error


main = do  -- do block 역시 I/O action
  -- name <- getLine  -- getLine 으로 입력받은거는 타입이 IO String 임. 이걸 꺼내서 쓰려면 name <- getLine
  -- name = getLine 이렇게 하면 그냥 getLine 함수 그자체임.
  -- putStrLn name
  return "111"  -- 이거는 "111" 을 IO 로 감싸는것. <- 의 반대개념
  putChar '1'
  putStr "123" -- putChar 를 빈문자 나올때까지 재귀로
  print "123" -- putStr . show
  when True (print "hi") -- 조건이 True 면 2번째 I/O action 파라매터 아니면 return ()
  sequence [(print 1), (print 2), (print 3)] -- 배열을 받아서 IO [a] 로 리턴
  mapM print [1,2,3]  -- sequence 동작과 동일
  mapM_ print [1,2,3]  -- mapM 와 동일하나 ghci 에서 마지막에 자신에 대한 I/O 는 리턴안함

  -- forever $ do  -- I/O action을 하나 받아서 하나 리턴, 반복
  -- putStr "input>"
  -- line <- getLine
  -- print line
  -- 위에것들은 다 I/O action 을 리턴함
  -- getline 이나 I/O action 들  쉘에서 | 이거 파이프로 연결할수 있음
  -- content <- getContents  -- 입력받은거를 lazy 하게 처리함 (인풋값을 출력을 한다고 할때 출력하기 직전까지 동작을 미룸) 읽을 때는 line 단위로 읽는다

  gen <- getStdGen  -- 시스템에서 적절한 랜덤 생성기를 만듬. 여러번 호출해도 글로벌 1개만 사용하고 새로 만드려면 newStdGen 사용
  print $ take 10 (randoms gen :: [Int])
  print $ take 10 (randomRs (1,10) gen :: [Int]) -- 랜덤의 범위를 지정해줄때
  print $ take 10 (randomRs (1,10) gen :: [Int])
  gen' <- newStdGen  
  print $ take 10 (randomRs (1,10) gen' :: [Int])
  print $ take 10 (randomRs (1,10) gen' :: [Int])

-- bytestrings  [Word8]
-- 크기가 큰 파일을 스트링으로 읽어오는데 이걸 lazy 방식으로 하나씩(바이트) 가져오기에는 느리다. 이런경우에 사용함
-- 2종류가 있음.
-- Data.ByteString  -- bytestrings strict  -- 값을 1바이트라도 읽으려면 전체를 읽어야함. 읽기에는 빠름 그러나 메모리를 많이 먹음
-- Data.ByteString.Lazy -- bytestings lazy  -- 64k 청크 단위로 lazy 하게 처리함. cpu l2 캐시크기에 맞춤?
-- GHC.Word.Word8 1바이트 짜리 숫자타입(0~255)
-- 일단 String 을 쓰다가 성능이 필요해지면 그때 부터 사용하면 될듯
  print $ BL.pack [90,91,92]
  print $ BS.pack [90,91,92]

-- exception
-- haskell 에서는 순수함수가 기본적으로 lazy 로 동작하기 때문에 언제 평가(evaluate) 될지 알수 없어서
-- 예외가 역시 발생시점을 알수 없기 때문에 예외는 I/O 처리 부분에서만 받아서 처리 한다.
-- import Control.Exception
  -- catch rfile (\e -> print e)  -- 람다를 쓴다면 아래처럼 타입을 명시 해줘야 동작함
  catch tryReadfile ((\e -> print e) :: IOError -> IO ())
  tryReadfile `catch` handler  -- 이렇게 쓰면 가독성있고 좋음

tryReadfile :: IO ()
tryReadfile = do
  content <- readFile "filename"
  putStrLn content

handler :: IOError -> IO ()
handler e = print e

handler2 :: IOError -> IO ()
handler2 e  -- 특정 예외만 따로 처리할경우 이런식으로
  | isDoesNotExistError e = print "isDoesNotExistError"
  | isIllegalOperation e = print "isIllegalOperation"
  | otherwise = ioError e
