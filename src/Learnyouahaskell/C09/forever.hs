import Control.Monad
main = forever $ do  -- I/O action을 하나 받아서 하나 리턴
         putStr "input>"
         line <- getLine
         print line
