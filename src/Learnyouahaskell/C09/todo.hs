import System.Environment
import System.IO
import System.Directory

main = do
  args <- getArgs
  parseArgs args

parseArgs :: [String] -> IO ()
parseArgs ("view":fname:_) = viewTodo fname
parseArgs ("add":fname:todo:_) = addTodo fname todo
parseArgs ("del":fname:idx:_) = delTodo fname idx
parseArgs _ = print "??"

viewTodo :: String -> IO ()
viewTodo fname = withFile fname ReadMode (\handle -> do
  contents <- hGetContents handle
  mapM_ print (zipWith (\l r -> (show l) ++ " : " ++ r) [0..] (lines contents)) )

addTodo :: String -> String -> IO ()
addTodo fname todo = appendFile fname (todo ++ "\n")

delTodo :: String -> String -> IO ()
delTodo fname idx = do
  contents <- readFile fname
  removeFile fname  -- resource busy (file is locked) 때문에 해야함
  withFile fname WriteMode (\handle -> do
    let index = (read idx) :: Int
        filtered = filter (\(i,_) -> i /= index) (zip [0..] (lines contents))
    mapM_ (hPutStrLn handle) (map (\(i,s) -> s) filtered)
    )
