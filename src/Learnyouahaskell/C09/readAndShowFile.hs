import System.IO

main = readAndShowFile'' "hihi.txt"

readAndShowFile :: String -> IO ()
readAndShowFile fname = do
  handle <- openFile fname ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

readAndShowFile' :: String -> IO ()
readAndShowFile' fname = do
  withFile fname ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)

readAndShowFile'' :: String -> IO ()
readAndShowfile'' fname = do
  contents <- readFile fname
  putStr contents
