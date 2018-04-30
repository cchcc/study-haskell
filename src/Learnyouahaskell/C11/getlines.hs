main = do
  line <- (++) <$> getLine <*> getLine
  putStrLn line
