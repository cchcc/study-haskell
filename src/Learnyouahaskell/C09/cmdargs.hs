import System.Environment

main = do
  args <- getArgs
  print args
  pname <- getProgName
  print pname
