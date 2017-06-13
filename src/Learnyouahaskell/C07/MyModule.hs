module MyModule (
  plus
  ,hello
) where

plus :: (Num a) => a -> a -> a
plus = (+)

-- hello :: () -> IO()
hello = print "hello"

-- hi :: () -> IO()
hi = print "hi"
