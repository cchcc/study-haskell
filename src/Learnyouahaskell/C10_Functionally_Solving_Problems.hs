module Learnyouahaskell.C10_Functionally_Solving_Problems where

--
-- 10. Functionally Solving Problems  http://learnyouahaskell.com/functionally-solving-problems
--


-- Reverse Polish Notation Calculator
solveRPN :: (Floating a, Read a) => String -> Maybe a
solveRPN =
  let maybeHead [] = Nothing  -- maybeHead l = if null l then Nothing else Just (head l)
      maybeHead l  = Just (head l)
      calculate (n1:n2:ns) "+" = (n2+n1):ns
      calculate (n1:n2:ns) "-" = (n2-n1):ns
      calculate (n1:n2:ns) "*" = (n2*n1):ns
      calculate (n1:n2:ns) "/" = (n2/n1):ns  -- 나누기도 되게 하려면 Floating 으로 해야함
      calculate acc x = (fst $ head (reads x)):acc
  in maybeHead . foldl calculate [] . words

just :: Maybe a -> a
just (Just a) = a

main = do
  print $ solveRPN ""
  print $ just $ solveRPN "2 3 +"  -- 5
  print $ solveRPN "90 34 12 33 55 66 + * - +"  -- -3947
  print $ solveRPN "90 34 12 33 55 66 + * - + -"  -- 4037
  -- print $ solveRPN "a b c"
