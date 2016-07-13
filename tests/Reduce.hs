-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-} 

module Main where 

import PATL.EDSL
import PATL.EDSL.Syntax hiding (Z) 
import PATL.EDSL.Shape
import PATL.EDSL.Compile





-- Sum 0 to 1000

sumIt :: Exp Int 
sumIt = reduce (emb (+)) 0 (iota (emb (1000:.Z :: Shape '[Exp Int])))
          -- shape annotation currently required


showSum = putStrLn $ show (toExp sumIt)


showGraph = do
  gr <- genGraph (unExp sumIt)
  putStrLn $ show gr


main = do
  putStrLn "**** EDSL Syntax ****"
  showSum
  putStrLn "\n\n"
  putStrLn "**** Graph ****" 
  showGraph 
