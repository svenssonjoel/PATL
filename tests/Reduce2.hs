{-# LANGUAGE DataKinds #-} 

module Main where 

import PATL.EDSL
import PATL.EDSL.Syntax hiding (Z) 
import PATL.EDSL.Shape
import PATL.EDSL.Compile
import qualified PATL.AST as AST
import PATL.Eval

import Prelude hiding (map)
import qualified Prelude as P

import Data.Functor.Identity

-- Sum 0 to 1000 (not including 1000) 

generateData = generate (emb (Identity 10:.Z :: Shape '[Exp Int]))
                        (emb $ \_ -> iota (emb (Identity 10:.Z :: Shape '[Exp Int])))
                                        

sumIt :: Exp Int
sumIt = reduce (emb (+)) 0 $ map (emb (reduce (emb (+)) 0)) generateData 
          -- shape annotation currently required

doIt = do
  putStrLn "**** EDSL Syntax ****"
  putStrLn $ show (toExp sumIt)
  putStrLn "\n\n"
  
  gr <- genGraph (unExp sumIt)

  putStrLn "**** Graph ****" 
  putStrLn $ show gr
  putStrLn "\n\n"

  
  case graphToAST gr of
    Nothing -> putStrLn "No AST"
    Just a -> do
        putStrLn "**** AST ****"
        putStrLn $ show a
        putStrLn "\n\n"
        putStrLn "**** extracted TP ****"
        let (_,m) = AST.extractTuneParams a
        putStrLn $ show m
        putStrLn "\n\n"
        putStrLn "**** Evaluated AST ****"
        let res = eval emptyEnv a
        putStrLn $ show res


main = do
  doIt 
