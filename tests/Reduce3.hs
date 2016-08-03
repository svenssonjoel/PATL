{-# LANGUAGE DataKinds #-} 

module Main where 

import PATL.EDSL
import PATL.EDSL.Syntax hiding (Z) 
import PATL.EDSL.Shape
import PATL.EDSL.Compile
import qualified PATL.AST as AST
import PATL.Eval
import PATL.Value 

import Prelude hiding (map,div)
import qualified Prelude as P

import qualified Data.Map as M

import Data.Functor.Identity
-- Sum 

generateData =
  let ts = 10  `powi` tInt 1 5
      bs = 100000 `div` ts
  in generate (emb (Identity ts:.Z :: Shape '[Exp Int]))
              (emb $ \_ -> generate (emb (Identity bs:.Z :: Shape '[Exp Int])) (emb (\_ -> (1 :: Exp Int))))
                                        
-- two level nested data-parallel program 
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
        let (a',m) = AST.extractTuneParams a
        putStrLn $ show m
        putStrLn "\n\n"
        putStrLn "**** Evaluated AST ****"
        let res = eval (M.fromList [("tp0", Scalar (VInt 2))]) a'
        putStrLn $ show res

        putStrLn "**** Checking TP safety ****"
        checkTPSafety a


main = do
  doIt 
