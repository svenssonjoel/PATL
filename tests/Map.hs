{-# LANGUAGE DataKinds #-} 

module Main where 

import PATL.EDSL
import PATL.EDSL.Syntax hiding (Z) 
import PATL.EDSL.Shape
import PATL.EDSL.Compile
import qualified PATL.AST as AST
import PATL.Eval

import Data.Functor.Identity 

import Prelude hiding (map)

addIt :: Exp (Array '[Exp Int, Exp Int] (Exp Int))
addIt = map (emb (+1000)) (iota (emb (Identity 10:. Identity 100:.Z :: Shape '[Exp Int,Exp Int])))


doIt = do
  putStrLn "**** EDSL Syntax ****"
  putStrLn $ show (toExp addIt)
  putStrLn "\n\n"
  
  gr <- genGraph (unExp addIt)

  putStrLn "**** Graph ****" 
  putStrLn $ show gr
  putStrLn "\n\n"


  putStrLn "**** uses ****" 
  putStrLn $ show $ uses 1 gr
  putStrLn $ show $ uses 8 gr
  putStrLn $ show $ allUses gr 
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
