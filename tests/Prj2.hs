{-# LANGUAGE DataKinds #-} 

module Main where 

import PATL.EDSL
import PATL.EDSL.Syntax hiding (Z,IIndex, IRange ) 
import PATL.EDSL.Shape
import PATL.EDSL.Compile
import qualified PATL.AST as AST
import PATL.Eval

import Data.Functor.Identity

-- Project a range out of a 2d array 

-- TODO: This illustrates a problem with the EDSL.hs embedding 

-- prj1 :: Exp (Array '[Exp Int] (Exp Int)) 
-- prj1 = index (generate (emb (Identity 10:.Z :: Shape '[Exp Int])) 
--                        (emb $ \_ -> iota (emb (Identity 1000:.Z :: Shape '[Exp Int]))))
--              (emb (IIndex 5 :. IRange 10 100 :.Z))



-- doIt = do
--   putStrLn "**** EDSL Syntax ****"
--   putStrLn $ show (toExp prj1)
--   putStrLn "\n\n"
  
--   gr <- genGraph (unExp prj1)

--   putStrLn "**** Graph ****" 
--   putStrLn $ show gr
--   putStrLn "\n\n"


--   putStrLn "**** uses ****" 
--   putStrLn $ show $ uses 1 gr
--   putStrLn $ show $ allUses gr 
--   putStrLn "\n\n"

  
--   case graphToAST gr of
--     Nothing -> putStrLn "No AST"
--     Just a -> do
--         putStrLn "**** AST ****"
--         putStrLn $ show a
--         putStrLn "\n\n"
--         putStrLn "**** extracted TP ****"
--         let (_,m) = AST.extractTuneParams a
--         putStrLn $ show m
--         putStrLn "\n\n"
--         putStrLn "**** Evaluated AST ****"
--         let res = eval emptyEnv a
--         putStrLn $ show res


main = do
  putStrLn "BROKEN" -- doIt 
