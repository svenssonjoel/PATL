{-# LANGUAGE DataKinds #-} 

module Main where 

import PATL.EDSL
import PATL.EDSL.Syntax hiding (Z) 
import PATL.EDSL.Shape
import PATL.EDSL.Compile
import qualified PATL.AST as AST
import PATL.Eval




-- Sum 0 to 1000

sumIt :: Exp Int
sumIt = reduce (emb (+)) 0 (iota (emb (1000:.Z :: Shape '[Exp Int])))
          -- shape annotation currently required


showSum = putStrLn $ show (toExp sumIt)



showGraph = do
  gr <- genGraph (unExp sumIt)
  putStrLn $ show gr


showAST = do
  gr <- genGraph (unExp sumIt)
  case graphToAST gr of
    Nothing -> putStrLn "NO AST"
    Just a  -> putStrLn $ show a


showTP = do
  gr <- genGraph (unExp sumIt)
  case graphToAST gr of
    Nothing -> putStrLn "NO AST"
    Just a  -> let (_,m) = AST.extractTuneParams a
               in putStrLn $ show m


showResult = do
  gr <- genGraph (unExp sumIt)
  case graphToAST gr of
    Nothing -> putStrLn "No AST"
    Just a  -> do 
      let res = eval emptyEnv a
      putStrLn $ show res
      
 

--showAnGraph = do
--  gr <- genGraph (unExp sumIt)
--  let (UsageGraph agr root) = anUsage gr
--  putStrLn $ show agr 


main = do
  putStrLn "**** EDSL Syntax ****"
  showSum
  putStrLn "\n\n"
  putStrLn "**** Graph ****" 
  showGraph 
  putStrLn "\n\n"
  putStrLn "**** AST ****"
  showAST
  putStrLn "\n\n"
  putStrLn "**** extracted TP ****"
  showTP
  putStrLn "\n\n"
  putStrLn "**** Evaluated AST ****"
  showResult



-- let apa = [(1,(Reduce 2 7 8,fromList [2,3,4,7,8,9,10,11]))
--           ,(2,(Lam (FunArg 0) 3,fromList [3,4]))
--           ,(3,(Lam (FunArg 1) 4,fromList [4]))
--           ,(4,(Op Add [5,6],fromList []))
--           ,(7,(Constant (VInt 0),fromList []))
--           ,(8,(Iota 9,fromList [9,10,11]))
--           ,(9,(Cons 10 11,fromList [10,11]))
--           ,(10,(Constant (VInt 1000),fromList []))
--           ,(11,(Z,fromList []))]

-- let bepa = [(1,(Reduce 2 7 8,fromList [2,3,4,5,6,7,8,9,10,11]))
--            ,(2,(Lam (FunArg 0) 3,fromList [3,4,5,6]))
--            ,(3,(Lam (FunArg 1) 4,fromList [4,5,6]))
--            ,(4,(Op Add [5,6],fromList [5,6]))
--            ,(5,(Var (FunArg 0),fromList []))
--            ,(6,(Var (FunArg 1),fromList []))
--            ,(7,(Constant (VInt 0),fromList []))
--            ,(8,(Iota 9,fromList [9,10,11]))
--            ,(9,(Cons 10 11,fromList [10,11]))
--            ,(10,(Constant (VInt 1000),fromList []))
--            ,(11,(Z,fromList []))]
