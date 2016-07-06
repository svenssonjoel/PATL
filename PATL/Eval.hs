
module PATL.Eval where


import PATL.AST

import qualified Data.Vector as V 
import qualified Data.Map as M

import Control.Monad.State 


------------------------------------------------------------
-- Evaluation

data EvalResult = Scalar Value
                | Array  (Shape EvalResult) (V.Vector Value)
                | Tuning TP
                | Function (EvalResult -> E EvalResult)


type Env = M.Map Identifier EvalResult --what about functions ?

emptyEnv = M.empty

type E a = State Env a

eval :: Exp -> EvalResult
eval e = evalState (doEval e) emptyEnv 
  where
    doEval :: Exp -> E EvalResult 
    doEval e =
      case e of
      
      Constant v -> return $ Scalar v

      Var ident  ->
        do env <- get 
           case M.lookup ident env of
             Nothing -> error "Environment malfunction"
             Just v  -> return v

      TuneParam t -> return $ Tuning t

      -- Evaluate Lam to haskell functions 
      Lam ident e ->
        return $ Function $ \fr ->
        do env <- get
           let env2 = M.insert ident fr env
           put env2 
           res <- doEval e
           put env -- reset the environment
           return res                
                          
      -- evaluate App to Haskell function application 
      App fun e ->
        do efun <- doEval fun
           ee   <- doEval e
           case efun of
             Function f -> f ee
             _          -> error "First argument of App is not a function"

      Op op es -> evalOp op es 



      -- Let bindings. Extends the environment
      Let ident e1 e2 ->
        do ee1 <- doEval e1
           env <- get
           let env2 = M.insert ident ee1 env
           put env2
           doEval e2
      
      Iota extents -> evalIota extents 

      -- Project out of container
      Prj e idx -> undefined

      Block bkng e -> undefined 
      UnBlock e -> undefined 

      -- SizeOf can return either a scalar or a shape. 
      SizeOf e -> undefined  


      -- PATTERNS
      Generate exts e -> undefined
      Map fun e -> undefined
      ZipWith fun e1 e2 -> undefined
      Reduce fun e -> undefined
      Transpose e -> undefined 
        
    evalIota :: Extents -> E EvalResult
    evalIota e =
      do shape <- evalExtents e
         let  size  = sizeExtents shape
         return $ Array shape (V.generate  size (\i -> (VInt i)))
        
    evalExtents Z = return Z 
    evalExtents (sh:.e) =
      do sh' <- evalExtents sh
         e' <- doEval e
         return $ sh':.e'
        
    sizeExtents :: Shape EvalResult -> Int
    sizeExtents Z = 1
    sizeExtents (sh:.Scalar (VInt v)) = v * sizeExtents sh
    sizeExtents _ = error "Invalid shape"
    
    

    evalOp :: Op -> [Exp] -> E EvalResult 
    evalOp = undefined 
      

