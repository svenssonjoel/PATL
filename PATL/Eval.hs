{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module PATL.Eval where


import PATL.AST
import qualified PATL.EDSL.Shape as S -- TODO: This is unfortunate, fix it! 
import PATL.EDSL.Shape hiding (Z)

import PATL.Value
import PATL.Operators
import PATL.TuneParam

import qualified Data.Vector as V 
import qualified Data.Map as M

import Control.Monad.State 


------------------------------------------------------------
-- Evaluation

type EvalShape = [EvalResult]

-- Clean this up later
data EvalResult = Scalar Value
                | Tup    [EvalResult]
                | Array  EvalShape (V.Vector (EvalResult))
                | Shap   EvalShape  -- Better constr names! (maybe prefix Eval_)
             --   | Tuning TP
                | Function (EvalResult -> EvalResult)


type Env = M.Map Identifier EvalResult --what about functions ?

emptyEnv = M.empty

type E a = State Env a

eval :: Env -> Exp -> EvalResult
eval env e = evalState (doEval e) env 
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


      -- ------------------------------
      -- Tuning parameters should have been
      -- removed before evaluation.
      -- extractTuneParams replaces these with variables.

      TuneParam t -> error $ "Tuning parameter present at eval" 

      -- Evaluate tuples
      Tuple es -> do
        es' <- mapM doEval es 
        return $ Tup es'

      -- Evaluate Lam to haskell functions 
      Lam ident e ->
        do env <- get -- capture environment here 
           
           return $ Function
                  $ \fr -> evalState ( 
             do env <- get
                let env2 = M.insert ident fr env
                put env2 
                res <- doEval e
                put env -- reset the environment
                return res
             ) env
             -- changes to env within function does not leak out 
                          
      -- evaluate App to Haskell function application 
      App fun e ->
        do efun <- doEval fun
           ee   <- doEval e
           case efun of
             Function f -> return $ f ee
             _          -> appNotAFun

      -- Evaluate any operator
      Op op es -> evalOp op es

      -- SHAPES
      Sh shape -> do
        ext <- evalExtents shape 
        return $ Shap ext

      Ix shape -> error "NOT IMPLEMENTED" 

      -- Let bindings. Extends the environment
      Let ident e1 e2 ->
        do ee1 <- doEval e1
           env <- get
           let env2 = M.insert ident ee1 env
           put env2
           doEval e2

      -- Iota. Shape to array 
      Iota extents -> do
        extents' <- doEval extents
        case extents' of
          (Shap e) -> evalIota e
          _ -> error "Argument to Iota must be a shape" 
   

      -- Project out of container
      Prj e idx -> undefined

      -- SizeOf can return either a scalar or a shape. 
      SizeOf e ->
        do e' <- doEval e
           case e' of
             Scalar _ -> return $ Scalar (VInt 1)
             Array sh _ -> return $ Shap sh 


      -- PATTERNS
      Generate exts e -> undefined


      Map fun e ->
        do e' <- doEval e
           fun' <- doEval fun 
           case e' of
             Array sh v ->
               case fun' of
                 Function f -> return $ Array sh (V.map f v)
                 _ -> error "Argument to Map is not a function"
             _ -> error "Argument to Map is not an Array"
             -- The errors here are of the kind that type checking will catch 


      -- ZipWith needs to do some extents checking.
      -- TODO: check extents
      -- TODO: decide what combinations of shapes are allowed in a zipwith
      -- Currently requires that both inputs have exact same shape and extents 
      ZipWith fun e1 e2 ->
        do e1' <- doEval e1
           e2' <- doEval e2
           fun' <- doEval fun
           case (e1',e2') of
             (Array sh1 v1, Array sh2 v2) ->
               case fun' of
                 Function f ->
                   return $ Array sh1
                   (V.zipWith
                    (\x y -> let Function f' = f x in f' y) v1 v2) 
                 _ -> error "Argument to ZipWith is not a function"
             _ -> error "Argument to ZipWith is not an Array" 
                         


      -- what reduction to perform here ???
      -- Reduce should be augmented with a description of
      -- what dimension to reduce.
      -- Currently reduce over outermost dimension (reduce_rows)

      -- Reduction could actually be down to a scalar in ALL cases !
      -- It is then up to the programmer to "block" up the data into
      -- chunks before (map reduce) on the blocked structure, if some
      -- other reduction is desired. 
      
      Reduce fun e_id e ->
        do e'    <- doEval e
           e_id' <- doEval e_id 
           Function f <- doEval fun
           case e' of
             (Array sh v) ->
               case sh of
                 [] ->  return $ Array sh v
                 [x] -> return $ Array [] (V.singleton $ V.foldr
                                           (\x y -> let Function f' = f x in f y) e_id' v)
                 _ -> error "NOT SUPPORTED"
             _ -> error "Argument to reduce is not an Array" 
    
                                                 
           


    appNotAFun = error "First argument of App is not a function"

    evalIota :: EvalShape -> E EvalResult
    evalIota sh =
      do --shape <- evalExtents e
         let  size  = sizeExtents sh
         return $ Array sh (V.generate size (\i -> (Scalar (VInt i))))

    evalExtents :: Exp -> E EvalShape
    evalExtents Z = return [] 
    evalExtents (Cons sh e) =
      do sh' <- evalExtents sh
         e' <- doEval e
         return $  e' : sh'
        
    sizeExtents :: EvalShape -> Int
    sizeExtents xs = foldl (\b (Scalar (VInt v))  -> v * b) 1 xs     
    
    evalOp :: Op -> [Exp] -> E EvalResult
    evalOp op1 [e1]    = undefined 
    evalOp op2 [e1,e2] =
      do e1' <- doEval e1
         e2' <- doEval e2
         case (e1',e2') of
           (Scalar (VInt i1), Scalar (VInt i2)) ->
             case op2 of
               Add -> return $ Scalar (VInt (i1 + i2))
               Sub -> return $ Scalar (VInt (i1 - i2))
               Mul -> return $ Scalar (VInt (i1 * i2))
               Div -> return $ Scalar (VInt (i1 `div` i2))

           (Scalar (VFloat f1), Scalar (VFloat f2)) ->
             case op2 of
               Add -> return $ Scalar (VFloat (f1 + f2))
               Sub -> return $ Scalar (VFloat (f1 - f2))
               Mul -> return $ Scalar (VFloat (f1 * f2))
               Div -> return $ Scalar (VFloat (f1 / f2))

   
    
      

