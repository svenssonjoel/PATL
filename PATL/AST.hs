
--
-- 
--

module PATL.AST where

--import PATL.Shape
import PATL.Value 
import PATL.TuneParam
import PATL.Operators

type Size = Exp 

{- The Language -} 

type Identifier = String

-- TODO: Should there be Tuples? (I think yes) 
data Exp = -- Annotated with what "top-level" types these would have
           -- (in whatever language we present to user)
          
           --  :: Int , Float  for example 
           Constant Value
           -- :: Int, Float, Array sh a, (a -> b) ... 
         | Var Identifier

           -- Shape and Index expressions
           -- :: Shape Int
         | Sh Exp
         | Ix Exp   
         | Z
         | Cons Exp Exp
         | IAll | IIndex Exp | IRange Exp Exp 

           -- :: Tune Int, Tune Bool 
         | TuneParam TP  -- tuning parameters

           -- Tuples :: (Int,Float) , (Array Int, Bool) ... 
         | Tuple [Exp]

           -- :: a -> b, a -> b -> c ... 
         | Op Op [Exp]

           -- Functions
           -- Lam :: a -> b 
         | Lam Identifier Exp
           
           -- App :: (a -> b) -> a -> b 
         | App Exp Exp

           -- Let bindings
           -- Let :: b 
         | Let Identifier Exp Exp


           -- Create an array (todo more ways)
           -- Iota :: (Shape Int) -> Array sh Int 
         | Iota Exp  -- what does this mean for multidim arrays ?


           -- Project from N-dimensional arrays
           -- Prj :: Array sh a -> Index -> Array sh' a
           -- Prj :: Array sh a -> Index -> a
           -- Maybe also Prj :: sh -> Index -> Exp 
         | Prj Exp Exp 

           -- SKETCHING

           -- Block :: Blocking -> Array sh a -> Array sh (Array sh' a) 
         | Block Blocking Exp -- Can be implemented by Generate + Prj (IRange)

           -- UnBlock :: Array sh (Array sh' a) -> Array sh'' a 
         | UnBlock Exp        -- Need some concat-like functionality


           -- Size of Exp (array or scalar)
           -- SizeOf :: Array sh a -> sh
           -- SizeOf :: Int, Float -> Int
           -- ... 
         | SizeOf Exp

           -------------------------------------------------------
           -- Patterns
           ------------------------------------------------------- 
           
           -- Generate :: (Shape Int) -> (Index -> a) -> Array sh a 
         | Generate Exp Exp
           
           -- Map :: (a -> b) -> Array sh a -> Array sh b 
         | Map Exp Exp
           
           -- ZipWith :: (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
         | ZipWith Exp Exp Exp
           
           -- Reduction is a tricky one!
           -- Reduce :: (a -> b -> b) -> b -> Array (sh:.i) a -> Array sh b
           -- Reduce :: (a -> b -> b) -> b -> Array (sh:.x:.rest) a -> Array (sh:.rest) b ??? 
           -- Reduce :: (a -> b -> b) -> b -> Array sh a -> b
           -- How to specify what reduction to use ??

           -- I think now that there will be only one Reduce!
           -- Reduce :: (a -> b -> b) -> b -> Array sh a -> b
         | Reduce Exp Exp Exp        
           -- | Permute ?
           -- | Scatter
           -- | Gather
           -- | Scan 
           
           deriving (Eq, Show)
                    
-- Need to change the way to express blocking (maybe using projections
-- and Generates) 
data Blocking = Square Exp
              | Rectangular Exp Exp
              | Chunk Exp 
                deriving (Eq, Show)


------------------------------------------------------------
-- Examples
myArray :: Exp
myArray = Iota (Cons (Constant (VInt 100)) Z)

myFun :: Exp
myFun = Lam "a" (Op Add [(Var "a"),(Constant (VInt 1))])

myPrg :: Exp
myPrg = Map myFun myArray
                 

------------------------------------------------------------
-- Blocked Matrix Mult sketch (for square matrices)
-- Cheating here and there 

-- blocked_mmult =
--   Lam "m1" $
--   Lam "m2" $ 
                      
--   Let "tp" (TuneParam TPInt) $
--   Let "blocked_m1" (Block (Square (Var "tp")) (Var "m1")) $
--   Let "blocked_m2" (Block (Square (Var "tp")) (Var "m2")) $
--   Let "block_rows_m1" (ZipWith extract_row (Var "blocked_m1") (Iota (Sh (Z:.(Var "bs"))))) $
--   Let "block_cols_m2" (ZipWith extract_col (Var "blocked_m2") (Iota (Sh (Z:.(Var "bs"))))) $
--   Let "bs" (SizeOf (Var "blocked_m1")) $ -- assuming square
--   Let "outer_gen" (Lam "m1"    
--                    $ Lam "m2" 
--                    $ Lam "row"
--                    $ Lam "col"
--                    $ Reduce (Var "add") (Constant (VInt 0)) (ZipWith (Var "mmult")
--                                          (apply extract_row [Var "row", Var "m1"])
--                                          (apply extract_col [Var "col", Var "m2"]))) $
--   Let "gen_func" (Lam "m1"
--                   $ Lam "m2"
--                   $ Lam "i"
--                   $ Lam "j"
--                   $ Reduce (Var "add") (Constant (VInt 0)) 
--                      (ZipWith (Var "multiply")
--                       (apply extract_row [Var "i", Var "m1"])
--                       (apply extract_col [Var "j", Var "m2"]))) $
--   Let "mmult" (Lam "m1"
--                $ Lam "m2"
--                $ Generate (Sh (Z:.(Var "tp"):.(Var "tp")))
--                  $ apply (Var "gen_fun") [Var "m1", Var "m2"]) $
--   Let "multiply" (Lam "x"
--                   $ Lam "y"
--                   $ Op Mul [Var "x",Var "y"]) $
--   Let "add" (Lam "x"
--              $ Lam "y"
--              $ Op Add [Var "x", Var "y"]) $

--   UnBlock $ Generate (Sh (Z:.(Var "bs"):.(Var "bs")))
--           $ apply (Var "outer_gen") [Var "blocked_m1",
--                                     Var "blocked_m2"]
               
-- extract_row :: Exp
-- extract_row = Lam "arr"
--               $ Lam "y"
--               $ Prj (Var "arr") (Z:.IAll:.(IIndex (Var "y")))

-- extract_col :: Exp
-- extract_col = Lam "arr"
--               $ Lam "y"
--               $ Prj (Var "arr") (Z:.(IIndex (Var "y")):.IAll)




-- apply :: Exp -> [Exp] -> Exp
-- apply e [] = e
-- apply e (x:xs) = apply (App e x) xs

-- mmult_example =
--   let c = Constant (VInt 128) 
--   in App (App blocked_mmult (Iota (Sh (Z:.c:.c)))) (Iota (Sh (Z:.c:.c)))


-- ------------------------------------------------------------
-- -- Reduction can be expressed in many ways.

-- myReduce = Lam "arr"
--            $ Let "add" (Lam "x"
--                         $ Lam "y"
--                         $ Op Add [Var "x", Var "y"])
--            $ Reduce (Var "add") (Constant (VInt 0)) (Var "arr")


-- -- 2 level reduce that can make better use of cache or local memory 
-- myReduce2 = Lam "arr"
--             $ Let "add" (Lam "x"
--                          $ Lam "y"
--                          $ Op Add [Var "x", Var "y"])
--             $ Let "chunk_size" (TuneParam TPInt) 
--             $ Reduce (Var "add") (Constant (VInt 0))
--                 (Map (Lam "chunk" (Reduce (Var "add") (Constant (VInt 0))(Var "chunk")))
--                   (Block (Chunk (Var "chunk_size"))  (Var "arr")))


------------------------------------------------------------
-- Fusion and Compilation sketches






