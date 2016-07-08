
module PATL.EDSLCompile where

import PATL.Shape
import qualified PATL.AST as A 
import qualified PATL.EDSL as E


import Data.Reify
import Data.Reify.Graph


-- For suitable restrictions on a 
compile :: E.Exp a -> A.Exp
compile = undefined 
