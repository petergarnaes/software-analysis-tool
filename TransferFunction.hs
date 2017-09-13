module TransferFunction where
import WhileLangAst
import Data.List as List (foldl')
import Lattice
import Environment

class (Lattice a) => TransferFunction a where
    -- Implement the transfer functions for all the types of statements
    transfer :: ProgramPoint -> Environment a () -> Environment a () 
    -- Implement transfer functions for expressions, note that you have to 
    -- return a lattice element, however if this is not needed you can return 
    -- 'initialize' and just use the side effect. This function is to help you 
    -- evaluate expressions in output and assignment
    transferExpr :: Expr -> Environment a () -> Environment a a

evaluateProgramPoints :: (TransferFunction a) => [ProgramPoint] -> Environment a () -> Environment a ()
evaluateProgramPoints pps env = List.foldl' helper env pps
    where helper env pp = do env
                             transfer pp env
