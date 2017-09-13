module WhileLangAst where
import Environment
import BasicTypes
import Data.Map

data ProgramPoint = Assignment Ident Expr
                  | Output Expr
                  -- | If (Environment a ()) (Environment a ())
                  -- | While (Environment a ())
                  | Declare Ident -- Parser should separate 'var x, y, z; to var x;var y;var z;'
                  deriving Show

-- Linked list version of ProgramPoint, forming a tree structure of the 
-- program. 
data Cfg a = CfgPP (LatticeMap a) ProgramPoint (Cfg a)
         -- 1st is true branch, 2nd is false branch and 3rd is after
         | CfgIf (LatticeMap a) (Cfg a) (Cfg a) (Cfg a)
         -- 1st is loop statement 2nd is after
         | CfgWhile (LatticeMap a) (Cfg a) (Cfg a)
         | CfgExit (LatticeMap a) -- To terminate the linked list, should hold a (CfgExit a)?
         deriving Show

cfgMap :: Cfg a -> LatticeMap a
cfgMap (CfgPP m _ _) = m
cfgMap (CfgIf m _ _ _) = m
cfgMap (CfgWhile m _ _) = m
cfgMap (CfgExit m) = m

-- Analysis III slide 14 says we can assume variables can only be integers,
-- so I assume (for now at least) expressions can only be integers
data Expr = Plus Expr Expr
          | Minus Expr Expr
          -- | Mult Expr Expr
          -- | Div Expr Expr
          | Number Integer
          | Var Ident
          deriving (Show, Eq)
