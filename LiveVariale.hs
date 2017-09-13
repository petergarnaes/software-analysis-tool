import Analysis
import Lattice
import Environment
import WhileLangAst
import TransferFunction
import Data.Map as Map (Map,empty)

data LiveVar = LTop | LBottom deriving (Eq,Show)

instance Lattice LiveVar where
    initialize = LBottom
    super LBottom = LBottom : super LTop
    super LTop = [LTop]

transferComposite :: (TransferFunction a) => Expr -> Expr -> 
    Environment a () -> Environment a a
transferComposite e1 e2 env = do
    transferExpr e1 env
    transferExpr e2 env
    return initialize

instance TransferFunction LiveVar where
    transfer (Assignment id expr) env = do
        -- Resets element, do this first in case this id is used in expr
        insertElement id initialize
        -- Just use side effect
        transferExpr expr env
        return ()
    transfer (Declare id) env = insertElement id initialize
    transfer (Output expr) env = do
        transferExpr expr env
        return ()
    transferExpr (Plus e1 e2) env = transferComposite e1 e2 env
    transferExpr (Minus e1 e2) env = transferComposite e1 e2 env
    transferExpr (Var id) env = do
        insertElement id LTop
        return initialize
    transferExpr _ env = return initialize

test1 :: Cfg LiveVar
test1 = CfgPP
    Map.empty
    (Assignment "x" (Number 0))
        (CfgPP
            Map.empty
            (Assignment "y" (Number 1)) 
                (CfgIf
                    Map.empty
                    (CfgPP Map.empty (Output (Plus (Var "x") (Number 5))) (CfgExit Map.empty))
                    (CfgPP Map.empty (Assignment "x" (Minus (Number 1) (Number 2))) (CfgExit Map.empty))
                    (CfgExit Map.empty)))

-- This analysis is performed backwards
analyseTest1 = fullAnalysis (invert test1)
