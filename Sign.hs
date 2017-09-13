import Analysis
import Lattice
import Environment
import WhileLangAst
import TransferFunction
import Data.List
import Data.Map as Map (Map,empty)

data Sign = SBottom | SZero | SMinus | SPlus | SMinusZero | SPlusZero | STop deriving (Eq,Show)

instance Lattice Sign where
    initialize = SBottom
    super SBottom = SBottom : nub (concatMap super [SZero,SPlus,SMinus])
    super SZero = SZero : nub (concatMap super [SMinusZero,SPlusZero])
    super SPlus = SPlus : super SPlusZero
    super SMinus = SMinus : super SMinusZero
    super SPlusZero = SPlusZero : super STop
    super SMinusZero = SMinusZero : super STop
    super STop = [STop]

invert :: Sign -> Sign
invert sign = if sign == SPlus then SMinus else if sign == SMinus then SPlus else sign

signOf :: Integer -> Sign
signOf int = if int >= 0 
             then if int > 0 then SPlus else SZero 
             else SMinus

plusCombine :: Sign -> Sign -> Sign
plusCombine sign1 sign2 = if sign1 == SBottom || sign2 == SBottom then SBottom else
    if sign1 == sign2 
    then sign1 
    else if sign1 == SZero 
        then sign2
        else if sign2 == SZero
            then sign1
            else STop

minusCombine :: Sign -> Sign -> Sign
minusCombine sign1 sign2 = if sign1 == SBottom || sign2 == SBottom then SBottom else
    if sign1 == SZero 
        then invert sign2
        else if sign2 == SZero
            then invert sign1
            else if sign1 == SMinus && sign2 == SPlus 
                then SMinus 
                else if sign1 == SPlus && sign2 == SMinus 
                    then SPlus
                    else STop

instance TransferFunction Sign where
    transfer (Assignment id expr) env = do 
        --idSign <- getLatticeElement id
        exprSign <- transferExpr expr env
        insertElement id exprSign 
    --transfer (If pp1 pp2) env = let
        --envIf = evaluateProgramPoints pp1 env
        --envElse = evaluateProgramPoints pp2 env 
        --in leastUpperBound envIf envElse
    --transfer (If envIf envElse) env = leastUpperBound envIf envElse
    --transfer (While pps) env = let 
    --    envWhile = evaluateProgramPoints pps env 
    --    in leastUpperBound envWhile env
    --transfer (While envWhile) env = leastUpperBound envWhile env
    transfer (Declare id) env = insertElement id initialize -- 
    transfer _ env = env -- all other statements are ignored
    transferExpr (Plus expr1 expr2) env = do
        sign1 <- transferExpr expr1 env
        sign2 <- transferExpr expr2 env
        return $ plusCombine sign1 sign2
    transferExpr (Minus expr1 expr2) env = do
        sign1 <- transferExpr expr1 env
        sign2 <- transferExpr expr2 env
        return $ minusCombine sign1 sign2
    transferExpr (Number int) env = return $ signOf int
    transferExpr (Var id) env = getLatticeElement id

-- x := 0;
-- y := 0+0;
-- if(...){
--   x := 1;
-- } else {
--   y := -2;
-- }
-- output x;
test1 :: Cfg Sign
test1 = CfgPP 
    Map.empty 
    (Assignment "x" (Number 0))
        (CfgPP Map.empty 
        (Assignment "y" (Plus (Var "x") (Number 0))) 
        (CfgIf Map.empty 
            (CfgPP Map.empty (Assignment "x" (Number 1)) (CfgExit Map.empty)) 
            (CfgPP Map.empty (Assignment "y" (Number (-2))) (CfgExit Map.empty)) 
            (CfgPP Map.empty (Output (Var "x")) (CfgExit Map.empty))))

--test = [Declare "x", Declare "y",
--    If [Assignment "x" (Number 1)] [Assignment "y" (Number 2)],
--    Output (Var "x"), Declare "z"]

--test2 = [Declare "x",Declare "y",Assignment "y" (Number 0),
--    Assignment "x" (Plus (Number 6) (Var "y")),
--    If [Assignment "x" (Number (-1))] [Assignment "y" (Number 2)]]
--test3 = [Declare "x",Declare "y"]
--test4 = [Declare "x",Declare "y",Declare "z",While [Assignment "a" (Number 4),Assignment "b" (Number 5)],Output (Var "g"),Output (Var "g"),Output (Var "g"),Output (Var "g")]
--test5 = [Assignment "x" (Number 1),Assignment "y" (Number 2)]
--test6 = [If [Assignment "x" (Number 1)] [Assignment "y" (Number 2)]]
--test7 = [Declare "x",Declare "y",Declare "z",While [Assignment "y" (Number 4)],Declare "g"]

--runTest4 = runCompleteAnalysis test4 SBottom
--runTest2 = runCompleteAnalysis test2 SBottom
--runTest = runCompleteAnalysis test SBottom
