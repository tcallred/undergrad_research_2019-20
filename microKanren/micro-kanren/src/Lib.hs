module Lib
    () where


data Var = Var Int deriving (Show, Eq)


data Val = 
    Int Int
    | Str String
    | VarVal Var
    | Pair (Val, Val)
    | Nil
    deriving (Show, Eq)


data SubMap = 
    Empty
    | KeyValue { sKey :: Var, sValue :: Val, sRest :: SubMap}
    deriving (Show)


data State = 
    State { sMap :: SubMap, varCnt :: Int }


data Stream =
    MZero
    | Unit State


type Goal = State -> Stream


isVar :: Val -> Bool
isVar v =
    case v of
        VarVal _ -> True

        _ -> False


extend :: Var -> Val -> SubMap -> SubMap
extend x v s = KeyValue x v s


walk :: SubMap -> Val -> Val
walk sub k = 
    -- Take a key as a val. If it's a var, find its value
    -- If it's any other val, just return itself
    case k of
        VarVal var -> 
            case sub of
                KeyValue key value rest -> 
                    if key == var then 
                        value 
                    else 
                        walk rest k
                
                Empty -> k

        _ -> k


(<=>) :: Val -> Val -> Goal
left <=> right = 
    (\st -> 
        case unify left right (sMap st) of
            Just sm -> Unit $ State sm (varCnt st)

            Nothing -> MZero
    )


unify :: Val -> Val -> SubMap -> Maybe SubMap
unify left right sm =
    case (u, v) of
        (VarVal u, VarVal v) -> 
            if u == v then
                Just sm
            else
                Nothing

        (VarVal u, v) ->
            Just $ extend u v sm 
        
        (u, VarVal v) ->
            Just $ extend v u sm
        
        (u, v) ->
            if u == v then
                Just sm
            else
                Nothing

    where
        u = walk sm left
        v = walk sm right 


callFresh :: (Var -> Goal) -> Goal
callFresh f = (\st -> (f (Var $ varCnt st)) (State (sMap st) ((varCnt st) + 1) ) )