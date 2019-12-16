module Lib
    () where


import Data.Map


data Val = 
    Num Int
    | Str String
    | Var Int
    | Pair (Val, Val)
    | Nil
    deriving (Show, Eq)


type Substitution = Map Int Val 


data State = 
    State { sMap :: Substitution, varCnt :: Int }
    deriving (Show)


emptyState :: State
emptyState = State Data.Map.empty 0


type Result = [State]


type Goal = State -> Result


extend :: Int -> Val -> Substitution -> Substitution
extend x v s = Data.Map.insert x v s


walk :: Val -> Substitution -> Val
walk v sub  = 
    let pr = case v of
                Var i -> Data.Map.lookup i sub
                _ -> Nothing

    in
        case pr of
            Just v' -> 
                walk v' sub

            Nothing -> 
                v


(<=>) :: Val -> Val -> Goal
left <=> right = 
    (\st -> 
        case unify left right (sMap st) of
            Just sm -> [State sm (varCnt st)]

            Nothing -> []
    )


unify :: Val -> Val -> Substitution -> Maybe Substitution
unify left right sm =
    case (u, v) of
        (Var u, Var v) | u == v -> 
            Just sm

        (Var u, v) ->
            Just $ extend u v sm 
        
        (u, Var v) ->
            Just $ extend v u sm
        
        (Pair (u1, u2), Pair (v1, v2)) ->
            case unify u1 v1 sm of
                Just sub' -> 
                    unify u2 v2 sub'
                
                Nothing ->
                    Nothing

        (u, v) ->
            if u == v then
                Just sm
            else
                Nothing

    where
        u = walk left sm
        v = walk right sm


callFresh :: (Val -> Goal) -> Goal
callFresh f = 
    (\st ->
        let 
            var   = Var $ varCnt st
            state = State (sMap st) ((varCnt st) + 1)

        in 
            (f var) state
    )


-- disj :: Goal -> Goal -> Goal
-- disj g1 g2 = (\st -> mplus (g1 st) (g2 st))


-- conj :: Goal -> Goal -> Goal
-- conj g1 g2 = (\st -> bind (g1 st) g2)


-- data Hole = Hole
-- hole = undefined


-- mplus :: Hole
-- mplus = hole


-- bind :: Hole
-- bind = hole