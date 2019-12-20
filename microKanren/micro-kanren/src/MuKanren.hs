module MuKanren
    (callFresh,
    conj,
    disj,
    (<=>),) where


import Data.Map


data Val = 
    -- Literals
    NumLit 
    | StringLit
    | X
    -- Expression constructors
    | Lam Val Val
    | Ref Val
    | App Val Val
    | BinOp Val Val
    -- Type Constructors
    | BoolT
    | NumT
    | StringT
    | ArrowT Val Val
    -- Type env
    | TECons Val Val Val
    | TEEmpty
    -- MuKanren specific
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


disj :: Goal -> Goal -> Goal
disj g1 g2 = (\st -> mplus (g1 st) (g2 st))


conj :: Goal -> Goal -> Goal
conj g1 g2 = (\st -> bind (g1 st) g2)


mplus :: Result -> Result -> Result
mplus r1 r2 = r1 ++ r2 


bind :: Result -> Goal -> Result
bind [] g = []
bind (state : states) g = mplus (g state) (bind states g)


reify :: Val -> Substitution -> Val
reify t sub =
    let val = walk t sub

    in
        case val of
            Var v ->
                Var v
            
            Pair (t0, t1) ->
                Pair (reify t0 sub, reify t1 sub)
            
            v ->
                v

run :: Goal -> [Val]
run g = 
    Prelude.map 
        (\State{sMap=sub, varCnt=n} -> 
            
            let v = case Data.Map.lookup 0 sub of Just val -> val; Nothing -> Nil
            in
                reify v sub) 
        
        (g emptyState)


proves gamma e a =

    -- The type check proves if any of the following goals succeed 
    -- Note: When I say these "if...then" statements, what I really mean is "all these statements must hold at once" because that is the definition of conj
    disj 

    -- First, if the expression e unifies with a literal term then the type unifies with its type
    -- In this case only Number literals are considered
    (callFresh 
        (\n -> conj (e <=> NumLit) (a <=> NumT)))

    -- If the expression is an application of some expression e0 to another expression e1
    -- then gamma proves that e0 has type ArrowT from some type b to a
    -- and gamma proves that e1 is of some type b
    (disj 
    (callFresh 
        (\e0 -> 
            callFresh 
                (\e1 -> 
                    callFresh 
                        (\b -> 
                            conj (e <=> App e0 e1) 
                                 (conj (proves gamma e0 (ArrowT b a)) 
                                 (proves gamma e1 b)))))) 
  
    -- This is the big work horse
    -- If e is a lambda expression with formal parameter x and inner expression e0
    -- then gamma can be extended to be gamma'
    -- gamma' proves that the type of e0 is c
    -- and a is of type Arrow from fresh var b to fresh var c
    (disj 
    (callFresh 
        (\x ->
            callFresh 
                (\e0 -> 
                    callFresh
                        (\b -> 
                            callFresh
                                (\c ->
                                    callFresh 
                                        (\gamma' -> 
                                            conj (e <=> Lam x e0) 
                                                 (conj (extendEnv gamma x b gamma' )
                                                 (conj (proves gamma' e0 c)
                                                 (a <=> ArrowT b c)))
                                            ))))))

    -- IF e is a reference to a variable x then x can be looked up in gamma and the type will be a
    (callFresh 
        (\x -> 
            conj (e <=> Ref x)
                 (MuKanren.lookup gamma x a)))))

    
-- Extending an environment gamma is as simple and unifying this new variable gamma' with a new environment that is the 
-- construction of a new key and value with the old environment gamma                                            
extendEnv gamma x a gamma' = gamma' <=> TECons x a gamma 


lookup gamma x a =
    callFresh
        (\gamma' -> 

            -- "Looking up key x in gamma is of type a" succeeds if
            -- Gamma unifies with some environment containing this exact key value pair at its head
            -- OR gamma has some arbitrary key-value pair y and b at its head and with a new variable gamma', x, and a lookup succeeds
            disj (gamma <=> TECons x a gamma')
                  (callFresh
                    (\y ->
                        callFresh
                            (\b -> 
                                conj (gamma <=> TECons y b gamma')
                                     (MuKanren.lookup gamma' x a)))))

