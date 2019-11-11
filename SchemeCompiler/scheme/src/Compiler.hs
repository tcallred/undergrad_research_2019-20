module Compiler
    ( compileProgram
    , Expr(Immediate, Primcall)
    , Immediate(Integer, Character, Boolean, EmptyList)
    , Primcall(Add1, Sub1)
    ) where


import Data.Word
import Data.Bits
import Data.Char


data Expr =
    Immediate Immediate
    | Primcall Primcall
    

data Immediate =
    Integer Int
    | Character Char
    | Boolean Bool
    | EmptyList 


data Primcall =
    Add1 Expr
    | Sub1 Expr


compileProgram :: Expr -> [String]
compileProgram x = emitExpr x


emitExpr :: Expr -> [String]
emitExpr x =
    case x of
        Immediate immediate -> 
            ["movl $" ++ (show . immediateRep) immediate ++ ", %eax"]
    
        Primcall primcall ->
            case primcall of
                Add1 operand ->
                    (emitExpr operand) ++ ["addl $" ++ (show . immediateRep) (Integer 1) ++ ", %eax"]
                
                Sub1 operand ->
                    (emitExpr operand) ++ ["subl $" ++ (show . immediateRep) (Integer 1) ++ ", %eax"]


immediateRep :: Immediate -> Word32
immediateRep immediate =
    case immediate of
        Integer x ->
            shift (fromIntegral x) fixNumShift
        
        Character c ->
            shift ((fromIntegral.ord) c) charShift .|. charTag

        Boolean b ->
            shift (boolAsInt b) boolShift .|. boolTag
        
        EmptyList ->
            fromIntegral emptyList
    
    where 
        fixNumShift = 2
        charShift = 8
        charTag = 15
        boolAsInt b = if b then 1 else 0
        boolShift = 7
        boolTag = 31
        emptyList = 47


