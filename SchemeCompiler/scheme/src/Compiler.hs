module Compiler
    ( compileProgram
    , Expr(Immediate)
    , Immediate(Integer, Character, Boolean, EmptyList)
    ) where


import Data.Word
import Data.Bits
import Data.Char


data Immediate =
    Integer Int
    | Character Char
    | Boolean Bool
    | EmptyList 

newtype Expr =
    Immediate Immediate
    

compileProgram :: Expr -> [String]
compileProgram x =
    case x of
        Immediate immediate -> 
            ("movl $" ++ (show . immediateRep) immediate ++ ", %eax") : ["ret"]
        

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