module Compiler
    ( compileProgram
    , Expr(Immediate)
    , Immediate(Integer, Character)
    ) where


import Data.Word
import Data.Bits
import Data.Char


data Immediate =
    Integer Int
    | Character Char
    

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
    
    where 
        fixNumShift = 2
        charShift = 8
        charTag = 7