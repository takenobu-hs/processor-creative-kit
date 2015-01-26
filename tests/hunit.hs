
{-# OPTIONS -Wall #-}

{-
  % runhaskell tests/hunit.hs
-}


module Main where

import Test.HUnit
import System.IO
import qualified Data.ByteString.Char8 as B

import Language.Pck.Cpu
import Language.Pck.Tool



main :: IO (Counts, Int)
main = runTestText (putTextToHandle stderr False) tests


tests :: Test
tests = TestList [
        -- Cpu
          "gr test" ~:
              (head . grFromCpuState $ run [(0,[MOVI R0 7, HALT])] [])
              ~?= 7

        , "store test" ~:
              (head . snd . head . dmemFromCpuState $
                 run [(0,[MOVI R1 0, MOVI R2 8, ST R1 R2, HALT])] [])
              ~?= 8

        , "load test" ~:
              (head . grFromCpuState $
                 run [(0,[MOVI R1 0, LD R0 R1, HALT])] [(0,[10])])
              ~?= 10

        , "branch test" ~:
              (head . grFromCpuState $
                 run [(0,[MOVI R0 0, MOVI R1 0, CMP R0 R1, BRI FCEQ 2, HALT,
                          MOVI R0 11, HALT])] [])
              ~?= 11

        -- Debugger
        , "debug basic test" ~:
              (pcFromCpuState . snd $
                 runDbg [TrcInst, TrcReg] [(BrkPc BEQ 0)]
                        [(0,[MOVI R0 7, HALT])] [])
              ~?= 1

        -- Assembler
        , "basic nop test" ~:
              parseInst (B.pack "nop\n")             ~?= Right [NOP]

        , "basic halt test" ~:
              parseInst (B.pack "halt\n")            ~?= Right [HALT]
        , "basic movi test" ~:
              parseInst (B.pack "mov  r1, 100\n")    ~?= Right [MOVI  R1  100]
        , "basic mov test" ~:
              parseInst (B.pack "mov  r1, r2\n")     ~?= Right [MOV   R1  R2]
        , "basic movpc test" ~:
              parseInst (B.pack "mov  r1, pc\n")     ~?= Right [MOVPC R1]
        , "basic add test" ~:
              parseInst (B.pack "add  r1, r2, r3\n") ~?= Right [ADD   R1 R2 R3]
        , "basic sub test" ~:
              parseInst (B.pack "sub  r1, r2, r3\n") ~?= Right [SUB   R1 R2 R3]
        , "basic cmp test" ~:
              parseInst (B.pack "cmp  r1, r2\n")     ~?= Right [CMP   R1 R2]
        , "basic abs test" ~:
              parseInst (B.pack "abs  r1, r2\n")     ~?= Right [ABS   R1 R2]
        , "basic ash test" ~:
              parseInst (B.pack "ash  r1, r2, r3\n") ~?= Right [ASH   R1 R2 R3]
        , "basic mul test" ~:
              parseInst (B.pack "mul  r1, r2, r3\n") ~?= Right [MUL   R1 R2 R3]
        , "basic div test" ~:
              parseInst (B.pack "div  r1, r2, r3\n") ~?= Right [DIV   R1 R2 R3]
        , "basic and test" ~:
              parseInst (B.pack "and  r1, r2, r3\n") ~?= Right [AND   R1 R2 R3]
        , "basic or test" ~:
              parseInst (B.pack "or   r1, r2, r3\n") ~?= Right [OR    R1 R2 R3]
        , "basic not test" ~:
              parseInst (B.pack "not  r1, r2\n")     ~?= Right [NOT   R1 R2]
        , "basic xor test" ~:
              parseInst (B.pack "xor  r1, r2, r3\n") ~?= Right [XOR   R1 R2 R3]
        , "basic lsh test" ~:
              parseInst (B.pack "lsh  r1, r2, r3\n") ~?= Right [LSH   R1 R2 R3]
        , "basic bri test" ~:
              parseInst (B.pack "b    eq, -3\n")     ~?= Right [BRI   FCEQ (-3)]
        , "basic jri test" ~:
              parseInst (B.pack "jmp  3\n")          ~?= Right [JRI   3]
        , "basic j test" ~:
              parseInst (B.pack "jmp  r1\n")         ~?= Right [J     R1]
        , "basic call test" ~:
              parseInst (B.pack "call r1\n")         ~?= Right [CALL  R1]
        , "basic ret test" ~:
              parseInst (B.pack "ret\n")             ~?= Right [RET]
        , "basic ld test" ~:
              parseInst (B.pack "ld   r1, m(r2)\n")  ~?= Right [LD    R1 R2]
        , "basic st test" ~:
              parseInst (B.pack "st   m(r1), r2\n")  ~?= Right [ST    R1 R2]

        , "parse and run test" ~:
              (head . grFromCpuState $
                 (let Right inst = parseInst (B.pack "mov r0,7\nhalt\n")
                  in  run [(0,inst)] [] ))
              ~?= 7

        -- IO
        , "debug trace io otest" ~:
              (do a <- runDbgIO [TrcInst, TrcReg] [(BrkPc BEQ 0)]
                            [(0,[MOVI R0 7, HALT])] []
                  a @?= ())

        , "profile io otest" ~:
              (do a <- runProfIO
                           [ProfInst, ProfCall, ProfLoad, ProfStore, ProfBranch]
                           [(0,[MOVI R0 7, HALT])] []
                  a @?= ())

        ]



