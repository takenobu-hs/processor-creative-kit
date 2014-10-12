
{-# OPTIONS -Wall #-}


module Processor.Core.Instruction (
        -- * Instructin set type
          Inst(..)
        , GReg(..)
        , FCond(..)
  ) where

import Data.Array (Ix)


----------------------------------------
--  instruction set
----------------------------------------
-- | Instruction define.
--
--   You can create instructions as you like :-)
--
--   Operand order is Intel, ARM, MIPS, PowerPC,... order. 
--   (opcode dst src1 src2)
data Inst = NOP                    -- ^ no operation
          | HALT                   -- ^ halt (stop processor)

          | MOVI  GReg Int         -- ^ GReg <- Int
          | MOV   GReg GReg        -- ^ GReg <- GReg
          | MOVPC GReg             -- ^ GReg <- PC

          | ADD   GReg GReg GReg   -- ^ GReg <- GReg + GReg
          | SUB   GReg GReg GReg   -- ^ GReg <- GReg - GReg
          | CMP   GReg GReg        -- ^ Flag <- compare(GReg, GReg)
          | ABS   GReg GReg        -- ^ GReg <- abs(GReg)
          | ASH   GReg GReg GReg   -- ^ GReg <- GReg << GReg // arithmetic shift
          | MUL   GReg GReg GReg   -- ^ GReg <- GReg * GReg
          | DIV   GReg GReg GReg   -- ^ GReg <- GReg / GReg

          | AND   GReg GReg GReg   -- ^ GReg <- GReg & GReg
          | OR    GReg GReg GReg   -- ^ GReg <- GReg | GReg
          | NOT   GReg GReg        -- ^ GReg <- ~GReg
          | XOR   GReg GReg GReg   -- ^ GReg <- GReg ^ GReg
          | LSH   GReg GReg GReg   -- ^ GReg <- GReg << GReg // logical shift

          | BRI   FCond Int        -- ^ if (FCond(Flag)) goto (PC + Int)
                                   --     // pc relative addressing
          | JRI   Int              -- ^ goto (PC + Int)
                                   --     // pc relative addressing
          | J     GReg             -- ^ goto GReg
                                   --     // absolute addressing
          | CALL  GReg             -- ^ goto GReg; R0 <- PC
                                   --     // absolute addressing
          | RET                    -- ^ goto R0

          | LD    GReg GReg        -- ^ GReg <- memory(GReg)
          | ST    GReg GReg        -- ^ memory(GReg) <- GReg

          | UNDEF                  -- ^ undefined
          deriving (Show, Eq)



-- | General purpose registers.
--
--   You can create registers as you like :-)
data GReg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
              deriving (Show, Eq, Ord, Ix, Enum, Bounded)


-- | Flag conditions
data FCond = FCEQ  -- ^ equal
           | FCNE  -- ^ not equal
           | FCLT  -- ^ little than
           | FCLE  -- ^ little equal
           | FCGT  -- ^ greater than
           | FCGE  -- ^ greater eaual
           deriving (Show, Eq)




