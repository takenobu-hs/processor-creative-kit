
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}


module Processor.Tool.Assembler (
        -- * Assembler driver
          parseInst
        , parseInstFile
        -- * Assembler parse examples
        -- $parsenote
  ) where

-- Attoparsec
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.ByteString.Char8 as P8
import Control.Applicative

-- ByteString
import qualified Data.ByteString.Char8 as B

-- List
import Data.List (elemIndex, sortBy)
import Data.Char (toLower)

-- instruction
import Processor.Core.Instruction



------------------------------------------------------------
-- driver
------------------------------------------------------------

-- | parse instructions from ByteString
--
-- Example:
--
-- >  > parseInst (B.pack "mov r0,1\n halt\n")
-- >  [MOVI R0 1,HALT]
--
parseInst :: B.ByteString -> [Inst]
parseInst inp = case (parseOnly file (B.map toLower inp)) of
                  Right x -> x
                  _       -> error "parseInst: parse error"

-- | parse instructions from file
--
-- Example:
--
-- >  > parseInstFile "examples/test0.asm"
-- >  [MOVI R0 1,HALT]
--
parseInstFile :: FilePath -> IO [Inst]
parseInstFile f = B.readFile f >>= (return . parseInst)


------------------------------------------------------------
-- top
------------------------------------------------------------
file :: Parser [Inst]
file = do a <- many (skipElements >> instLine)
          skipElements >> endOfInput
          return a

type ParseInst = Parser Inst

instLine :: ParseInst
instLine = do skipSpaces
              a <- inst
              skipSpaces
              endOfLine <|> skipLineComment <|> skipRangeComment
              return a

------------------------------------------------------------
-- instructions
------------------------------------------------------------
inst :: ParseInst
inst = miscInsts
   <|> movInsts
   <|> arithInsts
   <|> logicInsts
   <|> jumpInsts
   <|> memInsts

miscInsts :: ParseInst
miscInsts = inst0 NOP  "nop"
        <|> inst0 HALT "halt"

movInsts :: ParseInst
movInsts = inst2 MOVI  "mov"  greg imm
       <|> inst2 MOV   "mov"  greg greg
       <|> inst2 movpc "mov"  greg pc

arithInsts :: ParseInst
arithInsts = inst3 ADD  "add" greg greg greg
         <|> inst3 SUB  "sub" greg greg greg
         <|> inst2 CMP  "cmp" greg greg
         <|> inst2 ABS  "abs" greg greg
         <|> inst3 ASH  "ash" greg greg greg
         <|> inst3 MUL  "mul" greg greg greg
         <|> inst3 DIV  "div" greg greg greg

logicInsts :: ParseInst
logicInsts = inst3 AND  "and" greg greg greg
         <|> inst3 OR   "or"  greg greg greg
         <|> inst2 NOT  "not" greg greg
         <|> inst3 XOR  "xor" greg greg greg
         <|> inst3 LSH  "lsh" greg greg greg

jumpInsts :: ParseInst
jumpInsts = inst2 BRI  "b"    fcond imm
        <|> inst1 JRI  "jmp"  imm
        <|> inst1 J    "jmp"  greg
        <|> inst1 CALL "call" greg
        <|> inst0 RET  "ret"

memInsts :: ParseInst
memInsts = inst2 LD "ld"  greg mem
       <|> inst2 ST "st"  mem  greg


-- asynmetric operand utility
movpc :: GReg -> b -> Inst
movpc a _ = MOVPC a

------------------------------------------------------------
-- instruction formats
------------------------------------------------------------
type F0       = Inst
type F1 a     = a -> Inst
type F2 a b   = a -> b -> Inst
type F3 a b c = a -> b -> c -> Inst

inst0 :: F0 -> B.ByteString -> ParseInst
inst0 f op = f <$ string op

inst1 :: F1 a -> B.ByteString -> Parser a -> ParseInst
inst1 f op p1 = f <$> (string op >> delimSpace >> p1)

inst2 :: F2 a b -> B.ByteString -> Parser a -> Parser b -> ParseInst
inst2 f op p1 p2 = f <$> (string op >> delimSpace >> p1)
                     <*> (delimComma >> p2)

inst3 :: F3 a b c -> B.ByteString -> Parser a -> Parser b -> Parser c
      -> ParseInst
inst3 f op p1 p2 p3 = f <$> (string op >> delimSpace >> p1)
                        <*> (delimComma >> p2)
                        <*> (delimComma >> p3)

------------------------------------------------------------
-- operand patterns
------------------------------------------------------------
-- general purpose register
greg :: Parser GReg
greg = do let reverseSortedGregNames = sortBy (flip compare) gregNames
          a <- choice $ map string reverseSortedGregNames
          return $ strToGReg a

-- pc
pc :: Parser ()
pc = do string "pc"
        return ()

-- flag condition
fcond :: Parser FCond
fcond = do a <- (string "eq" <|> string "ne" 
             <|> string "lt" <|> string "le" 
             <|> string "gt" <|> string "ge")
           return $ strToFCond (B.unpack a)

-- immidiate
imm :: Parser Int
imm = immMinus <|> immHex <|> immNoSign

immNoSign :: Parser Int
immNoSign = do d <- P.takeWhile1 (inClass "0123456789")
               return $ read (B.unpack d)

immMinus :: Parser Int
immMinus = do char8 '-'
              d <- P.takeWhile1 (inClass "0123456789")
              return $ read ('-' : B.unpack d)

immHex :: Parser Int
immHex = do string "0x"
            d <- P.takeWhile1 (inClass "0123456789abcdef")
            return $ read ("0x" ++ B.unpack d)


-- memory operand
mem :: Parser GReg
mem = do string "m(" >> skipSpaces
         a <- greg
         skipSpaces >> string ")"
         return a


-- converter utility
gregNames :: [B.ByteString]
gregNames  = map (B.pack . (map toLower) . show)
               [(minBound :: GReg) .. (maxBound :: GReg)]

strToGReg :: B.ByteString -> GReg
strToGReg  x = case (elemIndex x gregNames) of
                 Just n  -> toEnum n
                 Nothing -> error $ "strToGReg" ++ (show x)

strToFCond :: String -> FCond
strToFCond "eq" = FCEQ
strToFCond "ne" = FCNE
strToFCond "lt" = FCLT
strToFCond "le" = FCLE
strToFCond "gt" = FCGT
strToFCond "ge" = FCGE
strToFCond x      = error $ "strToFCond" ++ (show x)


------------------------------------------------------------
-- utility
------------------------------------------------------------
skipSpaces :: Parser ()
skipSpaces = skipWhile P8.isHorizontalSpace

delimSpace :: Parser ()
delimSpace = satisfy P8.isHorizontalSpace *> skipWhile P8.isHorizontalSpace

delimComma :: Parser ()
delimComma = do skipSpaces
                char8 ','
                skipSpaces



------------------------------------------------------------
-- comment and empty line
------------------------------------------------------------
skipElements :: Parser ()
skipElements = do many (skipLineComment <|> skipRangeComment <|> skipEmptyLine)
                  return ()

-- empty line
skipEmptyLine :: Parser ()
skipEmptyLine = do skipSpaces >> endOfLine
                   return ()

-- comment
skipLineComment :: Parser ()
skipLineComment =  do skipSpaces
                      string "#" >> manyTill P8.anyChar endOfLine
                      skipSpaces
                      return ()

skipRangeComment :: Parser ()
skipRangeComment = do skipSpaces
                      string "/*" >> manyTill P8.anyChar (string "*/")
                      skipSpaces
                      return ()


-- $parsenote
--
-- Parse Example:
--
-- text to 'Processor.Core.Instruction.Inst' data type
--
-- >  text               ->   Inst data type
-- >  ----------------------------------------
-- >  nop                ->   NOP
-- >  halt               ->   HALT
-- >  mov  r1, 100       ->   MOVI  R1  100
-- >  mov  r1, r2        ->   MOV   R1  R2
-- >  mov  r1, pc        ->   MOVPC R1
-- >  add  r1, r2, r3    ->   ADD   R1 R2 R3
-- >  sub  r1, r2, r3    ->   SUB   R1 R2 R3
-- >  cmp  r1, r2        ->   CMP   R1 R2
-- >  abs  r1, r2        ->   ABS   R1 R2
-- >  ash  r1, r2, r3    ->   ASH   R1 R2 R3
-- >  mul  r1, r2, r3    ->   MUL   R1 R2 R3
-- >  div  r1, r2, r3    ->   DIV   R1 R2 R3
-- >  and  r1, r2, r3    ->   AND   R1 R2 R3
-- >  or   r1, r2, r3    ->   OR    R1 R2 R3
-- >  not  r1, r2        ->   NOT   R1 R2
-- >  xor  r1, r2, r3    ->   XOR   R1 R2 R3
-- >  lsh  r1, r2, r3    ->   LSH   R1 R2 R3
-- >  b    eq, -3        ->   BRI   FCEQ (-3)
-- >  jmp  3             ->   JRI   3
-- >  jmp  r1            ->   J     R1
-- >  call r1            ->   CALL  R1
-- >  ret                ->   RET
-- >  ld   r1, m(r2)     ->   LD    R1 R2
-- >  st   m(r1), r2     ->   ST    R1 R2
--
-- Comment description:
--
-- >  # comment line
-- >  /* comment block */
--


