How to create your processor
============================

Contents
--------
  - add instructions
    - add an negative instruction (`neg r0,r1`)
    - add a special mul (2x+y) instruction (`mul3 r0,r1,r2`)
    - add a register swap instruction (`swap r0,r1`)
    - add a memory copy instruction (`memcpy m(r0),m(r1)`)
  - modify configurations
    - modify registers number
    - modify registers name
    - modify a link register
    - modify instruction/data memory configurations
  - modify assembler formats
    - modify commment formats
    - modify memory operand formats
    - modify immediate operand formats
    - modify register operand formats


add instructions
----------------

**add an negative instruction (`neg r0,r1`)**

insert following lines:

internal representation on machine(cpu)

[Language/Pck/Cpu/Instruction.hs]
```haskell
          | NEG   GReg GReg
```

internal behavior on machine(cpu)

[Language/Pck/Cpu/Execution.hs]
```haskell
evalStep (NEG   ra rb)    = uniopInst (*(-1)) ra rb
```

assembler format

[Language/Pck/Tool/Assembler.hs]
```haskell
         <|> inst2 NEG  "neg" greg greg
```


**add a special mul (2x+y) instruction (`mul3 r0,r1,r2`)**

insert following lines:

[Language/Pck/Cpu/Instruction.hs]
```haskell
          | MUL3  GReg GReg GReg
```

[Language/Pck/Cpu/Execution.hs]
```haskell
evalStep (MUL3  ra rb rc) = biopInst (\b c -> b*2 + c) ra rb rc
```

[Language/Pck/Tool/Assembler.hs]
```haskell
         <|> inst3 MUL3 "mul3" greg greg greg
```


**add a register swap instruction (`swap r0,r1`)**

insert following lines:

[Language/Pck/Cpu/Instruction.hs]
```haskell
          | SWAP  GReg GReg
```

[Language/Pck/Cpu/Execution.hs]
```haskell
evalStep (SWAP  ra rb)    = swap ra rb
```
```haskell
swap :: GReg -> GReg -> EvalCpu ResultStat
swap ra rb = do va <- readGReg ra
                vb <- readGReg rb
                updateGReg ra vb
                updateGReg rb va
                incPc
```

[Language/Pck/Tool/Assembler.hs]
```haskell
       <|> inst2 SWAP  "swap" greg greg
```


**add a memory copy instruction (`memcpy m(r0),m(r1)`)**

insert following lines:

[Language/Pck/Cpu/Instruction.hs]
```haskell
          | MEMCPY GReg GReg
```

[Language/Pck/Cpu/Execution.hs]
```haskell
evalStep (MEMCPY ra rb)   = memcpy ra rb
```
```haskell
memcpy :: GReg -> GReg -> EvalCpu ResultStat
memcpy ra rb = do va <- readGReg ra
                  vb <- readGReg rb
                  vm <- readDmem vb
                  updateDmem va vm
                  incPc
```

[Language/Pck/Tool/Assembler.hs]
```haskell
       <|> inst2 MEMCPY "memcpy"  mem mem
```




modify configurations
---------------------

**modify registers number**

[Language/Pck/Cpu/Instruction.hs]
```haskell
data GReg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9
```


**modify registers name**

[Language/Pck/Cpu/Instruction.hs]
```haskell
data GReg = EA | EB | EC | ED
```

**modify a link register**

[Language/Pck/Cpu/Execution.hs]
```haskell
linkReg = minBound::GReg  -- default if R0
```
[Language/Pck/Cpu/Execution.hs]
```haskell
linkReg = R7
```

**modify instruction/data memory configurations**

[Language/Pck/Cpu/Config.hs]
```haskell
cpuConfig = CpuConfig
            { cfgStartPc = 0        -- boot(start) address
            , cfgImemStart = 0      -- instruction memory first address
            , cfgImemSize  = 256    -- instruction memory size
            , cfgDmemStart = 0      -- data memory first address
            , cfgDmemSize  = 256    -- data instruction memory size
            }

```


modify assembler formats
------------------------

**modify commment formats**

[Language/Pck/Tool/Assembler.hs]
```haskell
strCmntLine = "#"
strCmntRangeBeg = "/*"
strCmntRangeEnd = "*/"
```

```haskell
strCmntLine = "--"
strCmntRangeBeg = "{-"
strCmntRangeEnd = "-}"
```

**modify memory operand formats**

[Language/Pck/Tool/Assembler.hs]
```haskell
mem = do string "m(" >> skipSpaces

```
```haskell
mem = do string "(" >> skipSpaces
```

**modify immediate operand formats**

[Language/Pck/Tool/Assembler.hs]
```haskell
imm = immMinus <|> immHex <|> immNoSign
```
```haskell
imm = do char8 '$'    -- add here
         immMinus <|> immHex <|> immNoSign
```

**modify register operand formats**

[Language/Pck/Tool/Assembler.hs]
```haskell
greg = do let reverseSortedGregNames = sortBy (flip compare) gregNames
          a <- choice $ map string reverseSortedGregNames
          return $ strToGReg a
```

```haskell
greg = do char8 '%'    -- add here
          let reverseSortedGregNames = sortBy (flip compare) gregNames
          a <- choice $ m
```

