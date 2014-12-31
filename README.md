Processor-creative-kit
======================

This is a haskell package, for playing processor; [processor-creative-kit](https://hackage.haskell.org/package/processor-creative-kit).

You can create your processor with your own instruction set.

enjoy! :smiley:


Contents
--------
  - [Summary] (#summary)
  - [Quick tour] (#quick-tour)


Summary
-------

### Feature
  - easy try, easy modify
  - Core/
    - a purely evaluation core (without IO)  (you can embed it anywhere)

  - Tool/
    - using monadic parser (Attoparsec)
    - independent design, Core machine and Assembler format
    - include very simple prototype assembler
    - include very simple prototype debugger
    - include very simple prototype profiler


### Default processor architecture
  - harvard arch. (split imem and dmem)
  - fixed length instruction (word length)
  - word addressing (no byte addressing)
  - ideal immediate lengh (settable word immediate by 1 instruction)
  - no MMU, cache, privilege level, interruption, I/O, and any


### Limitation
  - using slow container(Data.Array) for simple implementation.
  - assembler error message is unkindness.


### Acknowledge
  - [HARM](https://hackage.haskell.org/package/HARM),
    [powerpc](https://hackage.haskell.org/package/powerpc),
    [ministg](https://hackage.haskell.org/package/ministg),
    [hython](https://github.com/mattgreen/hython) packages.
    and many processors, many tools.


Quick tour
======================

 1. simple run
   - simple run
   - run with initial data memory
   - run with assembler file
 2. create your processor
   - add your instruction
 3. advance
   - run with trace print
   - debugging run
   - profiling
   - interactive debugger


simple run
---------------------

### simple run

```haskell
% ghci
> import Processor.Core
> run [(0, [MOVI R0 20, HALT])] []
```

```
pc : 1
gr : [20,0,0,0,0,0,0,0]
fl : [False,False]
```

### run with initial data memory

```haskell
run [(0, testpro)]  [(100, [10, 20])]

testpro  = [ MOVI R4 100
           , MOVI R5 101
           , LD   R1 R4
           , LD   R2 R5
           , ADD  R0 R1 R2
           , HALT ]
```

```
pc : 5
gr : [30,10,20,0,100,101,0,0]
fl : [False,False]
```


### run with assembler file

```haskell
testpro = parseInstFile "test.asm"
run [(0, testpro)] []
```

```
[test.asm]
  mov  r4, 100
  mov  r5, 101
  ld   r1, m(r4)
  ld   r2, m(r5)
  add  r0, r1, r2
  halt
```


create your processor
---------------------

### add your instruction

example : add "NEG" instruction.

[Processor/Core/Instruction.hs]
```haskell
          | NEG   GReg GReg
```

[Processor/Core/Execution.hs]
```haskell
evalStep (NEG   ra rb)    = uniopInst (neg) ra rb
```

[Processor/Tool/Assembler.hs]
```haskell
         <|> inst2 NEG  "neg" greg greg
```



advance
---------------------

### run with trace print

```haskell
runDbg [TrcPc, TrcInst] []  [(0, testpro)]  [(100, [10, 20])]
runDbgIO [TrcPc, TrcInst] []  [(0, testpro)]  [(100, [10, 20])]
```

```
TrcPc:  pc : 0
TrcInst:        pc : 0  MOVI R4 100

TrcPc:  pc : 1
TrcInst:        pc : 1  MOVI R5 101

TrcPc:  pc : 2
TrcInst:        pc : 2  LD R1 R4

TrcPc:  pc : 3
TrcInst:        pc : 3  LD R2 R5

TrcPc:  pc : 4
TrcInst:        pc : 4  ADD R0 R1 R2

TrcPc:  pc : 5
TrcInst:        pc : 5  HALT
```





### debugging run

```haskell
runDbg [TrcInst] [(BrkPc BEQ 2)]  [(0, testpro)]  [(100, [10, 20])]
runDbgIO [TrcInst] [(BrkPc BEQ 2)]  [(0, testpro)]  [(100, [10, 20])]
```

```
TrcInst:        pc : 0  MOVI R4 100

TrcInst:        pc : 1  MOVI R5 101
```



### profiling

```haskell
prof [ProfInst] $ runDbg [TrcInst] []  [(0, testpro)]  [(100, [10, 20])]

or

runProfIO [ProfInst]  [(0, testpro)] [(100, [10, 20])]
```

```
instruction profile:

  MOVI  2
  LD    2
  HALT  1
  ADD   1

  total 6
```


```haskell
runProfIO [ProfLoda]  [(0, testpro)] [(100, [10, 20])]
```

```
Memory load address profile:

  address       count
  0x00000065    1
  0x00000064    1

  total         2
```



### interactive debugger

```haskell
runIdbIO [TrcInst] [(BrkPc BEQ 2)]  [(0, testpro)]  [(100, [10, 20])]
```

```
(idb) run
TrcInst:        pc : 0  MOVI R4 100

TrcInst:        pc : 1  MOVI R5 101

(idb) info reg
pc : 2
gr : [0,0,0,0,100,101,0,0]
fl : [False,False]

(idb) x/8 0
0x00000000: 0x00000000 0x00000000 0x00000000 0x00000000
0x00000004: 0x00000000 0x00000000 0x00000000 0x00000000

(idb) s
TrcInst:        pc : 2  LD R1 R4

(idb) s
TrcInst:        pc : 3  LD R2 R5

(idb) c
TrcInst:        pc : 4  ADD R0 R1 R2

TrcInst:        pc : 5  HALT

(idb) q
```




