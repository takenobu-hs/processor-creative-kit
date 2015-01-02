Processor-creative-kit
======================

This is a [haskell package](https://hackage.haskell.org/package/processor-creative-kit) for playing processor.

You can create your processor with your own instruction set.

enjoy! :smiley:


Contents
--------
  - [Summary] (#summary)
  - [Quick tour] (#quick-tour)
   1. [simple run] (#simple run)
     - run
     - run with initial data memory
     - run with assembly file
   2. [advanced run] (#advanced run)
     - tracing run
     - breaking run
     - profiling run
     - interactive debugger
   3. [create your processor] (#create your processor)
     - add your instruction


Summary
-------

### Feature
  - easy try, easy modify
  - Core/
    - a purely functional core (without IO)  (you can embed it anywhere)

  - Tool/
    - using monadic parser (Attoparsec)
    - independent design, Core machine and Assembler format
    - including very simple prototype assembler
    - including very simple prototype debugger
    - including very simple prototype profiler


### Default processor architecture
  - Harvard architecture. (instruction and data memories are splited)
  - fixed length instruction (word length)
  - word addressing (no byte addressing)
  - ideal immediate lengh (settable word immediate by 1 instruction)
  - no MMU, cache, privilege level, interruption, I/O, and any


### Limitation
  - using slow container(Data.Array) for simple implementation.
  - assembly error messages are unkindness.


### Acknowledge
  - I was inspired from these packages:
  [HARM](https://hackage.haskell.org/package/HARM),
    [powerpc](https://hackage.haskell.org/package/powerpc),
    [ministg](https://hackage.haskell.org/package/ministg),
    [hython](https://github.com/mattgreen/hython).
  - and many processors, many tools. Thank you.


Quick tour
======================

(i) simple run
---------------------

### run
code:
```haskell
% ghci
> import Processor.Core
> run [(0, [MOVI R0 20, HALT])] []
```

result:
```
pc : 1
gr : [20,0,0,0,0,0,0,0]
fl : [False,False]
```

### run with initial data memory

code:
```haskell
run [(0, testpro)]  [(100, [10, 20])]

testpro  = [ MOVI R4 100
           , MOVI R5 101
           , LD   R1 R4
           , LD   R2 R5
           , ADD  R0 R1 R2
           , HALT ]
```

result:
```
pc : 5
gr : [30,10,20,0,100,101,0,0]
fl : [False,False]
```


### run with assembly file

code:
```haskell
testpro = parseInstFile "test.asm"
run [(0, testpro)] []
```

[test.asm] file :
```
  mov  r4, 100
  mov  r5, 101
  ld   r1, m(r4)
  ld   r2, m(r5)
  add  r0, r1, r2
  halt
```


(ii) advanced run
---------------------

### tracing run

code:
```haskell
runDbg [TrcPc, TrcInst] []  [(0, testpro)]  [(100, [10, 20])]
runDbgIO [TrcPc, TrcInst] []  [(0, testpro)]  [(100, [10, 20])]
```

result:
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





### breaking run

code:
```haskell
runDbg [TrcInst] [(BrkPc BEQ 2)]  [(0, testpro)]  [(100, [10, 20])]
runDbgIO [TrcInst] [(BrkPc BEQ 2)]  [(0, testpro)]  [(100, [10, 20])]
```

result:
```
TrcInst:        pc : 0  MOVI R4 100

TrcInst:        pc : 1  MOVI R5 101
```



### profiling run

code:
```haskell
prof [ProfInst] $ runDbg [TrcInst] []  [(0, testpro)]  [(100, [10, 20])]

or

runProfIO [ProfInst]  [(0, testpro)] [(100, [10, 20])]
```

result:
```
instruction profile:

  MOVI  2
  LD    2
  HALT  1
  ADD   1

  total 6
```

code:
```haskell
runProfIO [ProfLoda]  [(0, testpro)] [(100, [10, 20])]
```

result:
```
Memory load address profile:

  address       count
  0x00000065    1
  0x00000064    1

  total         2
```



### interactive debugger

code:
```haskell
runIdbIO [TrcInst] [(BrkPc BEQ 2)]  [(0, testpro)]  [(100, [10, 20])]
```

interactive command and result:
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


(iii) create your processor
---------------------

### add your instruction

example for adding "NEG" instruction.

insert following lines:

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
