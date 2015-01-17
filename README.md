Processor-creative-kit
======================

This is a [haskell package](https://hackage.haskell.org/package/processor-creative-kit) for playing processor.

You can create your processor with your own instruction set.

enjoy! :smiley:


Contents
--------
  - [Summary] (#summary)
  - [Quick tour] (#quick-tour)
   1. [install] (#install)
   2. [run examples] (#run examples)
     - simple run
     - tracing run
     - profiling run
     - interactive debugger
   3. and more...
     - how to use API.
     - how to create your processor.

Summary
-------

### Feature
  - easy try, easy modify
  - Cpu/
    - a purely functional core (without IO)  (you can embed it anywhere)

  - Tool/
    - using monadic parser (Attoparsec)
    - independent design, Cpu core and Assembler format
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


### Acknowledgements
  - I was inspired from these packages:
  [HARM](https://hackage.haskell.org/package/HARM),
    [powerpc](https://hackage.haskell.org/package/powerpc),
    [ministg](https://hackage.haskell.org/package/ministg),
    [hython](https://github.com/mattgreen/hython).
  - and many processors, many tools. Thank you.


Quick tour
======================

(i) install
-----------

To expand source code in your working directory:

```
$ cd YOUR_WORK_DIRECTORY
$ cabal unpack @@@
```

or


```
$ tar xvzf @@@.tar.gz
```

Then, install the dependent packages:

```
$ cabal install --only-dependencies
```


(ii) run examples
-----------------

### run
```
$ runhaskell examples/run.hs examples/test0.asm
```

result:
```
@@@
```


### tracing run
```
$ runhaskell examples/trace.hs examples/test0.asm
```


### profiling run
```
$ runhaskell examples/prof.hs examples/test0.asm
```



### interactive debugger
```
$ runhaskell examples/idb.hs examples/test0.asm

@@@@@@
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

