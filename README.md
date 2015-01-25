Processor-creative-kit
======================

This is a [haskell package][1] for playing processor.

You can create your processors with your own instruction set and cpu simulator and development tools.

enjoy! :smiley:


Summary
-------

Feature
  - easy try, easy modify
  - a purely functional CPU core (without IO)  (you can embed it anywhere)
  - including very simple prototype assembler
  - including very simple prototype debugger
  - including very simple prototype profiler

Acknowledgements
  - I was inspired from these packages:
    [HARM][2],
    [powerpc][3],
    [ministg][4],
    [hython][5].
  - and many processors, many tools. Thank you.




Quick tour
======================

### (i) install

To expand source code in your working directory:

    $ cd YOUR_WORK_DIRECTORY
    $ cabal unpack processor-creative-kit

or

    $ tar xvzf processor-creative-kit.tar.gz

Then, install the dependent packages:

    $ cabal install --only-dependencies


### (ii) run examples

**run**

    $ runhaskell examples/run.hs examples/test0.asm

result:

    pc : 3
    gr : [0,100,200,300,0,0,0,0]
    fl : [False,False]
    dm : [(0,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])]


**tracing run**

    $ runhaskell examples/trace.hs examples/test0.asm

result:

    TrcInst:        pc : 0x0        MOVI R1 100
    
    TrcInst:        pc : 0x1        MOVI R2 200
    
    TrcInst:        pc : 0x2        ADD R3 R1 R2
    
    TrcInst:        pc : 0x3        HALT


**profiling run**

    $ runhaskell examples/prof.hs examples/test0.asm

result:

    instruction profile:
    
      MOVI  2
      ADD   1
      HALT  1
    
      total 4
    
    
    Call target profile:
    
      address       count
    (snip)


**interactive debugger**

    $ runhaskell examples/idb.hs examples/test0.asm

result:

    For help, type "help".
    
    (idb) run
    TrcInst:        pc : 0x0        MOVI R1 100
    
    TrcInst:        pc : 0x1        MOVI R2 200
    
    TrcInst:        pc : 0x2        ADD R3 R1 R2
    
    TrcInst:        pc : 0x3        HALT
    
    (idb) info reg
    pc : 3
    gr : [0,100,200,300,0,0,0,0]
    fl : [False,False]
    
    (idb) x/8 0
    0x00000000: 0x00000000 0x00000000 0x00000000 0x00000000
    0x00000004: 0x00000000 0x00000000 0x00000000 0x00000000
    
    (idb) b 1
    Num  Enb What
    1    y   PC == 1  (PC == 0x1)
    
    (idb) run
    TrcInst:        pc : 0x0        MOVI R1 100
    
    (idb) s
    TrcInst:        pc : 0x1        MOVI R2 200
    
    (idb) s
    TrcInst:        pc : 0x2        ADD R3 R1 R2
    
    (idb) help
    List of commands:
    
    q       -- Exit debugger
    help    -- Print list of commands
    run     -- Start debugged program
    s       -- Step program
    c       -- Continue program being debugged
    x       -- Examin memory: x(/COUNT) ADDRESS
    info reg        -- List of registers
    disas   -- Disassemble: disassemble (ADDRESS)
    info b  -- Status of breakpoints
    disable -- Disable breakpoint: disable NUMBER
    enable  -- Enable breakpoint: enable NUMBER
    delete  -- Delete breakpoint: delete NUMBER
    b       -- Set breakpoint: b ADDRESS
    watch   -- Set a watchpoint. example:
                 data memory -- watch *0x80 != 10
                 pc          -- watch pc > 3
                 register    -- watch r7 == 3
    p       -- Print memory value: p *ADDRESS
    p       -- Set memory value: p *ADDRESS = VALUE
    
    (idb) q


More documents
--------------
  - [How to use API (docs/1_HowToUseAPI.md)] [6]
  - [How to create your processor (docs/2_HowToCreate.md)] [7]
  - [hackage processor-creative-kit] [1]



Note
----

Default processor architecture
  - Harvard architecture. (instruction and data memories are splitted)
  - fixed length instruction (word length)
  - word addressing (not byte addressing)
  - ideal immediate length (an immediate can be set by one instruction)
  - no MMU, cache, privilege level, interruption, I/O, and any


Limitation
  - using slow container(Data.Array) for simple implementation.


[1]: https://hackage.haskell.org/package/processor-creative-kit
[2]: https://hackage.haskell.org/package/HARM
[3]: https://hackage.haskell.org/package/powerpc
[4]: https://hackage.haskell.org/package/ministg
[5]: https://github.com/mattgreen/hython
[6]: https://github.com/takenobu-hs/processor-creative-kit/blob/master/docs/1_HowToUseAPI.md
[7]: https://github.com/takenobu-hs/processor-creative-kit/blob/master/docs/2_HowToCreate.md


