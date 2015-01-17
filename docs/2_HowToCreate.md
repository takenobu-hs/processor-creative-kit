How to create your processor
============================

Contents
--------
  - [add your instruction] (#add your instruction)
  - add/modify registers (number, name)
  - modify inst/data memory config
  - modify memory operand format
  - modify comment format
  - modify left/right operand order


create your processor
---------------------

### add your instruction

example for adding "NEG" instruction.

insert following lines:

[Language/Pck/Cpu/Instruction.hs]
```haskell
          | NEG   GReg GReg
```

[Language/Pck/Cpu/Execution.hs]
```haskell
evalStep (NEG   ra rb)    = uniopInst (neg) ra rb
```

[Language/Pck/Tool/Assembler.hs]
```haskell
         <|> inst2 NEG  "neg" greg greg
```



