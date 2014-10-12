

---
(gdb) b main
Breakpoint 1 at 0x40196e



---
(gdb) run
Starting program: /home/ttani/work.aes.ippemu/AES_Encrypt 
[New Thread 7956.0xd0]
[New Thread 7956.0x1b38]

Breakpoint 1, 0x0040196e in main ()


---
Breakpoint 1, KeyExpansion () at AES_Decrypt.c:130
130             for(i=0;i<Nk;i++)


---
(gdb) s
132                     RoundKey[i*4]=Key[i*4];


---
(gdb) 
133                     RoundKey[i*4+1]=Key[i*4+1];



---
(gdb) info reg
eax            0x1      1
ecx            0x22abf0 2272240
edx            0x0      0
ebx            0x22ac0c 2272268
esp            0x22abd4 0x22abd4
ebp            0x22abd8 0x22abd8
esi            0x2e     46
edi            0x72     114
eip            0x40196e 0x40196e <main+14>
eflags         0x202    [ IF ]
cs             0x1b     27
ss             0x23     35
ds             0x23     35
es             0x23     35
fs             0x3b     59
gs             0x0      0


---
(gdb) help
List of classes of commands:

aliases -- Aliases of other commands
breakpoints -- Making program stop at certain points
data -- Examining data
files -- Specifying and examining files
internals -- Maintenance commands
obscure -- Obscure features
running -- Running the program
stack -- Examining the stack
status -- Status inquiries
support -- Support facilities
tracepoints -- Tracing of program execution without stopping the program
user-defined -- User-defined commands

Type "help" followed by a class name for a list of commands in that class.
Type "help all" for the list of all commands.
Type "help" followed by command name for full documentation.
Type "apropos word" to search for commands related to "word".
Command name abbreviations are allowed if unambiguous.


---
(gdb) x 0x22abf0
0x22abf0:       0x00000001

---
(gdb) x/10 0x22abf0
0x22abf0:       0x00000001      0x0022ac0c      0x200280e8      0x61007f58
0x22ac00:       0x0022d404      0x00000002      0x6119ef5b      0x2003aed8
0x22ac10:       0x00000000      0x20000008


---
(gdb) info break
Num     Type           Disp Enb Address    What
1       breakpoint     keep y   0x0040196e <main+14>
        breakpoint already hit 1 time


---
(gdb) help disassemble

Dump of assembler code for function main:
   0x00401960 <+0>:     lea    0x4(%esp),%ecx
   0x00401964 <+4>:     and    $0xfffffff0,%esp
   0x00401967 <+7>:     pushl  -0x4(%ecx)
   0x0040196a <+10>:    push   %ebp
   0x0040196b <+11>:    mov    %esp,%ebp
   0x0040196d <+13>:    push   %ecx
=> 0x0040196e <+14>:    sub    $0x84,%esp
   0x00401974 <+20>:    call   0x401f30 <mcount>
   0x00401979 <+25>:    call   0x4010b0 <_monstartup>
   0x0040197e <+30>:    call   0x402408 <__main>
   0x00401983 <+35>:    jmp    0x4019a5 <main+69>
   0x00401985 <+37>:    movl   $0x4038e0,(%esp)
   0x0040198c <+44>:    call   0x402418 <printf>
   0x00401991 <+49>:    movl   $0x405030,0x4(%esp)



---
(gdb) b main
Breakpoint 1 at 0x402b6c: file AES_Decrypt.c, line 325.
(gdb) info b
Num     Type           Disp Enb Address    What
1       breakpoint     keep y   0x00402b6c in main at AES_Decrypt.c:325
(gdb) info watch
No watchpoints.
(gdb) watch *(int *)0x00402b6c
Hardware watchpoint 2: *(int *)0x00402b6c
(gdb) info watch
Num     Type           Disp Enb Address    What
2       hw watchpoint  keep y              *(int *)0x00402b6c


---
(gdb) info b
Num     Type           Disp Enb Address    What
1       breakpoint     keep n   0x00402b6c in main at AES_Decrypt.c:325
2       hw watchpoint  keep y              *(int *)0x00402b6c


---
(gdb) watch *(int*) 0x00402b6c > 4
Hardware watchpoint 3: *(int*) 0x00402b6c > 4
(gdb) info b
Num     Type           Disp Enb Address    What
1       breakpoint     keep n   0x00402b6c in main at AES_Decrypt.c:325
2       hw watchpoint  keep y              *(int *)0x00402b6c
3       hw watchpoint  keep y              *(int*) 0x00402b6c > 4


---
(gdb) watch *0x00402b6c == 0
Hardware watchpoint 4: *0x00402b6c == 0
(gdb) info b
Num     Type           Disp Enb Address    What
1       breakpoint     keep n   0x00402b6c in main at AES_Decrypt.c:325
2       hw watchpoint  keep y              *(int *)0x00402b6c
3       hw watchpoint  keep y              *(int*) 0x00402b6c > 4
4       hw watchpoint  keep y              *0x00402b6c == 0


---
(gdb) b 3
Breakpoint 5 at 0x4011ab: file AES_Decrypt.c, line 3.
(gdb) info b
Num     Type           Disp Enb Address    What
1       breakpoint     keep n   0x00402b6c in main at AES_Decrypt.c:325
2       hw watchpoint  keep y              *(int *)0x00402b6c
3       hw watchpoint  keep y              *(int*) 0x00402b6c > 4
4       hw watchpoint  keep y              *0x00402b6c == 0
5       breakpoint     keep y   0x004011ab in getSBoxInvert at AES_Decrypt.c:3

---
(gdb) enable 1
(gdb) info b
Num     Type           Disp Enb Address    What
1       breakpoint     keep y   0x00402b6c in main at AES_Decrypt.c:325
2       hw watchpoint  keep y              *(int *)0x00402b6c
3       hw watchpoint  keep y              *(int*) 0x00402b6c > 4
4       hw watchpoint  keep y              *0x00402b6c == 0
5       breakpoint     keep y   0x004011ab in getSBoxInvert at AES_Decrypt.c:3
(gdb) disable 2
(gdb) info b
Num     Type           Disp Enb Address    What
1       breakpoint     keep y   0x00402b6c in main at AES_Decrypt.c:325
2       hw watchpoint  keep n              *(int *)0x00402b6c
3       hw watchpoint  keep y              *(int*) 0x00402b6c > 4
4       hw watchpoint  keep y              *0x00402b6c == 0
5       breakpoint     keep y   0x004011ab in getSBoxInvert at AES_Decrypt.c:3

(gdb) delete 1
(gdb) info b
Num     Type           Disp Enb Address    What
2       breakpoint     keep y   0x00402b6c in main at AES_Decrypt.c:325



---
(gdb) p *0x22ac0c
$6 = 537112280

(gdb) p *0x22ac0c = 1
$7 = 1

---
(gdb) p *0x20018020
$1 = 1629491944

(gdb) p/x *0x20018020
$2 = 0x612012e8




