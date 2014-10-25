

# 0
	MOV 	R0, 10  
      	MOV 	R1, 11  
 	MOV 	R2, 12 
        MOV 	R3, 13   
	MOV 	R4, 14

	MOV 	R5, 15 
  	MOV 	R6, 16 
	MOV 	R7, 17 
	NOP
	NOP


# 10
	MOV	R5, 0
	MOV	R6, 7
	MOV	R7, 1
	ST	M(R5), R5
	LD	R4, M(R5)
	add	r5, r5, r7
	cmp	r5, r6
	b	ne, -4
	nop
	nop

# 20
	mov	r1, 30
	call	r1
	mov	r6,606
	mov	r5,0xcafe
	halt
    	nop
	nop
	nop
	nop
	nop
	nop

# 30
	mov	r7, 707
	ret



