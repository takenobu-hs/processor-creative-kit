

# 0
	mov 	r0, 10  
      	mov 	r1, 11  
 	mov 	r2, 12 
        mov 	r3, 13   
	mov 	r4, 14

	mov 	r5, 15 
  	mov 	r6, 16 
	mov 	r7, 17 
	nop
	nop


# 10
	mov	r5, 0
	mov	r6, 7
	mov	r7, 1
	st	m(r5), r5
	ld	r4, m(r5)
	add	r5, r5, r7
	cmp	r5, r6
	b	ne, -4
	nop
	nop

# 20
	mov	r1, 30
	call	r1
	mov	r6,606
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



