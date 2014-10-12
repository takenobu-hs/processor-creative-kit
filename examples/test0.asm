

	mov 	r0, 10  
 #     	mov 	r1, 11  
  /* # */	mov 	r2, 12  /*
 */ mov 	r3, 13    /* */
	mov 	r4, 14 #

/*
	mov 	r5, 15 
  	mov 	r6, 16 
	mov 	r7, 17 */  


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
	nop  /* */ /* */ # a
	nop
    	nop
	nop
	halt




