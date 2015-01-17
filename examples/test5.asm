/*
 * example5: call and return
 *  r4 = max(100, 200)
 */

	mov	r1, 100
	mov	r2, 200

	mov	r3, 10		# set max function's address
	call	r3		# call max(r1,r2); (and save next address to r0)
	mov	r4, r1		

	halt


	/* padding */
	nop
	nop
	nop
	nop

	/* max(r1, r2) function:
	 *    if (r1>r2) {return r1;} else {r1=r2; return r1;}
	 */
	cmp	r1, r2
	b	gt, 2		# if (r1>r2) goto pc+2  (to ret)
	mov	r1, r2		# r1 = r2	
	ret			# return to r0 (next of call)

