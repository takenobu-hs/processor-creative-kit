/*
 * example2: if structure
 *  if (r0 == 0) {r1 = 10;}
 *  else {r1 = 20;}
 */

	/* if (r0 == 0) */
	mov	r2, 0		# r2 = 0
	cmp	r0, r2		# flag = compare(r0, r2)
	b	eq, 3		# if (flag == eq) goto pc+3  (to then clause)

	/* else clause */
	mov	r1, 20		# r1 = 20
	jmp	2		# goto pc+2  (to halt)

	/* then clause */
	mov	r1, 10		# r1 = 10

	halt

