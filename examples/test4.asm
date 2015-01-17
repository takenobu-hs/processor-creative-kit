/*
 * example4: memory setting
 *  ptr = 0;
 *  for (i=0; i<10; i++) {*ptr++ = i;}
 */

	mov	r0, 0		# pr = 0

	/* for (i=0; i<10; i++) {*ptr++ = i;} */
	mov	r1, 0		# i = 0;
	mov	r2, 10		# loop condition
	mov	r3, 1		# loop variable incrementer

	/* loop head */
	cmp	r1, r2		# flag = compare(i, 10)
	b	ge, 5		# if (flag == ge) goto pc+5  (to halt)

	st	m(r0), r1	# *ptr = i
	add	r0, r0, r3	# ptr++
	add	r1, r1, r3	# i++
	jmp	-5		# goto pc-4  (to loop head)

	halt

