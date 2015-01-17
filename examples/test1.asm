/*
 * example1: memory load and store
 *  ptr0 = 10;
 *  ptr1 = 11;
 *  *ptr1 = *ptr0 + 100;
 */

	mov	r0, 10		# ptr0 = 10
	mov	r1, 11		# ptr1 = 11

	ld	r3, m(r0)	# r3 = *ptr0
	mov	r4, 100		# r4 = 100
	add	r5, r3, r4	# r5 = *ptr0 + 100
	st	m(r1), r5	# *ptr1 = r5

	halt

