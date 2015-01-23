/*
 * example6: instruction examples
 */

	/* no-operation instruction */
	nop					# no-operation

	/* move instructions */
	mov	r1, 0x403			# r1 = 0x403
	mov	r2, 0x101
	mov	r3, 0
	mov	r4, 2
	mov	r5, 27
	mov	r0, r1				# r0 = r1
	mov	r0, pc				# r0 = pc

	/* arithmetic operation instructions */
	add	r0, r1, r2			# r0 = r1 + r2
	sub	r0, r1, r2			# r0 = r1 - r2
	cmp	r0, r1				# flag = compare(r0, r1)
	abs	r0, r1				# r0 = abs(r1)
	ash	r0, r2, r4			# r0 = r2 << r4   // arithmetic
	mul	r0, r1, r4			# r0 = r1 * r4
	div	r0, r1, r4			# r0 = r1 / r4

	/* logical operation instructions */
	and	r0, r1, r2			# r0 = r1 & r2
	or	r0, r1, r2			# r0 = r1 | r2
	not	r0, r1				# r0 = ~r1
	xor	r0, r1, r2			# r0 = r1 ^ r2
	lsh	r0, r2, r4			# r0 = r2 << r4   // logical

	/* memory access instructions */
	st	m(r3), r1			# *r3 = r1
	ld	r0, m(r3)			# r0 = *r3

	/* control flow instructions */
	b	eq, 1				# if (flag == eq) goto pc+1
	jmp	1				# goto pc+1
	jmp	r5				# goto r5
	call	r1				# goto r1; r0 = pc
	ret					# goto r0

	/* machine halt instruction */
	halt					# halt cpu

