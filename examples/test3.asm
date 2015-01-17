/*
 * example3: for loop structure
 *  sum = 0;
 *  for (i=1; i<=10; i++) {sum = sum + i;}
 */

	/* sum = 0 */
	mov	r0, 0		# sum = 0

	/* for (i=1; i<=10; i++) {sum = sum + i;} */
	mov	r1, 1		# i = 1;
	mov	r2, 10		# loop condition
	mov	r3, 1		# loop variable incrementer

	/* loop head */
	cmp	r1, r2		# flag = compare(i, 10)
	b	gt, 4		# if (flag == gt) goto pc+4  (to halt)

	add	r0, r0, r1	# sum = sum + i
	add	r1, r1, r3	# i++
	jmp	-4		# goto pc-4  (to loop head)

	halt

