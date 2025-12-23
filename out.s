.global _start
	.data

truestr: .ascii "true"
	truestr_len = . - truestr
falsestr: .ascii "false"
	falsestr_len = . - falsestr
	.align 2
display: .skip 12
	.text
_start:
	leal -4(%esp), %eax
	movl %eax, display+0
	subl $4, %esp # allocate locals
	call main_uniqstr2
	addl $4, %esp # deallocate locals
_end:               # do not care about clearing the stack
	movl $1, %eax   # _exit system call (check asm/unistd_32.h for the table)
	movl $0, %ebx   # error code 0
	int $0x80       # make system call
main_uniqstr2:
	pushl display+8
	movl $3, %eax
	pushl %eax

	subl $8, %esp
	leal 8(%esp), %eax
	movl %eax, display+8
	call f_uniqstr1
	movl display+8, %esp
	addl $4, %esp
	popl display+8
	pushl %eax
	movl $1, %eax
	movl %eax, %ebx
	popl %eax
	addl %ebx, %eax

	pushl %eax
	movl display+0, %eax
	popl %ebx
	movl %ebx, -0(%eax)
	movl display+0, %eax
	movl -0(%eax), %eax

	pushl %eax
	call print_int32
	addl $4, %esp
	pushl $10           # '\n'
	movl $4, %eax       # write system call
	movl $1, %ebx       # stdout
	leal 0(%esp), %ecx  # address of the character
	movl $1, %edx       # one byte
	int  $0x80          # make system call
	addl $4, %esp

	ret
f_uniqstr1:
	movl display+8, %eax
	movl -0(%eax), %eax
	pushl %eax
	movl $1, %eax
	pushl %eax
	movl $2, %eax
	movl %eax, %ebx
	popl %eax
	imull %ebx, %eax
	movl %eax, %ebx
	popl %eax
	addl %ebx, %eax

	pushl %eax
	movl display+8, %eax
	popl %ebx
	movl %ebx, -4(%eax)
	movl display+8, %eax
	movl -4(%eax), %eax
	ret

	ret



print_int32:
	movl 4(%esp), %eax  # the number to print
	cdq
	xorl %edx, %eax
	subl %edx, %eax     # abs(%eax)
	pushl $10           # base 10
	movl %esp, %ecx     # buffer for the string to print
	subl $16, %esp      # max 10 digits for a 32-bit number (keep %esp dword-aligned)
0:	xorl %edx, %edx     #     %edx = 0
	divl 16(%esp)       #     %eax = %edx:%eax/10 ; %edx = %edx:%eax % 10
	decl %ecx           #     allocate one more digit
	addb $48, %dl       #     %edx += '0'       # 0,0,0,0,0,0,0,0,0,0,'1','2','3','4','5','6'
	movb %dl, (%ecx)    #     store the digit   # ^                   ^                    ^
	test %eax, %eax     #                       # %esp                %ecx (after)         %ecx (before)
	jnz 0b              # until %eax==0         #                     <----- %edx = 6 ----->
	cmp %eax, 24(%esp)  # if the number is negative
	jge 0f
	decl %ecx           # allocate one more character
	movb $45, 0(%ecx)   # '-'
0:	movl $4, %eax       # write system call
	movl $1, %ebx       # stdout
	leal 16(%esp), %edx # the buffer to print
	subl %ecx, %edx     # number of digits
	int $0x80           # make system call
	addl $20, %esp      # deallocate the buffer
	ret
