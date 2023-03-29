	.file	"main.c"
	.text
	.globl	main
	.type	main, @function
main:
.LFB6:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movb	$49, -1(%rbp)
	movzbl	-1(%rbp), %eax
	addl	$50, %eax
	movb	%al, -1(%rbp)
	movzbl	-1(%rbp), %eax
	addl	$51, %eax
	movb	%al, -1(%rbp)
	movsbl	-1(%rbp), %eax
	movl	%eax, %edi
	call	putchar@PLT
	movl	$1, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	main, .-main
	.ident	"GCC: (GNU) 11.1.0"
	.section	.note.GNU-stack,"",@progbits
