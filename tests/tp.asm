segment	.data
align	4
global	printnr:object
printnr:
segment	.text._L1
align	4
_L1:
	push	ebp
	mov	ebp, esp
	sub	esp, 0
	lea	eax, [ebp+8]
	push	eax
	pop	eax
	push	dword [eax]
	call	printi
	add	esp, 4
	call	println
align	4
_L2:
	leave
	ret
segment	.data
	dd	_L1
segment	.text._main
align	4
global	_main:function
_main:
	push	ebp
	mov	ebp, esp
	sub	esp, 12
	push	dword 0
	lea	eax, [ebp+-4]
	push	eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
	push	dword 5
	lea	eax, [ebp+-8]
	push	eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
	push	dword 5
	push	dword 4
	pop	eax
	imul	dword eax, [esp]
	mov	[esp], eax
	pop	eax
	sub	esp, eax
	push	esp
	lea	eax, [ebp+-12]
	push	eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
	push	dword 1
	push	dword [esp]
	lea	eax, [ebp+-12]
	push	eax
	pop	eax
	push	dword [eax]
	push	dword 0
	push	dword 4
	pop	eax
	imul	dword eax, [esp]
	mov	[esp], eax
	pop	eax
	add	dword [esp], eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
	add	esp, 4
	push	dword 2
	push	dword [esp]
	lea	eax, [ebp+-12]
	push	eax
	pop	eax
	push	dword [eax]
	push	dword 1
	push	dword 4
	pop	eax
	imul	dword eax, [esp]
	mov	[esp], eax
	pop	eax
	add	dword [esp], eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
	add	esp, 4
	push	dword 3
	push	dword [esp]
	lea	eax, [ebp+-12]
	push	eax
	pop	eax
	push	dword [eax]
	push	dword 2
	push	dword 4
	pop	eax
	imul	dword eax, [esp]
	mov	[esp], eax
	pop	eax
	add	dword [esp], eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
	add	esp, 4
	push	dword 4
	push	dword [esp]
	lea	eax, [ebp+-12]
	push	eax
	pop	eax
	push	dword [eax]
	push	dword 3
	push	dword 4
	pop	eax
	imul	dword eax, [esp]
	mov	[esp], eax
	pop	eax
	add	dword [esp], eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
	add	esp, 4
	push	dword 5
	push	dword [esp]
	lea	eax, [ebp+-12]
	push	eax
	pop	eax
	push	dword [eax]
	push	dword 4
	push	dword 4
	pop	eax
	imul	dword eax, [esp]
	mov	[esp], eax
	pop	eax
	add	dword [esp], eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
	add	esp, 4
align	4
	push	dword 0
	lea	eax, [ebp+-16]
	push	eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
align	4
_L4:
	lea	eax, [ebp+-16]
	push	eax
	pop	eax
	push	dword [eax]
	push	dword 3
	pop	eax
	xor	ecx, ecx
	cmp	[esp], eax
	setle	cl
	mov	[esp], ecx
	pop	eax
	cmp	eax, byte 0
	je	near _L5
	lea	eax, [ebp+-12]
	push	eax
	pop	eax
	push	dword [eax]
	lea	eax, [ebp+-16]
	push	eax
	pop	eax
	push	dword [eax]
	push	dword 4
	pop	eax
	imul	dword eax, [esp]
	mov	[esp], eax
	pop	eax
	add	dword [esp], eax
	pop	eax
	push	dword [eax]
	push	dword $printnr
	pop	eax
	push	dword [eax]
	pop	eax
	call	eax
	add	esp, 4
	lea	eax, [ebp+-16]
	push	eax
	pop	eax
	push	dword [eax]
	push	dword 1
	pop	eax
	add	dword [esp], eax
	push	dword [esp]
	lea	eax, [ebp+-16]
	push	eax
	pop	ecx
	pop	eax
	mov	[ecx], eax
	add	esp, 4
	jmp	dword _L4
align	4
_L5:
align	4
	push	dword 0
	pop	eax
	jmp	dword _L3
	push	dword 0
	pop	eax
align	4
_L3:
	leave
	ret
extern	printi
extern	println
