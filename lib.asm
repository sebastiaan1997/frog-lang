.cpu cortex-m0

.global odd
.global even


.text
.align 2

odd:
	PUSH {LR}
	CMP R0, #0
	BNE endif_0
	MOV R0, #0
	POP {PC}
endif_0:
	SUB R0, R0, #1
	BL even
	POP {PC}
even:
	PUSH {LR}
	CMP R0, #0
	BNE endif_2
	MOV R0, #1
	POP {PC}
endif_2:
	SUB R0, R0, #1
	BL odd
	POP {PC}
