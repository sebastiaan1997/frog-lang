.cpu cortex-m0

.global main


.text
.align 2

main:
	PUSH {LR}
	CMP R0, #1
	BNE endif_0
loop_1:
	PUSH {R0,R1,R2}
	BL print_hello
	POP {R0,R1,R2}
	ADD R1, R1, #1
	CMP R1, #254
	BLT loop_1
endif_0:
	POP {PC}
