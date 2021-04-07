.text
bne $8, $11, nottaken
bne $zero, $zero, nottaken
bne $9, $11, taken
addiu $3, $0, 1
addiu $2, $0, 10
syscall

nottaken:
addiu $10, $0, 2
addiu $2, $0, 10
syscall

taken:
addiu $13, $0, 3
addiu $2, $0, 10
syscall