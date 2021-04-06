.text

addiu $3, $zero, 2
addiu $4, $zero, 3
xor $5, $3, $4 #stores 1 to 5 since bitwise xor of 2 and 3
nor $6, $3, $zero #0xffd
xor $7, $5, $6 #0xffc
nor $8, $5, $7 #0x2 respectively

addiu $2, $zero, 0x0a
syscall