.text

addiu $3, $zero, 3
addiu $4, $zero, 4
mult $3, $4
addiu $5, $zero, 5
mflo $6
div $6, $5 #does 12/5
mfhi $7 #stores 2
mflo $8 #stores 2
addiu $6, $zero, 13
div $6, $5 #does 13/5 this time
mfhi $9 #stores 3, the remainder
mflo $10 #stores 2
addiu $2, $zero, 0x0a
syscall