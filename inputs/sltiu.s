.text

addiu $3, $zero, 2
addiu $4, $zero, 3

slt $5, $3, $4 # set 1
slt $6, $4, $3 # set 0

sltiu $7, $3, 4 # set 1
sltiu $8, $4, 3 # set 0

slti $9, $4, 4 # set 1
slti $10, $4, 2 # set 0

addiu $2, $zero, 0x0a
syscall