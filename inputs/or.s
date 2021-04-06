.text

addiu $3, $zero, 10
addiu $4, $zero, 7
and $5, $3, $4 #2
or $6, $3, $zero #10
and $7, $5, $6 #10
or $8, $5, $7 #10

addiu $2, $zero, 0x0a
syscall