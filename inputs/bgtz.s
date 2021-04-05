.text
bgtz $zero, nottaken
bgtz $8, nottaken
bgtz $9, taken1
addiu $3, $0, 1
addiu $2, $0, 10
syscall

nottaken:
addiu $3, $0, 2
addiu $2, $0, 10
syscall

taken1:
addiu $3, $0, 3
addiu $2, $0, 10
syscall
