.text
bgez $8, nottaken
bgez $9, taken1
addiu $2, $0, 10
syscall

nottaken:
addiu $10, $0, 3
addiu $2, $0, 10
syscall

taken1:
addiu $10, $0, 5
addiu $2, $0, 10
syscall