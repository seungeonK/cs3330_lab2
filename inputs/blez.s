.text
blez $8, nottaken
blez $9, taken1
addiu $2, $0, 10
syscall

nottaken:
addiu $2, $0, 10
syscall

taken1:
addiu $10, $0, 10
addiu $2, $0, 10
syscall