.text
j foo
syscall

foo:
aaddiu $2, $0, 12
syscall