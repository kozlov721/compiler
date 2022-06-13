.globl main
main:
    push %rbp
    mov %rsp, %rbp
    sub $8, %rsp
    mov $0, %rax
    movl %eax, -4(%rbp)
    mov $0, %rax
    movl %eax, -8(%rbp)
    mov $0, %rax
    movl %eax, -8(%rbp)
  1:
    movl -8(%rbp), %eax
    push %rax
    mov $10, %rax
    pop %rcx
    cmp %rax, %rcx
    mov $0, %rax
    setl %al
    jz 2f
    movl -4(%rbp), %eax
    push %rax
    movl -8(%rbp), %eax
    pop %rcx
    add %rcx, %rax
    movl %eax, -4(%rbp)
    movl -8(%rbp), %eax
    push %rax
    mov $1, %rax
    pop %rcx
    add %rcx, %rax
    movl %eax, -8(%rbp)
    jmp 1b
  2:
    movl -4(%rbp), %eax
    mov %rbp, %rsp
    pop %rbp
    ret
