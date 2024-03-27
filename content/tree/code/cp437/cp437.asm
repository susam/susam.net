org 0x100
begin:
    mov ax, 0xb800
    mov ds, ax
    xor di, di
    mov ax, 0x0a00
print:
    mov [di], ax
    inc di
    inc di
    inc al
    jnz print
    ret
