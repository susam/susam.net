org 0x7c00
begin:
    jmp 0:next
next:
    cld
    mov bx, 2
    mov ax, 0xb800
    mov es, ax
    mov ax, cs
    mov ds, ax
    xor di, di
outer:
    mov si, msg
    mov cx, end - msg
    mov ah, 0xa
print:
    lodsb
    stosw
    loop print
    dec bx
    jnz outer
idle:
    hlt
    jmp idle
msg:
    db 0xea, 0x05, 0x7c, 0x00, 0x00, 0xfc, 0xbb, 0x02
    db 0x00, 0xb8, 0x00, 0xb8, 0x8e, 0xc0, 0x8c, 0xc8
    db 0x8e, 0xd8, 0x31, 0xff, 0xbe, 0x26, 0x7c, 0xb9
    db 0x26, 0x00, 0xb4, 0x0a, 0xac, 0xab, 0xe2, 0xfc
    db 0x4b, 0x75, 0xf1, 0xf4, 0xeb, 0xfd
end: