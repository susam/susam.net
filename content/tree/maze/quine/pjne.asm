org 0x100
    mov cl, 2
outer:
    mov si, msg
print:
    mov dl, [si]
    mov ah, 0x2
    int 0x21
    inc si
    cmp si, end
    jne print
    dec cx
    jnz outer
    ret
msg:
    db 0xb1, 0x02, 0xbe, 0x16, 0x01, 0x8a, 0x14, 0xb4
    db 0x02, 0xcd, 0x21, 0x46, 0x81, 0xfe, 0x2c, 0x01
    db 0x75, 0xf3, 0x49, 0x75, 0xed, 0xc3
end: