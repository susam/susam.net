org 0x100
begin:
    cld
    mov ah, 0xb8
    mov es, ax
    xor di, di
    mov cl, end - begin
    mov ah, 0xa
print:
    lodsb
    stosw
    loop print
idle:
    hlt
    jmp idle
end:
