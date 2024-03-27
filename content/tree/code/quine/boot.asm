org 0x7c00
begin:
    jmp 0:next
next:
    cld
    mov ax, 0xb800
    mov es, ax
    mov ax, cs
    mov ds, ax
    xor di, di
    mov si, begin
    mov cx, end - begin
    mov ah, 0xa
print:
    lodsb
    stosw
    loop print
idle:
    hlt
    jmp idle
end:
