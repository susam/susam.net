org 0x100
begin:
    cld
    mov cl, end - begin
print:
    lodsb
    xchg dx, ax
    mov ah, 2
    int 0x21
    loop print
    ret
end:
