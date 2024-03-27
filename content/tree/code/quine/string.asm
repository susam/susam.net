org 0x100
begin:
    mov ax, 0x0923
    inc al
    mov [end], al
    mov dx, begin
    int 0x21
    ret
end:
