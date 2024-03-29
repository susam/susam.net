org 0x100
begin:
    mov dl, [si]
    mov ah, 0x2
    int 0x21
    inc si
    cmp si, end
    jne begin
    ret
end:
