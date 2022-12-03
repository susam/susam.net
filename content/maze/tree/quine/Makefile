# These quines read the executable bytes.
loop jne string:
	make _x86quine_test NAME=$@

b800:
	make _x86quine_run NAME=$@

# Proper quines that do not read executable bytes.
ploop pjne pstring:
	make _x86quine_test NAME=$@

pb800:
	make _x86quine_run NAME=$@

# Bootable quines.
boot pboot:
	make _bootquine_run NAME=$@

_x86quine_test:
	nasm -f bin -o $(NAME).com $(NAME).asm
	dosbox -c "MOUNT C ." -c "C:\$(NAME).com > C:\OUT.COM" -c "exit"
	ndisasm -o 0x100 $(NAME).com
	xxd -g1 $(NAME).com
	xxd -g1 OUT.COM
	ls -l $(NAME).com OUT.COM
	diff $(NAME).com OUT.COM

_x86quine_run:
	nasm -f bin -o $(NAME).com $(NAME).asm
	dosbox "$(NAME).com"
	ndisasm -o 0x100 $(NAME).com
	xxd -g1 $(NAME).com
	ls -l $(NAME).com

_bootquine_run:
	nasm -f bin -o $(NAME).img $(NAME).asm
	ls -l $(NAME).img
	echo 55 aa | xxd -r -p | dd seek=510 bs=1 of=$(NAME).img
	dosbox -c cls -c 'boot $(NAME).img'
	qemu-system-i386 -fda "$(NAME).img"
	ndisasm -o 0x7c00 $(NAME).img | head -n 20
	xxd -g1 $(NAME).img | head

clean:
	rm -rf *.com *.COM *.img

# loop (12 bytes)
# jne (14 bytes)
# string (14 bytes)
# b800 (19 bytes)
# boot (32 bytes)

# ploop (40 bytes)
# pjne (44 bytes)
# pstring (32 bytes)
# pb800 (64 bytes)
# pboot (76 bytes)
