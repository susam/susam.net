# Hex-coding boot sector programs
# Alternative to writing-boot-sector-code.html

# Print 'A'
printf '\xb8\x00\xb8\x8e\xd8\xb8\x41\x1e\x31\xff\x89\x05\xf4\xeb\xfd' > a.com
printf '\xb8\x00\xb8\x8e\xd8\xb8\x41\x1e\x31\xff\x89\x05\xf4\xeb\xfd' > a.img
printf '\x55\xaa' | dd seek=510 bs=1 of=a.img

# Print "hello, world"
printf '\xb8\x00\xb8\x8e\xd8\x31\xff\xbe\x1c\x01\xb4\x1e\x2e\x8a\x04\x89\x05\x46\x47\x47\x83\xff\x18\x75\xf3\xf4\xeb\xfd\x68\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64' > hello.com
printf '\xb8\x00\xb8\x8e\xd8\x31\xff\xbe\x1c\x7c\xb4\x1e\x2e\x8a\x04\x89\x05\x46\x47\x47\x83\xff\x18\x75\xf3\xf4\xeb\xfd\x68\x65\x6c\x6c\x6f\x2c\x20\x77\x6f\x72\x6c\x64' > hello.img
printf '\x55\xaa' | dd seek=510 bs=1 of=hello.img