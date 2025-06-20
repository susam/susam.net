# Hex-coding boot sector programs
# Alternative to writing-boot-sector-code.html

# Print 'A' in DOS.
echo b8 00 b8 8e d8 b8 41 1e 31 ff 89 05 f4 eb fd | xxd -r -p > a.com

# Print 'A' during boot.
cp a.com a.img
echo 55 aa | xxd -r -p | dd seek=510 bs=1 of=a.img

# Print "hello, world" in DOS.
echo b8 00 b8 8e d8 31 ff be 21 01 b4 1e 2e 8a 04 89 05 46 47 47 83 ff 18 75 f3 f4 eb fd 00 00 00 00 00 68 65 6c 6c 6f 2c 20 77 6f 72 6c 64 | xxd -r -p > hello.com

# Print "hello, world" during boot.
echo ea 05 7c 00 00 b8 00 b8 8e d8 31 ff be 21 7c b4 1e 2e 8a 04 89 05 46 47 47 83 ff 18 75 f3 f4 eb fd 68 65 6c 6c 6f 2c 20 77 6f 72 6c 64 | xxd -r -p > hello.img
echo 55 aa | xxd -r -p | dd seek=510 bs=1 of=hello.img
