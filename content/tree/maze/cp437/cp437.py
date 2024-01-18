print('CP437       Char  Unicode')
print('----------  ----  --------------')
s = open('cp437.txt').read()
for i in range(256):
    c = s[i]
    print('{:3} (0x{:02X})  {:4}  {:4} (0x{:04X})'.format(i, i, c, ord(c), ord(c)))
