"""Compare "high" characters of cp437.txt with how Python decodes bytes."""

s = open('cp437.txt').read()
all_match = True

for n in range(128, 256):
    c = bytes([n]).decode('cp437')
    match = s[n] == c
    all_match = all_match and match
    print(n, s[n], c, match, all_match)

print('PASS' if all_match else 'FAIL')
