for n in range(1, 1001):
    s = ''
    if n % 3 == 0:
        s += 'Fizz'
    if n % 5 == 0:
        s += 'Buzz'
    if n % 7 == 0:
        s += 'Jazz'
    if s == '':
        print(n)
    else:
        print(s)
