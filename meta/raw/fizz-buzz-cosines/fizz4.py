from math import cos, pi
for n in range(1, 101):
    s = [n, 'Fizz', 'Buzz', 'FizzBuzz']
    i = round(11 / 15 + (2 / 3) * cos(2 * pi * n / 3)
                      + (4 / 5) * cos(2 * pi * n / 5)
                      + (4 / 5) * cos(4 * pi * n / 5))
    print(s[i])
