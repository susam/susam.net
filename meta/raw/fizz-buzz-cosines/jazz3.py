from math import cos, pi
for n in range(1, 1001):
    s = [n, 'Fizz', 'Buzz', 'FizzBuzz', 'Jazz', 'FizzJazz', 'BuzzJazz', 'FizzBuzzJazz']
    i = round((137 / 105) + (2 / 3) * cos(2 * pi * n / 3)
                          + (4 / 5) * cos(2 * pi * n / 5)
                          + (4 / 5) * cos(4 * pi * n / 5)
                          + (8 / 7) * cos(2 * pi * n / 7)
                          + (8 / 7) * cos(4 * pi * n / 7)
                          + (8 / 7) * cos(6 * pi * n / 7))
    print(s[i])
