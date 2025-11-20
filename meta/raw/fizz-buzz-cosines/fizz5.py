from math import cos, pi
for n in range(1, 101):
    print([n, 'Fizz', 'Buzz', 'FizzBuzz'][round(11 / 15 + (2 / 3) * cos(2 * pi * n / 3) + (4 / 5) * (cos(2 * pi * n / 5) + cos(4 * pi * n / 5)))])
