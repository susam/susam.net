from math import cos, pi
for n in range(1, 1001):
    s = [n, 'Fizz', 'Buzz', 'FizzBuzz', 'Jazz', 'FizzJazz', 'BuzzJazz', 'FizzBuzzJazz']
    i = (n % 3 == 0) + 2 * (n % 5 == 0) + 4 * (n % 7 == 0)
    print(s[i])
