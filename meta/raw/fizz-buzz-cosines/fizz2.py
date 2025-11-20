for n in range(1, 101):
    s = [n, 'Fizz', 'Buzz', 'FizzBuzz']
    i = (n % 3 == 0) + 2 * (n % 5 == 0)
    print(s[i])
