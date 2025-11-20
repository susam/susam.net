for n in range(1, 101):
    print([n, 'Fizz', 'Buzz', 'FizzBuzz'][(n % 3 == 0) + 2 * (n % 5 == 0)])
