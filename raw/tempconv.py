def exact_c2f(c):
    return 9 * c / 5 + 32


def exact_f2c(f):
    return 5 * (f - 32) / 9


def crude_c2f(c):
    return 2 * c + 30


def crude_f2c(f):
    return (f - 30) / 2


def finer_c2f(c):
    return (c - c // 10) * 2 + 31


def finer_f2c(f):
    c = (f - 31) // 2
    c += c // 10
    return c


def test_crude_c2f(a, b, step=1):
    c = a
    while c <= b:
        exact_f = exact_c2f(c)
        crude_f = crude_c2f(c)
        error_f = crude_f - exact_f
        print('{:.2f} C => {:.2f} F ~ {:.2f} F; err: {:4.2f}'.format(
              c, exact_f, crude_f, error_f))
        c += step


def test_crude_f2c(a, b, step=1):
    f = a
    while f <= b:
        exact_c = exact_f2c(f)
        crude_c = crude_f2c(f)
        error_c = crude_c - exact_c
        print('{:.2f} F => {:.2f} C ~ {:.2f} C; err: {:4.2f}'.format(
              f, exact_c, crude_c, error_c))
        f += step

def test_finer_c2f(a, b, step=1):
    c = a
    while c <= b:
        exact_f = exact_c2f(c)
        finer_f = finer_c2f(c)
        error_f = finer_f - exact_f
        print('{:.2f} C => {:.2f} F ~ {:.2f} F; err: {:4.2f}'.format(
              c, exact_f, finer_f, error_f))
        c += step


def test_finer_f2c(a, b, step=1):
    f = a
    while f <= b:
        exact_c = exact_f2c(f)
        finer_c = finer_f2c(f)
        error_c = finer_c - exact_c
        print('{:.2f} F => {:.2f} C ~ {:.2f} C; err: {:4.2f}'.format(
              f, exact_c, finer_c, error_c))
        f += step


test_crude_c2f(-100, 100)
test_crude_f2c(-148, 212)
test_finer_c2f(-100, 100)
test_finer_f2c(-148, 212)
