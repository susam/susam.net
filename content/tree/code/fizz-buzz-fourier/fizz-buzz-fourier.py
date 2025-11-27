from math import e, pi

w = e**(1j * 2 * pi / 15)

# Calculate numerators of the Fourier coefficients for the Fizz Buzz
# index function described at <https://susam.net/fizz-buzz-with-cosines.html>.
for k in range(1, 16):
    a = (
        w ** (3 * k)
        + 2 * w ** (5 * k)
        + w ** (6 * k)
        + w ** (9 * k)
        + 2 * w ** (10 * k)
        + w ** (12 * k)
        + 3 * w ** (15 * k)
    )
    print(f'a_{{{k:02}}} = {a.real:4.1f} + {a.imag:4.1f}i')

# Output:
#
#   a_{01} = -0.0 + -0.0i
#   a_{02} = -0.0 + -0.0i
#   a_{03} =  6.0 + -0.0i
#   a_{04} = -0.0 + -0.0i
#   a_{05} =  5.0 + -0.0i
#   a_{06} =  6.0 + -0.0i
#   a_{07} = -0.0 +  0.0i
#   a_{08} = -0.0 +  0.0i
#   a_{09} =  6.0 + -0.0i
#   a_{10} =  5.0 + -0.0i
#   a_{11} = -0.0 + -0.0i
#   a_{12} =  6.0 + -0.0i
#   a_{13} = -0.0 +  0.0i
#   a_{14} = -0.0 + -0.0i
#   a_{15} = 11.0 + -0.0i
#
# This output contains only the numerators of the coefficients.
# The denominator 15 is excluded from these results.
# The actual Fourier coefficients are c_k = a_k/15.
# For example, c_3 = 6/15 = 2/5.
