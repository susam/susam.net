#!/usr/bin/env python3
import matplotlib.pyplot as plt
import numpy as np

# Less clumsy equation (simplified)
t = np.linspace(0.000001, 0.999999, 1000)
y = -t * np.log(t) + (1 - t) * np.log(1 - t)
plt.plot(t, y) # graph
plt.plot(0.5, 0, 'm.') # solution
plt.xlim(0, 1)
plt.xticks(np.arange(0, 1.1, 0.1))
plt.xlabel('t')
plt.ylabel('$ -t \ln t + (1 - t) \ln (1 - t) $')
plt.axhline(color='k', linewidth='0.8')
plt.savefig('clumsy-equations-1.png', dpi=200, bbox_inches='tight')
plt.clf()

# Less clumsy equation (original)
x = np.linspace(0.000001, 3, 1000)
y = x * np.exp(-x) + (1 - np.exp(-x)) * np.log(1 - np.exp(-x))
plt.plot(x, y) # graph
plt.plot(np.log(2), 0, 'm.') # solution
plt.xlim(0, 3)
plt.xticks(np.arange(0, 3.1, 0.2))
plt.xlabel('x')
plt.ylabel('$ x e^{-x} + (1 - e^{-x}) \ln (1 - e^{-x}) $')
plt.axhline(color='k', linewidth='0.8')
plt.savefig('clumsy-equations-2.png', dpi=200, bbox_inches='tight')
plt.clf()

# More clumsy equation
x = np.linspace(0.000001, 10, 10000)
y = np.log(x) - x**0.03
plt.plot(x, y) # graph
plt.plot(2.80506, 0, 'm.') # solution
plt.xlim(0, 10)
plt.ylim(-2, 2)
plt.xlabel('x')
plt.ylabel('$ \ln x - x^{0.03} $')
plt.axhline(color='k', linewidth='0.8')
plt.savefig('clumsy-equations-3.png', dpi=200, bbox_inches='tight')
plt.clf()
