#!/usr/bin/env python3
import matplotlib.pyplot as plt
import numpy as np

# Less clumsy equation (simplified)
p = np.linspace(0.000001, 0.999999, 1000)
y = 1 + p * np.log2(p) + (1 - p) * np.log2(1 - p)
plt.plot(p, y)

# Axes
plt.xlim(0, 1)
plt.ylim(0, 1)
plt.xticks(np.arange(0, 1.1, 0.1))
plt.xlabel('p')
plt.ylabel('$ 1 + p \log_2 p + (1 - p) \log_2 (1 - p) $')

# Output
plt.savefig('channel-capacity.png', dpi=200, bbox_inches='tight')
