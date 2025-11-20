import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np

mpl.rcParams.update({
    "text.usetex": True,
})

def plot(x, y, ylim, ylabel, filename):
    plt.plot(x, y)
    plt.xlabel('$ x $')
    plt.ylabel(ylabel)
    plt.xlim([0, 20.5])
    plt.ylim(ylim)
    plt.grid(which='major', linewidth='0.4')
    plt.grid(which='minor', linewidth='0.1')
    ax = plt.gca()
    ax.xaxis.set_major_locator(mpl.ticker.MultipleLocator(1.0))
    ax.yaxis.set_major_locator(mpl.ticker.MultipleLocator(1.0))
    ax.yaxis.set_minor_locator(mpl.ticker.MultipleLocator(0.1))
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    plt.clf()

def main():
    x = np.linspace(0, 21, 1000)
    plot(
        x,
        (1 / 3) + (2 / 3) * np.cos(2 * np.pi * x / 3),
        [-1.2, 1.2],
        r'$ \frac{1}{3} + \frac{2}{3} \cos \left( \frac{2 \pi x}{3} \right) $',
        'fizz-buzz-i3.png',
    )
    plot(
        x,
        (1 / 5) + (2 / 5) * np.cos(2 * np.pi * x / 5) + (2 / 5) * np.cos(4 * np.pi * x / 5),
        [-1.2, 1.2],
        r'$ \frac{1}{5} + \frac{2}{5} \cos \left( \frac{2 \pi x}{5} \right) + \frac{2}{5} \cos \left( \frac{4 \pi x}{5} \right) $',
        'fizz-buzz-i5.png',
    )
    plot(
        x,
        11 / 15 + (2 / 3) * np.cos(2 * np.pi * x / 3) + (4 / 5) * (np.cos(2 * np.pi * x / 5) + np.cos(4 * np.pi * x / 5)),
        (-1.2, 3.2),
        r'$ \frac{11}{15} + \frac{2}{3} \cos \left( \frac{2 \pi x}{3} \right) + \frac{4}{5} \cos \left( \frac{2 \pi x}{5} \right) + \frac{4}{5} \cos \left( \frac{4 \pi x}{5} \right) $',
        'fizz-buzz-f.png',
    )

main()
