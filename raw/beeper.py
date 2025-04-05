import datetime
import matplotlib.pyplot as plt

lines = open("beeper.txt").read().splitlines()
fmt = "%a %b %d %H:%M:%S %Z %Y"
start_time = datetime.datetime.strptime("Mon Jan 31 10:14:00 UTC 2022", fmt)
buckets = [0] * 24 + [float('nan')]

for line in lines:
    entry_time = datetime.datetime.strptime(line, fmt)
    hour = int((entry_time - start_time).total_seconds() / 3600)
    buckets[hour] += 1

print(sum(buckets))
hours = [f"{n}" for n in range(25)]

plt.bar(
    hours,
    buckets,
    width=1.0,
    linewidth=0.25,
    align="edge",
    edgecolor='black',
)
plt.xlabel("Hour", fontsize=8)
plt.ylabel("Number of connections", fontsize=8)
plt.xticks(range(0, 25))
plt.yticks(range(0, 1201, 100))
plt.xlim(0, 24)
plt.tick_params(which='major', labelsize=6)

for i, value in enumerate(buckets):
    plt.text(i + 0.5, value + 8, value, ha="center", fontsize=6)

plt.savefig("beeper.png", dpi=300, bbox_inches="tight")
