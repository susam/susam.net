import datetime
import matplotlib.pyplot as plt

lines = open("beeper1.txt").read().splitlines()
fmt = "%a %b %d %H:%M:%S %Z %Y"
start_time = datetime.datetime.strptime("Mon Jan 31 10:00:00 UTC 2022", fmt)
buckets = [0] * 25 + [float("nan")]

for line in lines:
    entry_time = datetime.datetime.strptime(line, fmt)
    hour = int((entry_time - start_time).total_seconds() / 3600)
    buckets[hour] += 1


x_ticks = list(range(len(buckets)))
x_labels = list(range(10, 25)) + list(range(1, 12))
x_labels = [str(x) for x in x_labels]

print("buckets:", buckets)
print("total:", sum(buckets[:-1]))
print("x_ticks:", x_ticks)
print("x_labels:", x_labels)

plt.bar(
    x_ticks,
    buckets,
    width=1.0,
    linewidth=0.25,
    align="edge",
    edgecolor="black",
)
plt.xlabel("Hour of day (UTC)", fontsize=8)
plt.ylabel("Number of connections", fontsize=8)
plt.yticks(range(0, 1201, 100))
plt.xlim(0, 25)
plt.tick_params(which="major", labelsize=6)

ax = plt.gca()
ax.set_xticks(x_ticks, x_labels)

for i, value in enumerate(buckets):
    plt.text(i + 0.5, value + 8, value, ha="center", fontsize=6)

plt.savefig("beeper1.png", dpi=300, bbox_inches="tight")
