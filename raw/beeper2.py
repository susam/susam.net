import datetime
import matplotlib.pyplot as plt

MAX = 9

lines = open("beeper2.txt").read().splitlines()
fmt = "%a %d %b %Y %I:%M:%S %p %Z"
start_time = datetime.datetime.strptime("Thu 10 Apr 2025 10:00:00 AM UTC", fmt)
end_time = datetime.datetime.strptime("Thu 11 Apr 2025 07:00:00 PM UTC", fmt)

buckets = [0] * 48
for line in lines:
    ts = ' '.join(line.split()[:-1])
    entry_time = datetime.datetime.strptime(ts, fmt)
    if start_time <= entry_time <= end_time:
        hour = int((entry_time - start_time).total_seconds() / 3600)
        buckets[hour] += 1

MAX = 14

buckets = buckets[:MAX] + [float('nan')]
print("buckets:", buckets)

x_ticks = list(range(len(buckets)))
x_labels = list(range(10, 25)) + list(range(1, 24))
x_labels = [str(x) for x in x_labels]
x_labels = x_labels[:MAX + 1]

plt.bar(
    x_ticks,
    buckets,
    width=1.0,
    linewidth=0.25,
    align="edge",
    edgecolor='black',
)
plt.xlabel("Hour of day (UTC)", fontsize=8)
plt.ylabel("Number of connections", fontsize=8)
plt.yticks(range(0, 80001, 10000))
plt.xticks(range(0, 10))
plt.xlim(0, MAX)
plt.tick_params(which='major', labelsize=6)

ax = plt.gca()
ax.set_xticks(x_ticks, x_labels)

for i, value in enumerate(buckets):
    plt.text(i + 0.5, value + 700, value, ha="center", fontsize=6)

plt.savefig("beeper2.png", dpi=300, bbox_inches="tight")
