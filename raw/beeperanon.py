lines = open("beeper2-ips.txt").read().splitlines()

anon = {}
counter = 0

for line in lines:
    words = line.split()
    ts = " ".join(words[:-1])
    ip = words[-1]
    if ip not in anon:
        anon[ip] = f"C{counter:04}"
        counter += 1
    print(ts, anon[ip])
