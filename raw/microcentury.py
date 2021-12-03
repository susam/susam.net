#!/usr/bin/env python3

import datetime

for year in range(1, 2400, 100):
    delta = datetime.date(year + 100, 1, 1) - datetime.date(year, 1, 1)
    print('{:04}-{:04}: {} d = {} s'
          .format(year, year + 99, delta.days, delta.total_seconds()))
