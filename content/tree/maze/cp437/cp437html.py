template = """<!DOCTYPE html>
<html lang="en">
  <head>
    <title>CP437 to Unicode Map</title>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <style>
      body {background: #000; color: #999; font-family: monospace}
      table {border-collapse: collapse}
      th, td {border: thin solid #333; padding: 0.5em; text-align: center}
    </style>
  </head>
  <body>
    <h1>Code Page 437 to Unicode Map</h1>
    <table>
      <tr><th>Code</th><th>Hex</th><th>Symbol</th><th>Unicode</th></tr>
{{ rows }}    </table>
  </body>
</html>
"""

with open('cp437.txt') as f:
    s = f.read()

rows = ''
for i in range(256):
    c = s[i]
    rows += ('      <tr><td>{}</td><td>0x{:02X}</td><td>&#x{:X};</td><td>{:04X}</td></tr>\n'
             .format(i, i, ord(c), ord(c)))

with open('cp437.html', 'w') as f:
    html = template.replace('{{ rows }}', rows)
    f.write(html)
