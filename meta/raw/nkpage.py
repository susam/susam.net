import random
import re
import string


PAGE = 'content/tree/code/test/2k.page.html'


def randstr(min_len, max_len):
    return ''.join(
        random.choices(
            string.ascii_letters + ' ',
            k=random.randint(min_len, max_len)
        )
    ).strip()


def rand_items():
    items = []
    for _ in range(2000):
        items.append((randstr(11, 11), randstr(20, 50)))
    return items


def read_items(fname):
    items = []
    with open(fname) as f:
        for line in f:
            if 'href' in line:
                m = re.search(r'<a href=".*">(.*)</a> \((.*)\)', line)
                if m is not None:
                    print(m.group(1), m.group(2))
                    items.append((m.group(2), m.group(1)))
    return items


def item_html(date, title):
    href = '#' + title.lower().replace(' ', '-')
    return f'  <li>\n    <a href="{href}">{title}</a> ({date})\n  </li>\n'


def make_page(items, fname):
    s = """<!-- date: 2025-10-04 -->
<!-- title: 2K Test Page -->
<!-- tag: Meta -->
<!-- key: tenkt -->
<!-- unlist: yes -->
<h1>{{ title }}</h1>
<p>
  This is a simple demo page to help with answering the HN comment
  <a href="https://news.ycombinator.com/item?id=46487498#46488380">#46488380</a>.
</p>
<ul class="blog">
"""
    for date, title in items:
            s += item_html(date, title)
    s += '</ul>\n'
    with open(fname, 'w') as f:
        f.write(s)

if __name__ == '__main__':
    fname = 'content/tree/code/test/2k.page.html'
    #make_page(rand_items(), fname)
    make_page(read_items(fname), fname)
