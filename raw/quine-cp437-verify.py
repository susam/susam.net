symbols = """\0☺☻♥♦♣♠•◘○◙♂♀♪♫☼►◄↕‼¶§▬↨↑↓→←∟↔▲▼ !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~⌂ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒáíóúñÑªº¿⌐¬½¼¡«»░▒▓│┤╡╢╖╕╣║╗╝╜╛┐└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀αßΓπΣσµτΦΘΩδ∞φε∩≡±≥≤⌠⌡÷≈°∙·√ⁿ²■ """


def hex_to_ints(hexa):
    return [int(h, base=16) for h in hexa.split()]


def null_to_space(ints):
    return [(32 if n == 0 else n) for n in ints]


def ints_to_glyphs(ints):
    return ''.join(symbols[n] for n in ints)


def ints_to_html(ints):
    return ''.join('&#x{:X};'.format(ord(symbols[n])) for n in ints)


def verify(desc, hexa, glyphs, html, doubled=False):
    print('Verifying', desc, '...\n')

    ints = hex_to_ints(hexa)
    norm = null_to_space(ints)

    g = ints_to_glyphs(norm)
    h = ints_to_html(norm)

    print('hexadecimal:    ', hexa)
    print('glyphs given:   ', glyphs)
    print('glyphs expected:', g)
    print('html given:     ', html)
    print('html expected:  ', h)

    assert glyphs == g
    assert html == h

    if doubled:
        mid = len(ints) // 2
        assert ints[:mid] == ints[mid:]

    print('\nPASS\n')


verify(
    'x86 quine',
    'fc b4 b8 8e c0 31 ff b1 12 b4 0a ac ab e2 fc f4 eb fd',
    'ⁿ┤╕Ä└1 ▒↕┤◙¼½Γⁿ⌠δ²',
    '&#x207F;&#x2524;&#x2555;&#xC4;&#x2514;&#x31;&#xA0;&#x2592;&#x2195;&#x2524;&#x25D9;&#xBC;&#xBD;&#x393;&#x207F;&#x2320;&#x3B4;&#xB2;',
)

verify(
    'x86 proper quine',
    'fc b3 02 b4 b8 8e c0 31 ff be 1b 01 b9 1b 00 b4 0a ac ab e2 fc 4b 75 f1 f4 eb fd fc b3 02 b4 b8 8e c0 31 ff be 1b 01 b9 1b 00 b4 0a ac ab e2 fc 4b 75 f1 f4 eb fd',
    'ⁿ│☻┤╕Ä└1 ╛←☺╣← ┤◙¼½ΓⁿKu±⌠δ²ⁿ│☻┤╕Ä└1 ╛←☺╣← ┤◙¼½ΓⁿKu±⌠δ²',
    '&#x207F;&#x2502;&#x263B;&#x2524;&#x2555;&#xC4;&#x2514;&#x31;&#xA0;&#x255B;&#x2190;&#x263A;&#x2563;&#x2190;&#x20;&#x2524;&#x25D9;&#xBC;&#xBD;&#x393;&#x207F;&#x4B;&#x75;&#xB1;&#x2320;&#x3B4;&#xB2;&#x207F;&#x2502;&#x263B;&#x2524;&#x2555;&#xC4;&#x2514;&#x31;&#xA0;&#x255B;&#x2190;&#x263A;&#x2563;&#x2190;&#x20;&#x2524;&#x25D9;&#xBC;&#xBD;&#x393;&#x207F;&#x4B;&#x75;&#xB1;&#x2320;&#x3B4;&#xB2;',
    True
)

verify(
    'boot quine',
    'ea 05 7c 00 00 fc b8 00 b8 8e c0 8c c8 8e d8 31 ff be 00 7c b9 20 00 b4 0a ac ab e2 fc f4 eb fd',
    'Ω♣|  ⁿ╕ ╕Ä└î╚Ä╪1 ╛ |╣  ┤◙¼½Γⁿ⌠δ²',
    '&#x3A9;&#x2663;&#x7C;&#x20;&#x20;&#x207F;&#x2555;&#x20;&#x2555;&#xC4;&#x2514;&#xEE;&#x255A;&#xC4;&#x256A;&#x31;&#xA0;&#x255B;&#x20;&#x7C;&#x2563;&#x20;&#x20;&#x2524;&#x25D9;&#xBC;&#xBD;&#x393;&#x207F;&#x2320;&#x3B4;&#xB2;'
)

verify(
    'boot proper quine',
    'ea 05 7c 00 00 fc bb 02 00 b8 00 b8 8e c0 8c c8 8e d8 31 ff be 26 7c b9 26 00 b4 0a ac ab e2 fc 4b 75 f1 f4 eb fd ea 05 7c 00 00 fc bb 02 00 b8 00 b8 8e c0 8c c8 8e d8 31 ff be 26 7c b9 26 00 b4 0a ac ab e2 fc 4b 75 f1 f4 eb fd',
    'Ω♣|  ⁿ╗☻ ╕ ╕Ä└î╚Ä╪1 ╛&|╣& ┤◙¼½ΓⁿKu±⌠δ²Ω♣|  ⁿ╗☻ ╕ ╕Ä└î╚Ä╪1 ╛&|╣& ┤◙¼½ΓⁿKu±⌠δ²',
    '&#x3A9;&#x2663;&#x7C;&#x20;&#x20;&#x207F;&#x2557;&#x263B;&#x20;&#x2555;&#x20;&#x2555;&#xC4;&#x2514;&#xEE;&#x255A;&#xC4;&#x256A;&#x31;&#xA0;&#x255B;&#x26;&#x7C;&#x2563;&#x26;&#x20;&#x2524;&#x25D9;&#xBC;&#xBD;&#x393;&#x207F;&#x4B;&#x75;&#xB1;&#x2320;&#x3B4;&#xB2;&#x3A9;&#x2663;&#x7C;&#x20;&#x20;&#x207F;&#x2557;&#x263B;&#x20;&#x2555;&#x20;&#x2555;&#xC4;&#x2514;&#xEE;&#x255A;&#xC4;&#x256A;&#x31;&#xA0;&#x255B;&#x26;&#x7C;&#x2563;&#x26;&#x20;&#x2524;&#x25D9;&#xBC;&#xBD;&#x393;&#x207F;&#x4B;&#x75;&#xB1;&#x2320;&#x3B4;&#xB2;',
    True
)
