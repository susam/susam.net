\version "2.12.3"
\pointAndClickOff

\header {
    title = "A few notes 1.0"
    composer = "Susam Pal"
    tagline = "Copyright (C) 2009 Susam Pal"
}

\score {
    \new PianoStaff
    <<
    \new Staff \relative c' { 
        \clef "treble"
        \time 4/4
        \tempo 4 = 132
        e4 d8 c e4 f~ f1
        f4 e8 d f4 g~ g1
        g4 a8 b c4 c~ c1
        c4 b8 a g4 f~ f1

        e4 d8 c e4 f~ f1
        f4 e8 d f4 g~ g1
        g4 a8 b c4 c~ c1
        c4 b8 a g4 f~ f1

        e4 d8 c e4 f
        f4 e8 d f4 g
        g4 a8 b c4 c
        c4 b8 a g4 f~ f1

        e4 d8 c e4 f~ f1
        f4 e8 d f4 g~ g1
        f4 e8 d e4 d4~ d1
        c4 e8 f e4 d4 c1
    }
    \new Staff \relative c, {
        \clef "bass"
        c4 c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c

        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c

        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c

        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c
        c, c' c c

        <c, c'>1
    }
    >>
    \midi { }
    \layout { }
}

