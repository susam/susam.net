\version "2.12.3"
\pointAndClickOff

\header {
    title = "A few notes 2.0"
    composer = "Susam Pal"
    tagline = "Copyright (C) 2009 Susam Pal"
}

pianoTreble = \relative c' { 
    % 8 measures
    e4\f d8 c e4 f~ f1
    f4 e8 d f4 g~ g1
    g4 a8 b c4 c~ c1
    c4 b8 a g4 f~ f1

    % 8 measures
    e4 d8 c e4 f~ f1
    f4 e8 d f4 g~ g1
    g4 a8 b c4 c~ c1
    c4 b8 a g4 f~ f1

    % 5 measures
    e4 d8 c e4 f
    f4 e8 d f4 g
    g4 a8 b c4 c
    c4 b8 a g4 f~ f1

    % 8 measures
    e4 d8 c e4 f~ f1
    f4 e8 d f4 g~ g1
    f4 e8 d e4 d4~ d1
    c4 e8 f e4 d4 c1
}

pianoBass = \relative c, {
    
    % 8 measures
    c4 c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c

    % 8 measures
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c

    % 5 measures
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c

    % 8 measures
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    c, c' c c
    <c, c'>1
}

strings = \relative c {
    % 8 measures
    r1 r r r r r r r

    % 8 measures
    c2\p f c f
    d g d g
    f c' f, c'
    c f, c' f,

    % 5 measures
    c f d g
    f c' c f, c' f,

    % 4 measures
    c f c f
    d g d g

    % 2 measures
    f d f d

    % 2 measures
    c e4 f
    c1
}

guitar = \relative c'' {
    % 16 measures
    r1 r r r
    r r r r
    r r r r
    r r r r

    % 9 measures
    a4\ppp c f c
    b d g d
    f a c a
    e g c g
    e g c g
    a, c f c
    a c f c
    b d g d
    b d g d

    %4 measures 
    r1 r r r
}

\score {
    <<
    \new PianoStaff <<
        \set PianoStaff.instrumentName = #"Piano"
        \new Staff {
            \time 4/4
            \tempo 4 = 132
            \set Staff.midiInstrument = "electric piano 2"
            \pianoTreble

        }

        \new Staff {
            \clef "bass"
            \set Staff.midiInstrument = "electric piano 2"
            \pianoBass
        }

    >>

    \new Staff {
        \clef "bass"
        \set Staff.instrumentName = #"Strings"
        \set Staff.midiInstrument = "string ensemble 1"
        \strings
    }

    \new Staff {
        \set Staff.instrumentName = #"Guitar"
        \set Staff.midiInstrument = "acoustic guitar (nylon)"
        \guitar
    }
    >>
    \midi { }
    \layout{ }
}

