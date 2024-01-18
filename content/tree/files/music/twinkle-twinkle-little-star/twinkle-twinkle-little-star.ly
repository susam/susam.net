\version "2.14.2"
\pointAndClickOff

\header {
    title = "Twinkle twinkle little star"
    composer = "Susam Pal"
    tagline = "Copyright © 2013 Susam Pal"
}

% ---- LEFT HAND -----

pianoIntroBass = \relative c {
    % 4 bars
    c8 e g e c e g e
    c8 f a f c f a f
    c8 e g e c e g e
    c8 f a f c f a f
}

pianoIntroTreble = {
    % 4 bars
    r1 r r r
}

melodyBass = \relative c {
    % 12 bars
    c8 e g e c e g e
    c8 f a f c e g e

    c8 f a f c e g e
    b8 d g d c e g e

    c8 e g e c f a f
    c8 e g e b d g d

    c8 e g e c f a f
    c8 e g e b d g d

    c8 e g e c e g e
    c8 f a f c e g e

    c8 f a f c e g e
    b8 d g d c e g e
}

melody = {
    % 12 bars
    c4 c g' g
    a a g2
    f4 f e e
    d d c2
    g'4 g f f
    e e d2
    g4 g f f
    e e d2
    c4 c g' g
    a4 a g2
    f4 f e e
    d d c2
}

melodyTreble = \relative c' { 
    \melody
}

melodyTrebleHigh = \relative c'' { 
    \melody
}

outroBass = \relative c {
    % 4 bars
    c8 e g e c e g e
    c8 f a f c e g e

    c8 f a f c e g e
    b8 d g d c2
}

outroTreble = \relative c' {
    % 4 bars
    c4 c g' g
    a a g2
    f4 f e e
    d d c2
}

strings = \relative c {
    % 16 bars 
    r1 r r r r r r r r r r r r r r r

    % 12 bars
    c~ c
    f~ f
    g~ g~ g~ g
    c,~ c
    f~ f
    
    % 12 bars
    <c c,>~ <c c,>
    <f f,>~ <f f,>
    <g g,>~ <g g,>~ <g g,>~ <g g,> 
    <c, c,>~ <c c,>
    <f f,>~ <f f,>

    % 12 bars
    <c c,>~ <c c,>
    <f f,>~ <f f,>
    <g g,>~ <g g,>~ <g g,>~ <g g,> 
    <c, c,>~ <c c,>
    <f f,>~ <f f,>

    % 4 bars
    <c c,>~ <c c,>~
    <c c,>~ <c c,>~
}

pad = \relative c'' {
    % 40 bars
    r1 r r r  r r r r
    r1 r r r  r r r r
    r1 r r r  r r r r
    r1 r r r  r r r r
    r1 r r r  r r r r

    % 12 bars
    c2 g' a g
    f e d c
    g' f e d
    g f e d
    c2 g' a g
    f e d c

    % 4 bars
    c2 g' a g
    f e d c
}

box = \relative c''' {
    % 48 bars
    r1 r r r  r r r r
    r1 r r r  r r r r
    r1 r r r  r r r r
    r1 r r r  r r r r
    r1 r r r  r r r r
    r1 r r r  r r r r

    % 4 bars
    c8 e g c, e g c, e
    c8 f a c, f a c, f
    c8 f a c, f a c, f
    c8 e g c, e g c, e

    % 4 bars
    c8 e g c, e g c, e
    c8 f a c, f a c, f
    c8 f a c, f a c, f
    c8 e g e c2
}

\score {
    <<
    \new PianoStaff <<
        \set PianoStaff.instrumentName = #"Piano"
        \set PianoStaff.midiInstrument = "acoustic grand"
        \new Staff {
            \time 4/4
            \tempo 4 = 120

            \pianoIntroTreble
            \melodyTreble
            \melodyTreble
            \melodyTrebleHigh
            \melodyTreble
            \outroTreble
        }

        \new Staff {
            \clef "bass"

            \pianoIntroBass
            \melodyBass
            \melodyBass
            \melodyBass
            \melodyBass
            \outroBass
        }
    >>

    \new Staff {
        \clef "bass"
        \set Staff.instrumentName = #"Slow strings"
        \set Staff.midiInstrument = "string ensemble 1"
        \strings
    }

    \new Staff {
        \set Staff.instrumentName = #"Xenon pad"
        \set Staff.midiInstrument = "violin"
        \pad
    }

    \new Staff {
        \set Staff.instrumentName = #"Music box"
        \set Staff.midiInstrument = "acoustic guitar (nylon)"
        \box
    }
    >>
    \midi { }
    \layout{ }
}

