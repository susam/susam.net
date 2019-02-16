\version "2.12.3"
\pointAndClickOff

\header {
    title = "Flowers"
    composer = "Susam Pal"
    tagline = "Copyright (C) 2010 Susam Pal"
}

% ---- LEFT HAND -----
introMajors = \relative c {
    c4 f a~
    a2.
    c,4 f a~
    a2.
    c,4 f a~
    a2.
    d,4 g b~
    b2.
    d,4 g b~
}

riseAndFallChords = \relative c {
    b'2.
    f4 a c~
    c2.
    c,4 f a~
    a2.
    c,4 f a~
    a2.
    a,4 d f~
    f2.
    g,4 c e~
    e2.
    a,4 c e~
    e2.
    g,4 b d~
    d2.
    g,4 c e~
    e2.
    g,4 c e
}

riseMajors = \relative c {
    c2. b d c e d

    c4 e g~
    g2.
    c,4 e g~
    g2.
    c,4 f a~
    a2.
    c,4 e g~
}

bridgeMajors = \relative c {
    g'2.
    c,4 e g~
    g2.
    c,4 e g~
}

mainRiffMajors = \relative c {
    g'2.
    c,4 f a~
    a2.
    c,4 f a~
    a2.
    d,4 g b~
    b2.
    d,4 g b~
}

excitementMajors = \relative c {
    b'2.
    f4 a c~
    c2.
    e,4 g c~
    c2.
    d,4 g b~
    b2.
    c,4 e g~
    g2.
    c,4 f a~
    a2.
    d,4 g b~
    b2.
    c,4 f a~
    a2.
    c,4 e g~
}

outroMajors = \relative c {
    b'2.

    c,4 f a~
    a2.
    c,4 f a~
    a2.
    c,4 f a~
    a2.
    c,4 f a~
    a2.
    <c, f a>2.
}

% ---- RIGHT HAND ----

mainRiff = \relative c' {
    c4 e g
    f2.
    c4 e g
    a2.
    d,4 f a
    g2.
    d4 f a
    b2.
}

riseAndFall = \relative c'' {
    a4 c e
    d2.
    a4 g f
    d'2.
    a8 g f4 g
    c2.
    a8 g f4 e
    d2.
    d8 c b4 c
    g2.

    a4 a c
    b2.
    a4 b c
    d2.
    a4 b c
    e2.
    c8 b a4 b8 c
    e2.
}

rise = \relative c' {
    c4 b a
    b c d
    d c b
    c d e
    e d c
    d e f
    g2.
    d4 e f
    g2.
    a4 g f
    g2.
    f4 e d
    c2.
}

bridge = \relative c' {
    d4 c b
    c2.
    d4 e d
    c2.
}

excitement = \relative c'' {
    g4 c b
    a2.
    g4 g f
    e2.
    e4 f g
    d2.
    e4 f a
    g2.
    g4 e f
    a2.
    g4 e f
    d2.
    g4 e f
    a2.
    g4 e f
    c2.
}

outro = \relative c' {
    c4 e g
    f2.
    c4 e g
    a2.
    c,4 e g
    f2.
    c4 e g
    f2.
    c4 e g
    f2.
}

bass = {
    \set tieWaitForNote = ##t
    \introMajors
    \repeat volta 2 {
        \riseAndFallChords
        \riseMajors
        \bridgeMajors
        \mainRiffMajors
        \excitementMajors
        \bridgeMajors
        \mainRiffMajors
    }
    \outroMajors
    \bar "|."
}

treble = { 
    \set tieWaitForNote = ##t
    R2.
    \mainRiff
    \repeat volta 2 {
        \riseAndFall
        \rise
        \bridge
        \mainRiff
        \excitement
        \bridge
        \mainRiff
    }
    \outro
    \bar "|."
}

\score {
    \new PianoStaff <<
        \new Staff {
            \time 3/4
            \tempo 4 = 156
            \treble

        }

        \new Staff {
            \clef "bass"
            \bass
        }

    >>
    \layout{ }
}

\score {
    \new PianoStaff <<
        \set PianoStaff.midiInstrument = "acoustic grand"
        \new Staff {
            \time 3/4
            \tempo 4 = 156
            \unfoldRepeats \treble

        }

        \new Staff {
            \clef "bass"
            \unfoldRepeats \bass
        }

    >>
    \midi { }
}

