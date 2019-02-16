\version "2.12.3"
\pointAndClickOff

\header {
    title = "Waiting for dawn"
    composer = "Susam Pal"
    tagline = "Copyright (C) 2010 Susam Pal"
}

% ---- LEFT HAND -----

% Night
aeaeTwice = \relative a {
    a, e' a e
    a, e' a e
}

aeae = {
    % 8 measures
    \aeaeTwice
    \aeaeTwice
    \aeaeTwice
    \aeaeTwice
}

bege = \relative a, {
    % 4 measures
    b e g e
    b e g e
    b e g e
    b e g e
}

adfd = \relative a, {
    a d f d
    a d f d
    a d f d
    a d f d
}

egbg = \relative e {
    e g b g
    e g b g
    e g b g
    e g b g
}

% Dawn

cgcgOnce = \relative c' {
    c, g' c g
}

cgcg = {
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
}

cgcgLastTwoTied = \relative c' {
    c, g' c g~
    <c, g' c>1
}

cgcgLast = {
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
    \cgcgOnce
    \cgcgLastTwoTied
}

% Rising
risingLeft = \relative c {
    % 10 measures
    d4 g b g
    d g b g
    c, e g e
    c e g e
    c f a f
    c f a f
    d g b g
    d g b g
    d g b g
    d g b g
}

risingHigherLeft = \relative c {
    % 4 measures
    d g b g
    d g b g
    d g b g
    d g b g
}

% ---- RIGHT HAND -----

mainRiff = {
    % 8 measures
    c1 e g e f d e c
}

nightRiff = \relative c' {
   \mainRiff 
}

waitRiff = \relative c' {
    c e g a           % 4 measures
    f d b c           % 4 measures
    c g' b a          % 4 measures
    f d e c           % 4 measures
}

dawnRiff = \relative c'' {
    \mainRiff
}

dawnRiffFast = \relative c'' {
    c2. d4
    e1
    g2. f4
    e1
    f2. e4
    d1
    e2. d4
    c1
}

dawnRiffFaster = \relative c'' {
    c2. d4
    e2. f4
    g2. f4
    e1
    f2. e4
    d1
    e2. d4
    c1
}

doubleDawnRiff = \relative c'' {
    <c c'>1 <e e'> <g g'> <e e'>
    <f f'>1 <d d'> <e e'> <c c'>
}

doubleDawnRiffFast = \relative c'' {
    <c c'>2. <d d'>4
    <e e'>1
    <g g'>2. <f f'>4
    <e e'>1
    <f f'>2. <e e'>4
    <d d'>1
    <e e'>2. <d d'>4
    <c c'>1
}

welcomeDawnSlow = \relative c'' {
    g1 c e f d a c g
}

welcomeDawn = \relative c'' {
    g2. b4 | c1 | e2. g4 | f1
    d2. b4 | a2. b4 | c2. a4 | g1
}

risingRight = \relative c'' {
    % 10 measures
    a2. c4 | b1 |
    g2. f4 | e1 |
    f2. a4 | g1 |
    a2. b4 | c1 |
    a2. b4 | c1 |
}

risingHigherRight = \relative c''' {
    % 4 measures
    a2. b4 | c1 |
    a2. b4 | c1 |
}

bass = \relative c {
    % Night                 50 measures
    \aeaeTwice            % 2 measures
    \aeae                 % 8 measures
    \aeae                 % 8 measures
    \bege                 % 4 measures
    \adfd                 % 4 measures
    \egbg                 % 4 measures
    \adfd                 % 4 measures
    \aeae                 % 8 measures
    \aeae                 % 8 measures
                         
    % Dawn                 25 measures
    \cgcgOnce             % 1 measure
    \cgcg                 % 8 measures
    \cgcg                 % 8 measures
    \cgcg                 % 8 measures
                         
    % Rising             
    \risingLeft            % 10 measures
    \risingHigherLeft      %  4 measures

    % Dawn
    \cgcg                 % 8 measures
    \cgcg                 % 8 measures
    \cgcg                 % 8 measures

    % Rising             
    \risingLeft            % 10 measures

    % Dawn
    \cgcg                 % 8 measures
    \cgcg                 % 8 measures
    \cgcgLast             % 8 measures
}

treble = { 
    % Night                 50 measures
    r1 r1                 % 2 measures
    \nightRiff            % 8 measures
    \nightRiff            % 8 measures
    \waitRiff             % 16 measures
    \nightRiff            % 8 measures
    \nightRiff            % 8 measures
                          
    % Dawn                 25 measures
    r1                    % 1 measure
    \welcomeDawnSlow      % 8 measures
    \welcomeDawn          % 8 measures
    \welcomeDawn          % 8 measures
                         
    % Rising              
    \risingRight          % 10 measures
    \risingHigherRight    %  4 measures

    % Dawn
    \doubleDawnRiff       % 8 measures
    \doubleDawnRiffFast   % 8 measures
    \dawnRiffFaster       % 8 measures

    % Rising              
    \risingRight          % 10 measures

    % Dawn
    \dawnRiff             % 8 measures
    \dawnRiffFast         % 8 measures
    \dawnRiffFaster       % 8 measures
}

\score {
    \new PianoStaff <<
        \set PianoStaff.midiInstrument = "acoustic grand"
        \new Staff {
            \time 4/4
            \tempo 4 = 132
            \treble

        }

        \new Staff {
            \clef "bass"
            \bass
        }

    >>
    \midi { }
    \layout{ }
}

