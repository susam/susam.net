;;; Copyright (c) 2022-2026 Susam Pal
;;; All rights reserved.

(("cmo" . (("cc-iant" "cc/iant/notes.html" "IANT")
           ("cc-ms-emacs" "cc/mastering-emacs/notes.html" "Mastering Emacs")
           ("cfrs" "cfrs.html" "CFRS[]")
           ("fxyt" "fxyt.html" "FXYT")
           ("hnskins" "code/news/hnskins/{{ index }}" "HN Skins")
           ("mathb" "code/news/mathb/{{ index }}" "MathB")
           ("mvs" "code/news/mvs/{{ index }}" )
           ("myrgb" "myrgb.html" "Guess My RGB")
           ("nq" "nq.html" "Nerd Quiz")
           ("quickqwerty" "quickqwerty.html" "QuickQWERTY")
           ("wander" "wander/{{ index }}" "Wander Console")))
 ("style" . (("font-family" . "georgia, serif")
             ;; Light colour scheme.
             ("lt-fill-colour" . "#eee") ; 1.2
             ("lt-edge-colour" . "#ccc") ; 1.6
             ("lt-code-colour" . "#050") ; 2.3, 7.9, 9.1
             ("lt-samp-colour" . "#730") ; 2.3, 8.0, 9.3
             ("lt-line-colour" . "#999") ; 2.8
             ("lt-okay-colour" . "#060") ; 7.2
             ("lt-err-colour" . "#900")  ; 8.9
             ;; Dark scheme overrides.
             ("dk-fill-colour" . "#000") ; 1.1
             ("dk-edge-colour" . "#333") ; 1.5
             ("dk-code-colour" . "#9c6") ; 1.0, 11.2, 10.1
             ("dk-samp-colour" . "#db0") ; 1.0, 11.2, 10.1
             ("dk-line-colour" . "#666") ; 3.3
             ("dk-okay-colour" . "#3c6") ; 9.0
             ("dk-err-colour" . "#f99")
             ;; Dark scheme special colours.
             ("dk-bg-colour" . "#111")
             ("dk-body-colour" . "#bbb") ; 10.9, 9.8
             ("dk-link-colour" . "#9bf") ; 1.0, 9.8, 9.8
             ("dk-lv-colour" . "#a9f")   ; 1.3, 7.8, 9.8
             ("dk-la-colour" . "#f99"))) ; 1.1, 9.2, 9.8
 ;; Map zone key -> (index path, zone name).
 ;; Zone key may be either list name or doc-path prefix.
 ("zones" . (("alt" "{{ root }}maze.html" "Maze")
             ("cc/" "{{ root }}cc/{{ index }}" "Club"))))
