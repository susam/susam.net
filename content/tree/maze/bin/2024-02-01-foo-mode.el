(define-minor-mode foo-mode
  "Foo mode to perform foo."
  :lighter " Foo"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") #'do-foo)
            map)
  (if foo-mode
      (message "foo-mode enabled")
    (message "foo-mode disabled")))

(defun do-foo ()
  (interactive)
  (message "do-foo"))
