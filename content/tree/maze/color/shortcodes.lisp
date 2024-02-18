;;;; Find nearest 3-digit (hex) color codes for given 6-digit color codes.

(defun nearest (code)
  (let ((min-diff 16)
        (min-code))
    (dotimes (c 16)
      (let* ((repeated-c (+ (* c 16) c))
             (difference (abs (- code repeated-c))))
        (when (< difference min-diff)
          (setf min-diff difference)
          (setf min-code c))))
    min-code))

(defun nearest-short-code (code description)
  (let ((r (parse-integer (subseq code 0 2) :radix 16))
        (g (parse-integer (subseq code 2 4) :radix 16))
        (b (parse-integer (subseq code 4 6) :radix 16)))
    (format t "~a -> ~x~x~x (~a)~%"
            code (nearest r) (nearest g) (nearest b) description)))

(nearest-short-code "526447" "garden green") ; https://encycolorpedia.com/526447
(nearest-short-code "6f4e37" "coffee")       ; https://encycolorpedia.com/6f4e37
(nearest-short-code "873b3c" "wine cask")    ; https://encycolorpedia.com/873b3c
(nearest-short-code "774d3f" "wine barrel")  ; https://encycolorpedia.com/774d3f
(nearest-short-code "bea493" "mocha")        ; https://encycolorpedia.com/bea493
