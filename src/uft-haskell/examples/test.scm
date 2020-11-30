
(define x 12)
(define (foo bar)
  (let ((baz 12)
        (quux 13))
    (+ 1 2)))

(if '(foo bar () baz)
  #f)
