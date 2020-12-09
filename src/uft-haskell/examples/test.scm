
(define (main)
  (define (foo)
    (bar 11))
  (define (bar x)
    x)
  (define x (bar 13))
  (define y (bar 12))
  (println x)
  (println y)
  (println (foo)))

(main)
