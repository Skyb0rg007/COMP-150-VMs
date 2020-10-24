#! /usr/bin/env scheme-script

(import (chezscheme))

; Ignore
(define-syntax : (syntax-rules () ((_ _ _) (begin))))

(: fib (-> Number Number))
(define (fib n)
  (cond [(= 0 n) 1]
        [(= 1 n) 1]
        [else    (+ (fib (- n 1)) (fib (- n 2)))]))

(format #t "(fib 23) == ~d~%" (fib 23))

