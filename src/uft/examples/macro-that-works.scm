#! /usr/bin/env -S scheme --program

(import (chezscheme))

(define-record-type special
  (fields name fun))

(define-record-type macro
  (fields clause env))

(define-record-type ident
  (fields name id))

(record-type-equal-procedure (record-type-descriptor ident)
  (lambda (x1 x2 eql?)
    #t
    #;(and (equal? (ident-name x1) (ident-name x2)) (= (ident-id x1) (ident-id x2)))))

(write (equal? (make-ident 'foo 0) (make-ident 'foo 0)))

#| 
(define (lookup env ident)
  (cond
    ((assq ident env) => cdr)
    (else ident)))

(define (bind env x v)
  (cons (cons x v) env))

(define (divert e1 e2)
  (if (null? e2)
    e1
    (bind (divert e1 (cdr e2)) (caar e2) (cdar e2))))

(define env1 (bind '() (make-ident 'foo 0) (make-ident 'foo 0)))
(define env2 (bind '() (make-ident 'bar 0) (make-ident 'bar 0)))

(define env3 (divert env1 env2))

; (newline)
; (write (lookup env1 'foo))
; (newline)
; (write (lookup env3 'foo))
; (newline)
; (write (lookup env3 'bar))
; (newline)
 |#
