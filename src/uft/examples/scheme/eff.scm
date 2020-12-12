#! /usr/bin/env scheme-script

(import (chezscheme) (control))

(define-syntax effect
  (lambda (x)
    (define (construct-name template . parts)
      (define (to-string x)
        (if (string? x)
          x
          (symbol->string (syntax->datum x))))
      (datum->syntax template
        (string->symbol (apply string-append (map to-string parts)))))
    (syntax-case x ()
      ((_ name)
       (with-syntax ([make-eff     (construct-name #'name 'make '- #'name)]
                     [eff-args     (construct-name #'name #'name '- 'args)]
                     [eff-cont     (construct-name #'name #'name '- 'cont)]
                     [check-eff    (construct-name #'name #'name '?)]
                     [prompt-name  (construct-name #'name 'prompt '- #'name)]
                     [perform-name (construct-name #'name 'perform '- #'name)]
                     [shallow-handle-name  (construct-name #'name 'shallow-handle '- #'name)]
                     [handle-name  (construct-name #'name 'handle '- #'name)])
         #'(begin
             (define-record-type name (fields cont args))
             (define prompt-name (new-prompt))
             (define (perform-name . args)
               (take-subcont prompt-name sk (make-eff sk args)))
             (define (shallow-handle-name handler)
               (lambda (f)
                 (let ([res (push-prompt prompt-name (f))])
                   (if (check-eff res)
                     (handler (lambda (x) (push-subcont (eff-cont res) x)) (eff-args res))
                     res))))
             (define (handle-name handler)
               (letrec ([h (lambda (f)
                             (let ([res (push-prompt prompt-name (f))])
                               (if (check-eff res)
                                 (h (lambda ()
                                      (handler (lambda (x) (push-subcont (eff-cont res) x)) (eff-args res))))
                                 res)))])
                 h))))))))

(effect print)

(define (example)
  (perform-print 1)
  (perform-print 2)
  (perform-print 3))

(define handle-print-once
  (handle-print (lambda (k args) (format #t "~s\n" (car args)))))

(define handle-all-prints
  (handle-print (lambda (k args) (format #t "~s\n" (car args)) (k #f))))

(define shallow-handle
  (shallow-handle-print (lambda (k args) (format #t "SHALLOW: ~s\n" (car args)) (k #f))))

(handle-print-once example)
(handle-all-prints
  (lambda ()
    (shallow-handle example)))
