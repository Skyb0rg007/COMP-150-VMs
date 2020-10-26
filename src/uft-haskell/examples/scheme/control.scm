;; From delimcc.scm - http://okmij.org/ftp/continuations/implementations.html

(library (control)
  (export
    ; procedure: (new-prompt)
    ; Returns a fresh prompt, eq? only to itself
    ; Note: all prompts are equal? to one another
    new-prompt
    ; syntax: (push-prompt p e1 e2 ...)
    ; Sets up the prompt p, then evaluates expressions e1 e2 ...
    push-prompt
    ; syntax: (abortP p e)
    ; Flushes the stack up to and including the most recent push-prompt with prompt p,
    ; then evaluate expression e in the remaining context
    abortP
    ; syntax: (take-subcont p sk e1 e2 ...)
    ; Captures continuation up to most recent push-prompt with prompt p,
    ; binds the continuation to variable sk before evaluating expressions e1 e2 ...
    take-subcont
    ; syntax: (push-subcont sk e1 e2 ...)
    ; Re-instates the continuation sk, then evaluates expressions e1 e2 ...
    push-subcont
    ; syntax: (push-delim-subcont sk e1 e2 ...)
    ; Re-instates the continuation sk, then evaluates expressions e1 e2 ...
    ; Inserts push-prompt underneath the reinstated continuation
    push-delim-subcont
    ; syntax: (shift p f e1 e2)
    ; Captures continuation up to most recent push-prompt with prompt p,
    ; binds a function re-instating the continuation with inserted push-prompt to variable f,
    ; then evaluates expressions e1 e2 ... under a push-prompt
    shift
    ; syntax: (shift0 p f e1 e2)
    ; Captures continuation up to most recent push-prompt with prompt p,
    ; binds a function re-instating the continuation with inserted push-prompt to variable f,
    ; then evaluates expressions e1 e2 ...
    shift0
    ; syntax: (shift p f e1 e2)
    ; Captures continuation up to most recent push-prompt with prompt p,
    ; binds a function re-instating the continuation to variable f,
    ; then evaluates expressions e1 e2 ... under a push-prompt
    control
    ; procedure: (prompt-set? p)
    ; Returns #t if the current context contains prompt p, #f otherwise
    prompt-set?)
  (import (chezscheme))
  
  (define pstack '())
  (define go #f)

  (define (new-prompt)
    (list #f)) ; Implement prompts as lists
  
  (define (push-prompt* p th)
    ((call/cc
       (lambda (k)
         (set! pstack (cons (cons p k) pstack))
         (go th)))))
  
  (define (unwind acc p pstack)
    (if (null? pstack)
      (error 'unwind "No prompt was set")
      (if (eq? p (caar pstack))
        (cons pstack acc)
        (unwind (cons (car pstack) acc) p (cdr pstack)))))
  
  (define (unwind-abort p pstack)
    (if (null? pstack)
      (error 'unwind-abort "No prompt was set")
      (if (eq? p (caar pstack))
        pstack
        (unwind-abort p (cdr pstack)))))
  
  (define (take-subcont* p f)
    ((call/cc
       (lambda (k)
         (let* ([subchain-pstack (unwind '() p pstack)]
                [_ (set! pstack (car subchain-pstack))]
                [subchain (cdr subchain-pstack)])
           (go (f (vector k p subchain))))))))
  
  (define (push-subcont* sk m)
    ((call/cc
       (lambda (k)
         (let ([p** (new-prompt)]
               [ekfrag (vector-ref sk 0)]
               [subchain (vector-ref sk 2)])
           (set! pstack (cons (cons p** k) pstack))
           (for-each
             (lambda (frame)
               (set! pstack (cons frame pstack)))
             subchain)
           (ekfrag m))))))
  
  (define (push-delim-subcont* sk m)
    ((call/cc
       (lambda (k)
         (let ([p (vector-ref sk 1)]
               [ekfrag (vector-ref sk 0)]
               [subchain (vector-ref sk 2)])
           (set! pstack (cons (cons p k) pstack))
           (for-each
             (lambda (frame)
               (set! pstack (cons frame pstack)))
             subchain)
           (ekfrag m))))))
  
  (define (abort* p th)
    (let* ([pstack-new (unwind-abort p pstack)]
           [h (car pstack-new)])
      (set! pstack (cdr pstack-new))
      ((cdr h) th)))
  
  (define (prompt-set? p)
    (assq p pstack))
  
  ;;
  
  (define-syntax push-prompt
    (syntax-rules ()
      ((_ p e1 e2 ...)
       (push-prompt* p (lambda () e1 e2 ...)))))
  
  (define-syntax abortP
    (syntax-rules ()
      ((_ p e)
       (abort* p (lambda () e)))))
  
  (define-syntax take-subcont
    (syntax-rules ()
      ((_ p sk e1 e2 ...)
       (take-subcont* p (lambda (sk) (lambda () e1 e2 ...))))))
  
  (define-syntax push-subcont
    (syntax-rules ()
      ((_ sk e1 e2 ...)
       (push-subcont* sk (lambda () e1 e2 ...)))))
  
  (define-syntax push-delim-subcont
    (syntax-rules ()
      ((_ sk e1 e2 ...)
       (push-delim-subcont* sk (lambda () e1 e2 ...)))))
  
  (define-syntax shift
    (syntax-rules ()
      ((_ p f e1 e2 ...)
       (take-subcont p sk
         (let ([f (lambda (v) (push-delim-subcont sk v))])
           (push-prompt p e1 e2 ...))))))
  
  (define-syntax shift0
    (syntax-rules ()
      ((_ p f e1 e2 ...)
       (take-subcont p sk
         (let ([f (lambda (v) (push-delim-subcont sk v))])
           e1 e2 ...)))))
  
  (define-syntax control
    (syntax-rules ()
      ((_ p f e1 e2 ...)
       (take-subcont p sk
         (let ([f (lambda (v) (push-subcont sk v))])
           (push-prompt p e1 e2 ...))))))
  
  (let
    ([v (call/cc
          (lambda (k)
            (set! go k)
            (k #f)))])
    (if v
      (let* ([r (v)]
             [h (car pstack)]
             [_ (set! pstack (cdr pstack))])
        ((cdr h) (lambda () r)))))

)



