
(library (queue)
  (export make-queue queue? queue-push! queue-enqueue! queue-pop! queue-length queue-empty?)
  (import (chezscheme))

  (define-record-type q
    (fields (mutable hd) (mutable tl)))

  (define (make-queue)
    (make-q '() #f))

  (define (queue? q) (q? q))

  (define (queue-empty? q)
    (null? (q-hd q)))

  (define (q-empty-check q who)
    (if (queue-empty? q)
      (error who "Queue is empty")))

  (define (queue-enqueue! q x)
    (let ([h (list x)])
      (if (null? (q-hd q))
        (q-hd-set! q h)
        (set-cdr! (q-tl q) h))
      (q-tl-set! q h))
    q)

  (define (queue-push! q x)
    (let ([h (cons x (q-hd q))])
      (q-hd-set! q h)
      (if (not (q-tl q))
        (q-tl-set! q h)))
    q)

  (define (queue-pop! q)
    (q-empty-check q 'queue-pop!)
    (let ([it   (car (q-hd q))]
          [next (cdr (q-hd q))])
      (if (null? next)
        (q-tl-set! q #f))
      (q-hd-set! q next)
      it))

  (define (queue-length q)
    (length (q-hd q)))

  )
