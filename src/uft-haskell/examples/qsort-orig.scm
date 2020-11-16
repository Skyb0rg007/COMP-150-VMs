
#;(let ([pivot ((global car) xs)])
  ((global append) pivot?))

(letrec
  ([o (lambda (f g) (lambda (x) (f (g x))))]
   [qsort
     (lambda (xs)
       (if ((global null?) xs)
         '()
         (let ([pivot  ((global car) xs)])
           (let ([rest   ((global cdr) xs)])
             (let ([right? (lambda (n) ((global >) n pivot))])
               (let ([left?  ((global o) (global not) right?)])
                 ((global append)
                  (qsort (filter left? rest))
                  ((global cons) pivot ((global qsort) ((global filter) right? rest))))))))))])
  0)
