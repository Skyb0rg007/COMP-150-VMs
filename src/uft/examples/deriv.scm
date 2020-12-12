(define (list1 a)
  (cons a '()))

(define (list2 a b)
  (cons a (list1 b)))

(define (list3 a b c)
  (cons a (list2 b c)))

(define (list4 a b c d)
  (cons a (list3 b c d)))

(define (atom? x)
  (not (pair? x)))

(define (map f xs)
  (if (null? xs)
    '()
    (cons (f (car xs)) (map f (cdr xs)))))

(define (cadr x)
  (car (cdr x)))

(define (caddr x)
  (car (cdr (cdr x))))

(define = eq?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define N 32000)

(define (deriv-aux a) (list3 '/ (deriv a) a))

(define (deriv a)
  (if (atom? a)
      (begin
        (print "atom") (println a)
        (if (= a 'x) 1 0))
      (if (= (car a) '+)
          (cons '+ (map deriv (cdr a)))
          (if (= (car a) '-)
              (cons '- (map deriv (cdr a)))
              (if (= (car a) '*)
                  (list3 '* a (cons '+ (map deriv-aux (cdr a))))
                  (if (= (car a) '/)
                      (list3 '-
                             (list3 '/
                                    (deriv (cadr a))
                                    (caddr a))
                             (list3 '/
                                    (cadr a)
                                    (list4 ('* (caddr a) (caddr a)(deriv (caddr a))))))
                       'error))))))

(define (benchmark start end)
  (while (< start end)
    (let* ((formula '(+ (* 3 x x) (* a x x) (* b x) 5))
           (dx/df   (deriv formula)))
      (println formula)
      (println dx/df)
      (set! start (+ start 1)))))

(benchmark 0 1)
