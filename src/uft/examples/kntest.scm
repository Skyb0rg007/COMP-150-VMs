
; ExpLit
(let* ([r0 1]
       [r1 1])
  (check r0 "1")
  (expect r1 "1"))
; ExpVar
(let* ([r0 1]
       [r1 r0]
       [r2 1])
  (check r1 "r0")
  (expect r2 "1"))
; ExpLet
(let ([r0 2])
  (let ([r2 2])
    (check r0 "2")
    (expect r2 "2")))
; ExpSet
(let* ([r0 1]
       [r1 2])
  (set r0 r1)
  (check r0 "r0")
  (expect r1 "2"))
; ExpSeq
(let* ([r0 1]
       [r1 1]
       [r2 2])
  (begin
    (set r1 r2)
    (set r0 r1))
  (check r0 "r0")
  (expect r2 "2"))
; ExpCmd
(let* ([r0 2]
       [r1 2]
       [r3 (+ r0 r1)]
       [r4 4])
  (check r3 "(+ 2 2)")
  (expect r4 "4"))

