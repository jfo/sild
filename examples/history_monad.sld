(define cadr (lambda (l) (car (cdr l))))

(define unit
  (lambda (x)
    (cons x '(()))))

(define mergehistory
  (lambda (mold mnew)
    (cond (eq (cadr mold) (cadr mnew)) mnew
              (cons (car mnew)
                    (cons (cons (car (car (cdr mnew)))
                                (car (cdr mold)))
                          '())))))

(define bind
  (lambda (f Mx)
    (mergehistory Mx (f (car Mx)))))

(define recorder (lambda (mx fn)
                   (cons (car mx)
                         (cons (cons fn
                                     (car (cdr mx)))
                               '()))))

(define compose (lambda (g f)
                  (lambda (m) (bind f (bind g m)))))

(define push_a (lambda (x) (recorder (unit (cons 'a x)) 'push_a)))
(define push_b (lambda (x) (recorder (unit (cons 'b x)) 'push_b)))
(define push_c (lambda (x) (recorder (unit (cons 'c x)) 'push_c)))
(define pop    (lambda (x) (recorder (unit (cdr x)) 'pop)))


(define y '(a b c))
(define My (unit y))

; left identity
(display (bind pop My))  ; ((b c) (pop))
(display (pop y))        ; ((b c) (pop))

; right identity
(display My)             ; ((a b c) ())
(display (bind unit My)) ; ((a b c) ())

; associativity
(display (bind push_c ((compose push_a pop) My))) ; ((c a b c) (push_c pop push_a))
(display ((compose pop push_c) (bind push_a My))) ; ((c a b c) (push_c pop push_a))

(display (bind push_c
            (bind pop
                (bind pop
                    (bind push_a
                        (bind push_b
                            (bind push_c
                                (bind pop
                                    (bind pop My)))))))))
; ((c c c) (push_c pop pop push_a push_b push_c pop pop))

