(define return
 (lambda (x)
  (cons x '(hello_monad))))

(define >>=
 (lambda (f Mx)
   (f (car Mx))))

(define >>
 (lambda (fns Mx)
  (cond (eq fns '()) Mx
        (>> (cdr fns)
            (return (>>= (car fns)
                    Mx))))))

(define y '(| | |))
(define My (return y))

; left identity
(display (>>= car My)) ; |
(display (car y))      ; |

; right identity
(display (>>= return My)) ; ((| | |) hello_monad)
(display My)              ; ((| | |) hello_monad)

; associativity
(define addone (lambda (x) (cons '| x)))                    ; ((| | | | | | |) hello_monad)
(define addthree (lambda (x) (addone (addone (addone x))))) ; ((| | | | | | |) hello_monad)

(display (>> '(addone addthree) My))
(display (>> '(addthree addone) My))
