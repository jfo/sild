; some aliases I like to use!
(define def define)
(def λ lambda)

; some old fashioned lisp functions
(def cadr (λ (l) (car (cdr l))))
(def caadr (λ (l) (car (car (cdr l)))))

; how do we reverse a list? Like this:
(def reverse (λ (l) (revinner l '())))
(def revinner
 (λ (l acc)
  (cond l (revinner (cdr l) (cons (car l) acc))
        acc)))

; like ruby's unshift: pushing (cons-ing) to the end of a list
(def unshift
  (λ (el l)
    (reverse (cons el (reverse l)))))

; -----------------------------
; here's the unit function. This monad will have the form:
;                       (() ())
; that is to say, it must be a list with two lists inside of it.
; The first will be the value, the second will be the record (history) of all the
; procedures that were bound to the monad. `unit` then will take a value (a
; list), and 'wrap' it into a monad with a blank history.
(def unit
  (λ (x)
    (cons x '(()))))
; m-make is just a convenience function to take two lists and wrap them up into
; a monad
(def m-make
  (λ (val hist)
    (cons val (cons hist '()))))

; some aliases to make usage clearer when applied to monads.
(def get-val car)
(def get-hist cadr)
(def get-most-recent-hist caadr)

; takes a symbol `sym` and a monad `Mx` and writes the symbol onto the end of
; the monad's history
(def write-to-hist
  (λ (sym Mx)
    (m-make (get-val Mx)
            (unshift sym (get-hist Mx)))))

; takes two monads, an old one and a new one. Combines the new's value with the
; old's history while appending the new's most recent history entry.
(def combine-hist
  (λ (m-old m-new)
; if both histories are equal, it's empty. We don't need to merge anything,
; just return the new one:
    (cond (eq (get-hist m-old) (get-hist m-new)) m-new
              (m-make
                (get-val m-new)
                (get-hist (write-to-hist (get-most-recent-hist m-new)
                                         m-old))))))

; it's our friend, bind aka `>>=`! in this case, we apply the function with
; signature `a -> Mb` to the extracted value, and combine histories, and we're
; done!
(def bind
  (λ (f Mx)
    (combine-hist Mx
                  (f (get-val Mx)))))

; aka `>>`
(def compose
  (λ (g f)
    (λ (m) (bind f (bind g m)))))

; takes a datum to push into something and a name for the function to be
; recorded into history as and returns a function that pushes into a value and
; returns a monad
(def makepusher
  (λ (datum name)
    (λ (l) (write-to-hist name (unit (cons sym l))))))

(def push-a (makepusher 'a 'push-a))
(def push-b (makepusher 'b 'push-b))
(def push-c (makepusher 'c 'push-c))
; this will break if the monad's value list is empty! caveat lisper
(def pop    (λ (l) (write-to-hist 'pop (unit (cdr l)))))

; an initial value state to play with
(def y '(a b c))
; an initial monad with that initial value state to play with
(def My (unit y))

; left identity
(display (bind pop My))  ; ((b c) (pop))
(display (pop y))        ; ((b c) (pop))

; right identity
(display My)             ; ((a b c) ())
(display (bind unit My)) ; ((a b c) ())

; associativity
(display (bind push-c ((compose push-a pop) My))) ; ((c a b c) (push-a pop push-c))
(display ((compose pop push-c) (bind push-a My))) ; ((c a b c) (push-a pop push-c))

(display (bind push-c
          (bind pop
           (bind pop
            (bind push-a
             (bind push-b
              (bind push-c
               (bind pop
                (bind pop My)))))))))
; ((c c c) (pop pop push-c push-b push-a pop pop push-c))
