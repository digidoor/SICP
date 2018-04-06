#lang sicp
(#%require sicp-pict)

(define (location wrd sent);this is a great exercise
  (cond ((null? sent) #f)
        ((equal? (car sent) wrd) 1)
        (#t (let( (sub (location wrd (cdr sent))) )
              (if (equal? sub #f)
                  #f
                  (+ 1 sub))))))
(set! location location)

(define (divisible? big small)
  (= (remainder big small) 0))
(define (fizzbuzz n)
  (cond ( (and (divisible? n 3) (divisible? n 5)) 'fizzbuzz)
        ( (divisible? n 3) 'fizz)
        ( (divisible? n 5) 'buzz)
        (#t n)))

(define (factorial n);recursive pattern
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))
(set! factorial factorial)

(define (fact-iter n);iterative pattern
  (define (fact-help cnt product)
    (if (> cnt n)
        product
        (fact-help (inc cnt) (* cnt product))))
  (fact-help 1 1))
(set! fact-iter fact-iter)

(define (fiber n) ;iterative fibonnaci
  (define (fibiter ante pen cnt)
    (if (= n cnt)
        pen
        (fibiter pen (+ ante pen) (+ 1 cnt))))
  (fibiter 0 1 1))
(set! fiber fiber)

(define (make-add-suffix s2);a simply currying function.
  (lambda (s) (string-append s s2)));((make-add-suffix "!") "shit") returns "shit!"

(define ((type-check f pred) x);currying shorthand
  (if (pred x)
      (f x)
      #f))
;(define type-check (lambda (f pred);"real" way
;                     (lambda (x) (if (pred x)
;                                     (f x)
;                                     #f))))

(define safe-sqrt (type-check sqrt number?))

(define (pwr x y);returns x^y, i.e. x to the power of y
  (define (dispwr x) (display x) (display "^2") (newline))
  (if (= 0 y)
      1;using let avoids redundant branching recursive calls
      (let ((root (pwr x (quotient y 2))))
        (if (= 0 (remainder y 2))
            (and (dispwr root) (* root root))
            (and (display x) (display "*") (dispwr root) (* x root root))))))
(set! pwr pwr)

;;inefficient method, use (trace badpwr) to see the redundant calls.
(define (badpwr x y)
  (if (= 0 y)
      1
      (if (= 0 (remainder y 2))
;;doubles the number of additional recursive calls for each level of recursion
          (* (badpwr x (quotient y 2)) (badpwr x (quotient y 2)))
          (* x (badpwr x (quotient y 2)) (badpwr x (quotient y 2))))))
(set! badpwr badpwr)

(define minute 60) (define hour (* 60 minute)) (define day (* 24 hour))
(define (describe-time t)
  (cond ((>= t day) (display (quotient t day))
                    (display " days ")
                    (describe-time (remainder t day)))
        ((>= t hour) (display (quotient t hour))
                     (display " hours ")
                     (describe-time (remainder t hour)))
        ((>= t minute) (display (quotient t minute))
                       (display " minutes ")
                       (describe-time (remainder t minute)))
        (#t (display t)
            (display " seconds"))))

(define (differences lst)
  (if (null? (cdr lst))
      '()
      (cons (- (cadr lst) (car lst)) (differences (cdr lst)))))

(define (remove-once wrd sent)
  (cond ((null? sent) '())
        ((equal? (car sent) wrd) (cdr sent))
        (#t (cons (car sent) (remove-once wrd (cdr sent))))))

(define (remove-all wrd sent)
  (cond ((null? sent) '())
        ((equal? (car sent) wrd) (remove-all wrd (cdr sent)))
        (#t (cons (car sent) (remove-all wrd (cdr sent))))))
(set! remove-all remove-all)

(define (copies n wrd)
  (if (= 0 n)
      '()
      (cons wrd (copies (- n 1) wrd))))

(define (member? wrd sent)
  (cond ((null? sent) #f)
        ((equal? (car sent) wrd) #t)
        (#t (member? wrd (cdr sent)))))
(define (rem-dupls sent)
  (cond ((null? sent) '())
        ((member? (car sent) (cdr sent)) (rem-dupls (cdr sent)))
        (#t (cons (car sent) (rem-dupls (cdr sent))))))

(define (count-word sent wrd)
  (cond ((null? sent) 0)
        ((equal? (car sent) wrd) (+ 1 (count-word (cdr sent) wrd)))
        (#t (count-word (cdr sent) wrd))))

(define (squares sent)
  (define (square x) (* x x))
  (if (null? sent)
      '()
      (cons (square (car sent)) (squares (cdr sent)))))

(define (ordered? sent)
  (cond ((null? (cdr sent)) #t)
        ((> (car sent) (cadr sent)) #f)
        (#t (ordered? (cdr sent)))))

(define (squaroot x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (fixed-point f first-guess)
  (define (close-enough? x1 x2)
    (< (abs (- x1 x2)) 0.000001))
  (define (try guess)
    (let( (next (f guess)) )
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (substitute sent old new)
  (cond ((null? sent) '())
        ((equal? (car sent) old) (cons new (substitute (cdr sent) old new)))
        (#t (cons (car sent) (substitute (cdr sent) old new)))))

(define f1 "fuck off")
(define (f2) (display "fuck off"))
(define (f3 x) (* x 10))
(define f4 (lambda () (lambda () (display "fuck off"))))
(define (((f5)) x)
  (cond ((> x 0) (display "fuck off") (newline) (((f5)) (- x 1)))))
(define (t f)
  (lambda (x) (f (f (f x)))) )

(define (make-tester w)
  (lambda (x) (equal? w x)))

;(define (sum term a next b)
;  (if (> a b)
;      0
;      (and (display (term a)) (newline) (+ (term a) (sum term (next a) next b)))))

(define (sum term a next b);ugly iterative version
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fecterial n) (product (lambda (x) x) 1 inc n))

(define (pi-approx n)
  (define (pi-term x) (/ (* x (+ x 2))
                         (square (inc x))))
  (* 4.0
     (product pi-term 2 (lambda (x) (+ x 2)) n)))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;(define (sum term a next b) (accumulate + 0 term a next b))
;(define (product term a next b) (accumulate * 1 term a next b))

(define (filtered-accumulate combiner null-value term a next b pred)
  (cond ((> a b) null-value)
        ((pred a) (combiner (term a) (filtered-accumulate combiner
                                                          null-value
                                                          term
                                                          (next a)
                                                          next
                                                          b
                                                          pred)))
        (#t (filtered-accumulate combiner null-value term (next a) next b pred))))
(define (rel-prime? x y)
  (= (gcd x y) 1))
(define (prod-of-some-numbers n)
  (filtered-accumulate * 1 (lambda (x) x) 1 inc n (lambda (x) (rel-prime? n x))))

