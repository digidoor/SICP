#lang sicp
(define (square x) (* x x))
(define (double x) (* 2 x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define tolerance 0.00001)

(define (average-damp f)
  (lambda (y) (* 0.5 (+ y (f y)))))

(define (skwareroot x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (iterate start improve good-enough?)
  (if (good-enough? start)
      start
      (iterate (improve start) improve good-enough?)))

(define (largest-square total)
  (iterate 1
           inc
           (lambda (x) (< total ((lambda (y) (* y y)) (inc x))))))

(define (substitute sent old new)
  (if (null? sent)
      '()
      (if (equal? (car sent) old)
          (cons new (substitute (cdr sent) old new))
          (cons (car sent) (substitute (cdr sent) old new)))))

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (dec n)))
      f))

(define (my-repeated f n)
  (if (> n 1)
      (lambda (x) (f ((my-repeated f (dec n)) x)))
      f))

;(define (my-repeated f n)
;  (if (> n 0)
;      (lambda (x) (f ((my-repeated f (dec n)) x)))
;      (lambda (x) x)))

;(define (my-repeated f n)
;  (cond ((> n 1) (compose f (my-repeated f (dec n))))
;        ((= n 1) f)
;        ((= n 0) (lambda (x) x))))

;(define ((my-repeated f n) x)
;  (if (> n 1)
;      (f ((my-repeated f (dec n)) x))
;      (f x)))

;(define (my-repeated f n)
;  (define (loop i result)
;    (if (= i n)
;        result
;        (loop (inc i) (lambda (x) (f (result x))))))
;  (loop 1 f))

(define (my-every f sent)
  (if (null? sent)
      '()
      (cons (f (car sent)) (my-every f (cdr sent)))))