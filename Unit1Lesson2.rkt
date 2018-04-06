#lang sicp
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
