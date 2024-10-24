; (define (over-or-under num1 num2) 
;   (if (<  num1 num2) (
;     display -1))
;   (if (>  num1 num2) (
;     display 1))
;   (if (=  num1 num2) (
;     display 0)))

(define (over-or-under num1 num2)
  (cond ((< num1 num2) (display -1))
        ((> num1 num2) (display 1))
        (else (display 0))))

(define (make-adder num) 
  (lambda (x) (+ num x)))

(define (composed f g) 
  (lambda (x) (f (g x))))

(define (repeat f n) 
  if(< n 1)
    (lambda (x) x)
    ()
)

(define (max a b)
  (if (> a b)
      a
      b))

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b) 
  (cond ((zero? a) b)
        ((zero? b) a)
        ((= (modulo (max a b) (min a b)) 0) (min a b))
        (else (gcd (min a b) (modulo (max a b) (min a b)))))
      )
