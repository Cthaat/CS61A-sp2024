(define (square n) (* n n))

(define (pow base exp) 
  (if (zero? exp) 1
      (if (even? exp)
          (square (pow base (/ exp 2)))
          (* base (pow base (- exp 1)))
      )
  )
)

(define (repeatedly-cube n x)
  (if (zero? n)
      x
      (let ((y (repeatedly-cube (- n 1) x)))
        (* y y y))))

(define (cddr s) (cdr (cdr s)))

(define (cadr s) (car (cdr s)))

(define (caddr s) (car (cddr s)))

(define (even-subset s)
  (if (null? s) nil
    (append (even-subset (cdr s))
      (map (lambda (t) (cons (car s) t))
        (if even? (car s)
          (even-subset (cdr s))
          ))))
)