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
      (let (y )
        (* y y y))))

(define (cddr s) (cdr (cdr s)))

(define (cadr s) 'YOUR-CODE-HERE)

(define (caddr s) 'YOUR-CODE-HERE)
