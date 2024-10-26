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

(define (even-subsets s) 
  (if (null? s)
    nil
    (append 
      (even-subsets (cdr s))
      (map (lambda (t) (cons (car s) t))
        (if (even? (car s))
          (even-subsets (cdr s))
          (odd-subsets (cdr s))))
      (if (even? (car s))
        (list (list (car s)))
        nil))))

(define (odd-subsets s) 
  (if (null? s)
    nil
    (append 
      (odd-subsets (cdr s))
      (map (lambda (t) (cons (car s) t))
        (if (odd? (car s))
          (even-subsets (cdr s))
          (odd-subsets (cdr s))))
      (if (odd? (car s))
        (list (list (car s)))
        nil))))

(define (nonempty-subsets s)
  (if (null? s)
    nil
    (append 
      (nonempty-subsets (cdr s))
      (map 
        (lambda (t) (cons (car s) t))
        (nonempty-subsets (cdr s)))
      (list (list (car s))))))

(define (even-sub s)
  (filter 
    (lambda (x)
      (even? 
        (apply + x)))
    (nonempty-subsets s)))

(define (composed f g) 
  (lambda (x) (f (g x))))

(define (reduce f s a)
  (if (null? s)
    nil
    (composed f (reduce f ()))))