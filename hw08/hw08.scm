(define (ascending? s) 
    (if
        (null? s)
        #t
        (if 
            (null? (cdr s))
            #t
            (if
                (> (car s) (car (cdr s)))
                #f
                (ascending? (cdr s))))))

(define (my-filter pred s) 
    (cond 
        ((null? s) '())
        ((pred (car s)) (cons (car s) (my-filter pred (cdr s))))
        (else (my-filter pred (cdr s)))))

(define (interleave lst1 lst2) 
    (if 
        (or 
            (null? lst1) 
            (null? lst2))
        (append lst1 lst2)
        (cons 
            (car lst1)
            (cons
                (car lst2)
                (interleave (cdr lst1) (cdr lst2))))))

(define (no-repeats s) 
    (if 
        (null? s)
        nil
        (append 
            (list(car s))
            (no-repeats (filter (lambda (x) (not (= x (car s)))) (cdr s))))))
