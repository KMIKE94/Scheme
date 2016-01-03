(define plus
  (λ (x y)
    (+ x y)))


; (reverse-with-count '(a) '(2))
; (reverse-with-count '(a b c) '(1 2 3))
(define reverse-with-count
  (lambda (l1 l2)
    (cond ((or (eq? l1 '()) (eq? l2 '())) '())
          (else (flatten (cons (reverse-with-count (cdr l1) (cdr l2)) (multiply (car l1) (car l2)))))
      )))

(define multiply
  (lambda (l n)
    (cond ((= n 0) '())
          (else (cons l (multiply l (- n 1)))))))

; filter function
(define filter
  (lambda (p xs)
    (cond ((null? xs) '())
          ((p (car xs)) (after-filter p (cdr xs)))
          (else (cons (car xs) (after-filter p (cdr xs))))
    )))

(define after-filter
  (lambda (p xs)
    (cond ((null? xs) '())
          ((not (p (car xs))) (after-filter p (cdr xs)))
          (else (p (car xs)) (cons (cadr xs) (rest-list p (cddr xs))))
          )))

(define rest-list
  (lambda (p xs)
    (cond ((null? xs) '())
          ((p (car xs)) (rest-list p (cdr xs)))
          (else (cons (car xs) (rest-list p (cdr xs)))))))


(define add-numbers
  (lambda (xs)
    (if (not (pair? xs))
      xs
      (apply + (number-list (flatten xs))))))

(define number-list
  (lambda (xs)
    (cond ((null? xs) '())
          ((not (number? (car xs))) (number-list (cdr xs)))
          (else (number? (car xs)) (cons (car xs) (number-list (cdr xs)))))))


(define deep-fetch
  (lambda (p xs)
    (if (list? xs)
      (filt p (flatten xs))
      '())))
(define filt
  (lambda (p xs)
    (cond ((null? xs) '())
          ((not (p (car xs))) (filt p (cdr xs)))
          (else (cons (car xs) (filt p (cdr xs))))

    )))





(define hypo (lambda (x y) (sqrt (+ (expt x 2) (expt y 2)))))


(define hypot (λ (x y) (sqrt (+ (expt x 2) (expt y 2)))))

(define factorial
  (lambda (n)
    (cond ((= n 0) 1)
          (else (* n (factorial (- n 1)))))))

(define le
  (lambda (x)
    (cond ((number? x) x)
          ((eq? (car x) '+)
            (+ (le (cadr x)) (le (caddr x))))
          ((eq? (car x) '*)
            (* (le (cadr x)) (le (caddr x))))
          ((eq? (car x) '/)
            (/ (le (cadr x)) (le (caddr x))))
          ((eq? (car x) '-)
            (- (le (cadr x)) (le (caddr x))))
          (error "Bad LE" x))))

; > (map + '(1 2 3 4) '(5 6 7 8))
; (6 8 10 12)
