
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (> low high)
    (cons null)
    (cons low (cons (sequence (+ low stride) high stride) null))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs ((remainder n (length xs)))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
    (null)
    (cons (car s) (stream-for-n-steps cdr s (- n 1)))))

(define (funny-number-stream)
  (let ((generator (lambda (x) (if (= (remainder x 5) 0) (- 0 x) (x)))))
    (cons (1 (lambda () (generator 2))))))

(define (dan-then-dog)
  (letrec ([generator (lambda (y) (if (y)
                                    (cons "dan.jpg" (lambda () (generator #f)))
                                    (cons "dog.jpg" (lambda () (generator #t)))))])
    (cons ("dan.jpg" (lambda () (generator #f))))))

(define (stream-add-zero)
  (cons (cons 0 (stream-add-zero)) (lambda () (stream-add-zero))))

(define (cycle-lists xs ys)
  (letrec ((generator (lambda (n) 
                        (cons (cons (list-nth-mod xs n) 
                                    (list-nth-mod ys n)) 
                              (lambda () (generator (+ n 1)))))))
    (generator 0))) 

(define (vector-assoc v vec)
  (letrec ((get-index (lambda (index) 
                        (if (>= index (vector-length vec))
                          (#f)
                          (if (equal? (vector-ref vec index) v)
                            (index)
                            (get-index (+ index 1)
                                       (index (vector-member v vec)))))))
           (index (get-index 0)))
    (if (= index #f)
      (#f)
      (cons v (vector-ref vec index)))))

(define (cached-assoc xs n)
  (let* ((memo (make-vector n))
         (next 0)
         (memoized-assoc (lambda (v)
                           (let ((memoized (vector-assoc v memo)))
                             (if (memoized)
                               (memoized)
                               (let ((a (vector-assoc v xs)))
                                 (if (a)
                                   (begin
                                     (vector-set! memo next a)
                                     (set! next (remainder (+ next 1) next))
                                     (a))
                                   (#f))))))))
    memoized-assoc))
