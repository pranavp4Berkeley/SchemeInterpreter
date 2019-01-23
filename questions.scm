(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (define (helper lst) (cons first lst)
  )
  (map helper rests))

(define (zip pairs)
  'replace-this-line)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper_func s number)
  (if (null? s)
   s
   (cons (cons number (cons (car s) ())) (helper_func (cdr s) (+ number 1))
    )
   )
  )
   (helper_func s 0)
  )
  
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
  ((null? denoms) ()
  )
  ((= total 0) 
  (cons () ())
  ) 
  ((> (car denoms) total) (list-change total (cdr denoms)))
  (else
   (define first_recursive_case 
    
    (list-change (- total (car denoms)) denoms))
   (define second_recursive_case (list-change total (cdr denoms)
    )
   )
   (append (cons-all (car denoms) first_recursive_case) second_recursive_case)
   )
  )
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons form 
            (cons params 
              (map let-to-lambda body)
              )
            )
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (append 
            (cons (cons 
              'lambda (cons 
                (car (list (map car values) (map cadr values))
                  ) 
                (map let-to-lambda body)
                )) ()) 
            (map let-to-lambda (cadr (list (map car values) (map cadr values))
              )
            )
            )
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (cons 
          (car expr) 
          (map let-to-lambda 
            (cdr expr)
            )
          )
         ; END PROBLEM 19
         )))
