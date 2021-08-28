;Bengisu Takkin
;2018400036
;compiling: yes
;complete: yes

#lang racket
(provide (all-defined-out))

(struct num (value grad)
    #:property prop:custom-write
   (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))

(define (get-value nums)
  (cond [(null? nums) '()]
        [(list? nums) (cons (cadr(car nums)) (get-value (cdr nums)))]  ;3.1
        [else (num-value nums)]))

(define (get-grad nums)
  (cond [(null? nums) '()]
        [(list? nums) (cons (caddr(car nums)) (get-grad (cdr nums)))]  ;3.1
        [else (num-grad nums)]))  ;3.2

(define (getAddGrad . args)(if (null? args) 0 (+ (num-grad (car args)) (apply getAddGrad (cdr args)))))  ;4.1
(define (getAddValue . args)(if (null? args) 0 (+ (num-value (car args)) (apply getAddValue (cdr args)))))
(define (add . args) (eval (list 'num (apply getAddValue args)  (apply getAddGrad args))))

(define (getMulValue . args)(if (null? args) 1 (* (num-value (car args)) (apply getMulValue (cdr args)))))  ;4.2
(define (getDerv all) (if (null? all) 1 (* (num-value (car all)) (getDerv (cdr all))))) 
(define (getMulGrad args all) (if (null? args) 0 (+ (* (num-grad (car args)) (getDerv (remove (car args) all))) (getMulGrad (cdr args) all))))
(define (mul . args) (eval (list 'num (apply getMulValue args) (getMulGrad args args))))


(define (sub num1 num2) (eval (list 'num (- (num-value num1) (num-value num2)) (- (num-grad num1) (num-grad num2))))) ;4.3

(define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0)))) ;4.4
(define mse (lambda (x y) (mul (sub x y) (sub x y))))


(define (make-pairs var-list val-list var)   ;5.1
  (if (null? var-list) '() (apply list (cons (car var-list) (eval (list 'num (car val-list) (if (eq? (car var-list) var) '1.0 '0.0)))) (make-pairs (cdr var-list) (cdr val-list) var))))
(define (create-hash var-list val-list var) (make-hash (make-pairs var-list val-list var)))


(define (parse hash expr)   ;5.2
  (cond [(null? expr) '() ]
        [(list? expr) (apply list (parse hash (car expr)) (parse hash (cdr expr)))]
        [(eq? expr '+) 'add]  
        [(eq? expr '*) 'mul]
        [(eq? expr '-) 'sub]
        [(eq? expr 'mse) 'mse]
        [(eq? expr 'relu) 'relu]
        [(integer? expr) (eval (list 'num expr 0.0))]
        [ else (hash-ref hash expr)]))

(define (grad names values var expr) (num-grad (eval (parse (create-hash names values var) expr))))  ;5.3

(define (helper times names values vars expr) (if (null? times) '() (cons (grad names values (if(memq (car times) vars) (car times) '()) expr) (helper (cdr times) names values vars expr))))          
(define (partial-grad  names values vars expr) (helper names names values vars expr))   ;5.4


(define (descent values lr part-grad) (if (null? values) '() (cons (-(car values) (* lr (car part-grad))) (descent (cdr values) lr (cdr part-grad)))))          
(define (gradient-descent names values vars lr expr) (descent values lr (partial-grad names values vars expr)))   ;5.5

(define (optimize names values vars lr k expr) (if (eq? k 1) (gradient-descent names values vars lr expr) (gradient-descent names (optimize names values vars lr (- k 1) expr) vars lr expr))) ;5.6




