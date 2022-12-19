#lang racket

;get element function
(define (get-element list n)
  (cond
    ((equal? n 0) (car list))
    (else (get-element (cdr list) (- n 1)))))


;tests for get-element
(display "(get-element '(1 2 3 5 7 9 4) 5) -> 9: ")
(get-element '(1 2 3 5 7 9 4) 5)

(display "(get-element '(1 2 3 5 7 9 4) 0) -> 1: ")
(get-element '(1 2 3 5 7 9 4) 0)

(display "(get-element '(1 2 3 5 7 9 4) 6) -> 4: ")
(get-element '(1 2 3 5 7 9 4) 6)

(display "(get-element '(1 2 3 5 7 9 4) 7 -> error: ")
;(get-element '(1 2 3 5 7 9 4) 7)



;append element function
(define (append-element list y)
  (cond
    ((null? list) (cons y list))
    (else (cons (car list) (append-element (cdr list) y)))))


;tests for append-element
(display "(append-element '(1 2 3 5 7 9 4) 5) -> '(1 2 3 5 7 9 4 5): ")
(append-element '(1 2 3 5 7 9 4) 5)

(display "(append-element '() 0) -> '(0): ")
(append-element '() 0)



;append list function
(define (append-list first second)
  (cond
    ((null? second) (cons (car first) (cdr first)))
    ((null? first) (cons (car second) (append-list (cdr second) first)))
    (else (cons (car first) (append-list (cdr first) second)))))


;tests for append-list
(display "(append-list '(1 3 5) '(4 6 8)) -> '(1 3 5 4 6 8): ")
(append-list '(1 3 5) '(4 6 8))

(display "(append-list '(1 2 3) '()) -> '(1 2 3): ")
(append-list '(1 2 3) '())

(display "(append-list '() '(2 4 6)) -> '(2 4 6): ")
(append-list '() '(2 4 6))



;backwards list function
(define (backwards list)
  (cond (null? list)
      (cons (backwards (cdr list)) (cons (car list) null))))


;tests for backwards
(display "(backwards '(1 2 3 4 5)) -> '(5 4 3 2 1)")
(backwards '(1 2 3 4 5))



;paren checker
(define (paren-balanced? str (stack '()))
  (define (str-list str)
   (cond
     ((string? str) (string->list str))
     (else str)))
  
  (define (opening-character? str)
     (cond
      ((null? str) #f)
      ((equal? (car (str-list str)) #\() #t)
      ((equal? (car (str-list str)) #\[) #t)
      ((equal? (car (str-list str)) #\{) #t)
      (else #f)))
  
  (define (closing-character? str)
   (cond
    ((null? str) #f)
    ((equal? (car (str-list str)) #\)) #t)
    ((equal? (car (str-list str)) #\]) #t)
    ((equal? (car (str-list str)) #\}) #t)
    (else #f)))

  (define (closed-to-open closed)
    (cond
     ((equal? #\) closed) #\()
     ((equal? #\] closed) #\[)
     ((equal? #\} closed) #\{)))
  
  (cond
    ((null? str)
     (cond
       ((null? stack) #t)
       (else #f)))
    ((opening-character? str) (paren-balanced? (cdr (str-list str)) (cons (car (str-list str)) stack)))
    ((closing-character? str)
     (cond
       ((null? stack) #f)
       ((equal? (closed-to-open (car (str-list str))) (car stack)) (paren-balanced? (cdr (str-list str)) (cdr stack)))
       (else #f)))
    (else (paren-balanced? (cdr (str-list str)) stack))))


;Tests for paren-balanced
(display "(paren-balanced? '()') -> #t")
(paren-balanced? "()")

(display "(paren-balanced? '(hi)') -> #t")
(paren-balanced? "(hi)")

(display "(paren-balanced? '(hi))') -> #f")
(paren-balanced? "(hi))")

(display "(paren-balanced? '((hi)') -> #f")
(paren-balanced? "((hi)")

(display "(paren-balanced? '[hi)') -> #f")
(paren-balanced? "[hi)")

(display "(paren-balanced? '(hi}') -> #f")
(paren-balanced? "(hi}")

(display "(paren-balanced? '[(hi)] {}') -> #t")
(paren-balanced? "[(hi)] {}")

(display "(paren-balanced? ')') -> #f")
(paren-balanced? ")")
    
(display "(paren-balanced? '(') -> #f")
(paren-balanced? "(")
     
  

  


      