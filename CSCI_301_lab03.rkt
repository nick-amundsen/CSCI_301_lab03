#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab #3
;;
;; Nick Amundsen
;; W01323151
;;
;; The purpose of this program is to
;; find the power set of a list using
;; recursion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide contains)
(provide distribute)
(provide sublists)
(provide length-ordered?)
(provide subsets)
(provide element-ordered?)





;function to see if a list contains a value
(define contains
  (lambda (n list)
    (cond ((null? list) #f)
          ((equal? (first list) n) #t)
          (else (contains n (cdr list)))
          )))

;function to distribute a value to each sub list, stops when
;it reaches a list that already contains that value
(define distribute
  (lambda (n list)
    (cond ((null? list) list)
          ((contains n (car list)) list)
          (else (cons (append (cons n '()) (car list)) (distribute n (cdr list))))
          )))

;function for finding all sublists of a list
(define sub
  (lambda (list)
    (cond ((null? list) (append list '()))
          (else (let ([sublist (sub (cdr list))])
                 (append sublist (distribute (first list) (append '(()) sublist )))
                  )) 
          )))

;function for finding all sublists of a list, including '()
(define sublists
  (lambda (list)
    (cons '() (sub list))))


;function for ordering sets by elements
(define element-ordered?
  (lambda (list list2)
    (cond ((equal? list '(())) #t)
          ((< (car list) (car list2)) #t)
          ((equal? (car (append list '())) (car (append list2 '()))) (element-ordered? (cdr list) (cdr list2)))
          (else #f))))

;function for ordering sets by length
(define length-ordered?
  (lambda (list list2)
    (cond ((null? (cons list '())) #t)
          ((< (length (append list '())) (length (append list2 '()))) #t)
          ((= (length list) (length list2)) (element-ordered? list list2))
          (else #f))))

;function for sorting the lists into organized sets
(define sorting
  (lambda (list)
    (cond ((null? (cdr list)) list)
          ((length-ordered? (car list) (car (cdr list))) (sorting (cons (car list) (cdr(cdr list)))))         
          (else (sorting (cdr list))))))

;function that outputs sorted subsets
(define subset-input
  (lambda (list) (let ((val (sorting list)))
    (cond ((null? (cdr list)) val)
          (else (cons (car val) (subset-input (remove (car val) list)))))
          )))

;main subset function
(define subsets
  (lambda (list) (let ((s-list (sublists list)))
                   (subset-input s-list))))

