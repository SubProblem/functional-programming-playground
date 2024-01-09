
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; add your funciton implementations below

;; 1) TODO: sequence
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; 2) TODO: string-append-map
(define (string-append-map xs suffix)
  (map (lambda(i) (string-append i suffix)) xs))


;; 3) TODO: string-append-map

(define (sum-square-diff n)
  (- (squared (calculate2 n)) (calculate1 n)))


(define (calculate1 n [acc 1])
  (if (= n 1)
      acc
      (calculate1 (- n 1) (+ acc (* n n)))))


(define (calculate2 n [acc1 1])
  (if (= n 1)
      acc1
      (calculate2 (- n 1) (+ acc1 n))))


(define (squared n)
  (if (equal? n 0)
      0
      (* n n)))


;; 4) TODO: string-append-map
(define (count-nucleotides dna)
  (test (string->list dna)))


(define (test str [a 0] [c 0] [g 0] [t 0])
  (cond
    [(null? str) (values a c g t)]
    [(equal? (car str) #\A) (test (cdr str) (+ a 1) (+ c 0 ) (+ g 0) (+ t 0))]
    [(equal? (car str) #\C) (test (cdr str) (+ a 0) (+ c 1 ) (+ g 0) (+ t 0))]
    [(equal? (car str) #\G) (test (cdr str) (+ a 0) (+ c 0 ) (+ g 1) (+ t 0))]
    [(equal? (car str) #\T) (test (cdr str) (+ a 0) (+ c 0 ) (+ g 0) (+ t 1))]
    ))



;; 5) TODO: string-append-map 
(define (dna-to-rna dna)
  (if (null? dna)
      null
      (cons (convert_function (car dna)) (dna-to-rna (cdr dna)))))
                      
             
(define (convert_function element)
  (if (equal? element #\T)
      #\U
      element))

(define (conversion dna1)
  (list->string(dna-to-rna (string->list dna1))))






