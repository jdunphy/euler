#! /usr/bin/env racket
#lang racket

; solution 1
(define (mod3 n) (zero? (modulo n 3)))
(define (mod5 n) (zero? (modulo n 5)))

(apply + (remove-duplicates
          (append (filter mod3 (range 1 1000)) (filter mod5 (range 1 1000))))) 


; solution 2
(define (modder mod) (lambda (num) (zero? (modulo num mod))))

(apply + (remove-duplicates
          (append (filter (modder 3) (range 1 1000))
                  (filter (modder 5) (range 1 1000))))) 


