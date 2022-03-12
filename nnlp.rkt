#! /usr/bin/env racket
#lang racket

; TODO: variadics
(define (test-function expected f args)
  (let ([result (apply f args)])
    (if (equal? expected result)
      "OK"
      (string-append "Failed: expected " (~a expected) " but got " (~a result)))))

; P01: Find the last box of a list.
(define (my-last items)
  (if (null? (cdr items))
    items
    (my-last (cdr items))))

; P02: Find the last but one box of a list.
(define (my-but-last items)
  (if (null? (cdr (cdr items)))
    items
    (my-but-last (cdr items))))

; P03: Find the K'th element of a list. (First element is index 1.)
(define (element-at items k)
  (if (equal? 1 k)
    (car items)
    (element-at (cdr items) (sub1 k))))

; P04: Find the number of elements in a list.
(define (my-length items)
  (apply + (map (const 1) items)))

; P05: Reverse a list.
(define (my-reverse items)
  (foldl cons '() items))

; P06: Find out whether a list is a palondrome
(define (is-palindrome items)
  (cond
   [(< (length items) 2) #t]
   [(not (equal? (car items) (last items))) #f]
   [else (is-palindrome (drop-right (cdr items) 1))]))

(let ([functions-list (list
        my-last
        my-but-last
        element-at
        my-length
        my-reverse
        is-palindrome)]
      [expected-list (list
        (list 5)
        (list 4 5)
        3
        5
        (list 5 4 3 2 1)
        #t)]
      [args-list (list
        (list (list 1 2 3 4 5))
        (list (list 1 2 3 4 5))
        (list (list 1 2 3 4 5) 3)
        (list (list 1 2 3 4 5))
        (list (list 1 2 3 4 5))
        (list (list 1 2 3 2 1)))])
  (map
    (lambda (e f a)
      (writeln (test-function e f a)))
    expected-list functions-list args-list))

