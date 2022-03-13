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

; P07: Flatten a nested list structure.
(define (my-flatten items)
  (apply append
    (map
      (lambda (x) (if (list? x) (my-flatten x) (list x)))
      items)))

; P08: Eliminate consecutive duplicates of list elements.
(define (compress items)
  (foldr
    (lambda (x xs)
      (if (or (null? xs) (not (equal? x (car xs)))) (cons x xs) xs))
    '() items))

; P09: Pack consecutive duplicates of list elements into sublists.
(define (pack items)
    (if (null? items) '()
      (let ([x (car items)]
            [rest (pack (cdr items))])
        (cond
          [(null? rest)
            (list (list x))]
          [(equal? x (car (car rest)))
            (cons (cons x (car rest)) (cdr rest))]
          [else
            (cons (list x) rest)]))))

; P10: Run-length encoding of a list.
(define (encode items)
  (map (lambda (xs) (list (my-length xs) (car xs))) (pack items)))

; P11: Modified run-length encoding.
(define (encode-modified items)
  (map (lambda (xs) (if (equal? 1 (first xs)) (second xs) xs)) (encode items)))

; P12: Decode a run-length encoded list.
(define (decode items)
  (apply append
    (map
      (lambda (x)
        (if (list? x) (build-list (first x) (const (second x))) (list x)))
      items)))

(let ([functions-list (list
        my-last
        my-but-last
        element-at
        my-length
        my-reverse
        is-palindrome
        my-flatten
        compress
        pack
        encode
        encode-modified
        decode)]
      [expected-list (list
        (list 5)
        (list 4 5)
        3
        5
        (list 5 4 3 2 1)
        #t
        (list 1 2 3 4 5)
        (list 1 2 3 1 4 5)
        (list (list 1 1 1 1) (list 2) (list 3 3) (list 1 1) (list 4) (list 5 5 5 5))
        (list (list 4 1) (list 1 2) (list 2 3) (list 2 1) (list 1 4) (list 4 5))
        (list (list 4 1) 2 (list 2 3) (list 2 1) 4 (list 4 5))
        (list 1 1 1 1 2 3 3 1 1 4 5 5 5 5))]
      [args-list (list
        (list (list 1 2 3 4 5))
        (list (list 1 2 3 4 5))
        (list (list 1 2 3 4 5) 3)
        (list (list 1 2 3 4 5))
        (list (list 1 2 3 4 5))
        (list (list 1 2 3 2 1))
        (list (list 1 (list 2 (list 3 4) 5)))
        (list (list 1 1 1 1 2 3 3 1 1 4 5 5 5 5))
        (list (list 1 1 1 1 2 3 3 1 1 4 5 5 5 5))
        (list (list 1 1 1 1 2 3 3 1 1 4 5 5 5 5))
        (list (list 1 1 1 1 2 3 3 1 1 4 5 5 5 5))
        (list (list (list 4 1) 2 (list 2 3) (list 2 1) 4 (list 4 5))))])
  (map
    (lambda (e f a)
      (writeln (test-function e f a)))
    expected-list functions-list args-list))

