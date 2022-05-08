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

; P13: Run-length encoding of a list (direct solution).
(define (encode-direct items)
  (letrec
    ([inc-pair (lambda (p)
      (cons (add1 (car p)) (cdr p)))]
    [go (lambda (x xs)
      (cond
        [(null? xs) (list x)]
        [(and (list? (car xs)) (equal? x (second (car xs))))
          (cons (inc-pair (car xs)) (cdr xs))]
        [(equal? x (car xs))
          (cons (list 2 x) (cdr xs))]
        [else
          (cons x xs)]))])
    (foldr go '() items)))

; P14: Duplicate the elements of a list.
(define (dupli items)
  (repli items 2))

; P15: Replicate the elements of a list.
(define (repli items n)
  (letrec
    ([n-times (lambda (e i)
      (if (zero? i) '() (cons e (n-times e (sub1 i)))))])
    (append-map (lambda (x) (n-times x n)) items)))

; P16: Drop every n'th element from a list.
(define (drop-every-n items n)
  (letrec
    ([help (lambda (i xs)
      (cond
        [(null? xs) '()]
        [(zero? i) (help (modulo (sub1 i) n) (cdr xs))]
        [else (cons (car xs) (help (sub1 i) (cdr xs)))]))])
    (help (sub1 n) items)))

; P17: Split a list into two parts; the length of the first part is given.
; (No predefined functions allowed).
(define (my-split items n)
  (cond
    [(null? items) (list '() '())]
	[(zero? n) (list '() items)]
	[else
	  (let
	    ([next (my-split (cdr items) (sub1 n))])
		(cons (cons (car items) (car next)) (cdr next)))]))

; TODO: Write test and verify solution
; P18: Extract a slice from a list. (1-indexed)
(define (slice items i k)
  (drop (take items k) (sub1 i)))

; P19: Rotate a list N places to the left.
(define (rotate items n)
  (letrec
    ([rot-left (lambda (xs) (append (cdr xs) (list (car xs))))]
    [rot-right (lambda (xs) (cons (last xs) (drop-right xs 1)))]
	[n-times (lambda (i f x)
	  (if (zero? i) x
	    (n-times (sub1 i) f (f x))))])
    (cond
	  [(or (zero? n) (null? items)) items]
	  [(positive? n) (n-times n rot-left items)]
	  [(negative? n) (n-times (abs n) rot-right items)])))

; P20: Remove the K'th element from a list. (1-indexed)
(define (my-remove-at items k)
  (if (equal? k 1)
    (cdr items)
	(cons (car items) (my-remove-at (cdr items) (sub1 k)))))

; P21: Insert an element at a given position into a list. (1-indexed)
(define (insert-at x items k)
  (if (equal? k 1)
    (cons x items)
	(cons (car items) (insert-at x (cdr items) (sub1 k)))))

; P22: Create a list containing all integers within a given range. (Inclusive)
; If first argument is smaller than second, produce a list in decreasing order.
(define (my-range i j)
  (if (equal? i j) (list i)
    (cons i
	  (my-range (if (< i j) (add1 i) (sub1 i)) j))))

; P23: Extract a given number of randomly selected elements from a list. (No duplicates)
; TESTED MANUALLY
(define (rnd-select items n)
  (if (or (zero? n) (null? items)) '()
    (let
      ([i (random (length items))])
      (cons
        (list-ref items i)
        (rnd-select (my-remove-at items (add1 i)) (sub1 n))))))

; P24: Lotto: Draw N different random numbers from the set 1..M
; TESTED MANUALLY
(define (lotto-select n m)
  (rnd-select (my-range 1 m) n))

; P25: Generate a random permutation of the elements of a list.
; TESTED MANUALLY
(define (rnd-permu items)
  (rnd-select items (length items)))

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
        decode
        encode-direct
        dupli
        repli
        drop-every-n
        my-split
        slice
        rotate ; positive test
        rotate ; negative test
        my-remove-at
        insert-at
        my-range ; ascending and descending tests
        my-range)]
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
        (list 1 1 1 1 2 3 3 1 1 4 5 5 5 5)
        (list (list 4 1) 2 (list 2 3) (list 2 1) 4 (list 4 5))
        (list 1 1 2 2 3 3 3 3 4 4)
        (list 1 1 1 2 2 2 3 3 3)
        (list 0 1 3 4 6 7 9)
        (list (list 0 1 2) (list 3 4 5 6 7 8 9))
        (list 2 3 4 5 6)
        (list 3 4 5 6 7 0 1 2) ; rotate positive
        (list 6 7 0 1 2 3 4 5) ; rotate negative
        (list 0 2 3)
        (list 0 99 1 2 3)
        (list 4 5 6 7 8 9) ; my-range ascending and descending tests
        (list 9 8 7 6 5 4))]
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
        (list (list (list 4 1) 2 (list 2 3) (list 2 1) 4 (list 4 5)))
        (list (list 1 1 1 1 2 3 3 1 1 4 5 5 5 5))
        (list (list 1 2 3 3 4))
        (list (list 1 2 3) 3)
        (list (list 0 1 2 3 4 5 6 7 8 9) 3)
        (list (list 0 1 2 3 4 5 6 7 8 9) 3)
        (list (list 0 1 2 3 4 5 6 7 8 9) 3 7)
        (list (list 0 1 2 3 4 5 6 7) 3) ; rotate positive
        (list (list 0 1 2 3 4 5 6 7) -2) ; rotate negative
        (list (list 0 1 2 3) 2)
        (list 99 (list 0 1 2 3) 2)
        (list 4 9) ; my-range ascending and descending tests
        (list 9 4))])
  (map
    (lambda (e f a)
      (writeln (test-function e f a)))
    expected-list functions-list args-list))

