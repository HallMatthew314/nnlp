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

; P26: Generate the combinations of K distinct objects chosen from the N elements of a list
; TODO

; P27: Group the elements of a set into disjoint subsets.
; TODO

; P28: Sorting a list of lists according to length of sublists
; TODO

; P31: Determine whether a given integer number is prime.
(define (is-prime n)
  (andmap
    (lambda (x) (not (zero? (remainder n x))))
    (my-range 2 (integer-sqrt n))))

; P32: Determine the greatest common divisor of two positive integer numbers.
(define (my-gcd a b)
  (cond
    [(> b a) (my-gcd b a)]
    [(zero? b) a]
    [else (my-gcd b (remainder a b))]))

; P33: Determine whether two positive integer numbers are coprime.
(define (coprime a b)
  (equal? 1 (my-gcd a b)))

; P34: Calculate Euler's totient function phi(m).
(define (totient-phi m)
  (if (equal? 1 m) 1
    (length (filter (lambda (x) (coprime x m)) (my-range 1 (sub1 m))))))

; P35: Determine the prime factors of a given positive integer.
(define (prime-factors n)
  (define (lowest-odd-factor x f)
    (cond
      [(<= x f) x]
      [(zero? (remainder x f)) f]
      [else (lowest-odd-factor x (+ f 2))]))
  (cond
    [(equal? n 1) '()]
    [(even? n) (cons 2 (prime-factors (/ n 2)))]
    [else
      (let
        ([lf (lowest-odd-factor n 3)])
        (cons lf (prime-factors (/ n lf))))]))

; P36: Determine the prime factors of a given positive integer (2). Construct a list containing the prime factors and their multiplicity.
(define (prime-factors-mult n)
  (map
    (lambda (p) (list (second p) (first p)))
    (encode (prime-factors n))))

; P37: Calculate Euler's totient function phi(m) (improved).
(define (totient-phi-improved n)
  (apply *
    (map
      (lambda (pm)
        (match pm [(list p m) (expt (* p (sub1 p)) (sub1 m))]))
      (prime-factors-mult n))))

; P38: Compare the two methods of calculating Euler's totient function.
; TODO

; P39: A list of prime numbers. Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
(define (prime-list start end)
  (define (help i ps)
    (cond
      [(> i end) ps]
      [(andmap (lambda (p) (not (zero? (remainder i p)))) ps) (help (+ 2 i) (cons i ps))]
      [else (help (+ 2 i) ps)]))
  (dropf (cons 2 (reverse (help 3 '()))) (lambda (x) (< x start))))

; P40: Goldbach's conjecture.
(define (goldbach n)
  (let
    ([primes (prime-list 2 n)])
  (let
    ([p (first (filter
                 (lambda (x) (member (- n x) primes))
                 primes))])
    (list p (- n p)))))

; P41: A list of Goldbach compositions.
(define (goldbach-list lower upper [print-limit 1])
  (define (fix-start s)
    (cond
      [(< s 4) 4]
      [(odd? s) (add1 s)]
      [else s]))
  (map
    (lambda (ps)
      (string-append
        (~a (apply + ps))
        " = "
        (~a (first ps))
        " + "
        (~a (second ps))))
    (filter
      (lambda (x) (andmap (lambda (i) (>= i print-limit)) x))
      (map goldbach (range (fix-start lower) upper 2)))))

; P46: Truth tables for logical expressions.
; UNTESTED
(define (my-and x y)
  (if x y #f))

(define (my-or x y)
  (if x #t y))

(define (my-nand x y)
  (not (my-and x y)))

(define (my-nor x y)
  (not (my-or x y)))

(define (my-equ x y)
  (equal? x y))

(define (my-xor x y)
  (not (my-equ x y)))

(define (my-impl x y)
  (if x y #t))

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
        my-range ; ascending test
        my-range ; descending test
        ; P23-P25 are random, omitted from tests
        ; skipping P26-30 for now
        is-prime
        my-gcd
        coprime
        totient-phi
        prime-factors
        prime-factors-mult
        ; P38 not testable
        prime-list
        goldbach
        goldbach-list)]
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
        (list 4 5 6 7 8 9) ; my-range ascending test
        (list 9 8 7 6 5 4) ; my-range descending test
        ; P23-P25 are random, omitted from tests
        ; skipping P26-30 for now
        #t
        9
        #t
        4
        (list 3 3 5 7)
        (list (list 3 2) (list 5 1) (list 7 1))
        4
        ; P38 not testable
        (list 5 7 11 13 17 19)
        (list 5 23)
        (list "992 = 73 + 919" "1382 = 61 + 1321" "1856 = 67 + 1789" "1928 = 61 + 1867"))]
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
        (list 4 9) ; my-range ascending test
        (list 9 4) ; my-range descending test
        ; P23-P25 are random, omitted from tests
        ; skipping P26-30 for now
        (list 7)
        (list 36 63)
        (list 35 64)
        (list 10)
        (list 315)
        (list 315)
        (list 10)
        ; P38 not testable
        (list 4 20)
        (list 28)
        (list 1 2000 50))])
  (map
    (lambda (e f a)
      (writeln (test-function e f a)))
    expected-list functions-list args-list))

