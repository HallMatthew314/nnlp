# lang racket

; P01: Find the last box of a list.
(define (my-last items)
  (if (null? (cdr items))
    (car items)
    (my-last (cdr items))))

; P02: Find the last but one box of a list.
(define (my-but-last items)
  (#f))

; P03: Find the K'th element of a list. (First element is index 1.)
(define (element-at items k)
  (#f))

; P04: Find the number of elements in a list.
(define (my-length items)
  (apply + (map (const 1) items)))

; P05: Reverse a list.
(define (my-reverse items)
  (foldl cons '() items))
