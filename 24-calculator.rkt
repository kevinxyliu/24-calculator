#lang typed/racket

(define-struct Pair
  ([value : Exact-Rational]
   [exp : String]))

(: operation-pair : Pair (Listof Pair) -> (Listof Pair))
;; given a pair and a list of pairs, return the list of
;; pairs after every possible calculations between the pair
;; and the list.
(define (operation-pair ab xs)
  (match* (ab xs)
    [((Pair a d) (cons (Pair b c) xr))
     (append
      (cond
        [(= 0 a b) (list
                    (Pair 0 (string-append "(" c "+" d ")"))
                    (Pair 0 (string-append "(" c "-" d ")"))
                    (Pair 0 (string-append "(" c "*" d ")")))]
        [(= a 0) (list
                  (Pair b (string-append "(" c "+" d ")"))
                  (Pair b (string-append "(" d "-" c ")"))
                  (Pair 0 (string-append "(" c "*" d ")"))
                  (Pair 0 (string-append "(" c "/" d ")")))]
        [(= b 0) (list
                  (Pair a (string-append "(" d "+" c ")"))
                  (Pair a (string-append "(" d "-" c ")"))
                  (Pair 0 (string-append "(" d "*" c ")"))
                  (Pair 0 (string-append "(" c "/" d ")")))]
        [else (list
               (Pair (+ a b) (string-append "(" d "+" c ")"))
               (Pair (- a b) (string-append "(" d "-" c ")"))
               (Pair (- b a) (string-append "(" c "-" d ")"))
               (Pair (* a b) (string-append "(" d "*" c ")"))
               (Pair (/ a b) (string-append "(" d "/" c ")"))
               (Pair (/ b a) (string-append "(" c "/" d ")")))])
      (operation-pair ab xr))]
    [(_ _) '()]))

(: operation-value : Exact-Rational (Listof Pair) -> (Listof Pair))
;; given a value and a list of pairs, return the list of
;; pairs after every possible calculations between the value
;; and the list.
(define (operation-value a xs)
  (operation-pair (Pair a (number->string a)) xs))

(: operation-list : (Listof Pair) (Listof Pair) -> (Listof Pair))
;; given two lists of pairs, return the list of
;; pairs after every possible calculations between the
;; two lists
(define (operation-list xs1 xs2)
  (match xs1
    [(cons (Pair x y) xr) (append (operation-pair (Pair x y) xs2)
                                  (operation-list xr xs2))]
    ['() '()]))

(: result-two : Exact-Rational Exact-Rational -> (Listof Pair))
;; given two values, return the list of pairs after
;; any possible calculations between the two values
(define (result-two a b)
  (operation-value a (list (Pair b (number->string b)))))

(: result-three : Exact-Rational Exact-Rational Exact-Rational -> (Listof Pair))
;; given three values, return the list of pairs after
;; any possible calculations between the three values
(define (result-three a b c)
  (append (operation-value a (result-two b c))
          (operation-value b (result-two a c))
          (operation-value c (result-two a b))))

(: result-four : Exact-Rational Exact-Rational Exact-Rational Exact-Rational -> (Listof Pair))
;; given four values, return the list of pairs after
;; any possible calculations between the four values
(define (result-four a b c d)
  (append (operation-value a (result-three b c d))
          (operation-value b (result-three a c d))
          (operation-value c (result-three a b d))
          (operation-value d (result-three a b c))
          (operation-list (result-two a b) (result-two c d))
          (operation-list (result-two a c) (result-two b d))
          (operation-list (result-two a d) (result-two b c))))

(: print : Exact-Rational Exact-Rational Exact-Rational Exact-Rational -> Void)
;; return the list of strings representing all the ways to calculate 24
(define (print a b c d)
  (local
    {(: check-list : String (Listof String) -> Boolean)
     ;; a helper function to check if a string already appears on a list of strings
     (define (check-list str xs)
       (ormap (lambda ([str1 : String]) (string=? str1 (string-append str "=24"))) xs))
     (: print-aux : (Listof Pair) (Listof String) -> (Listof String))
     ;; use an accumulator to track the strings already included in the return list
     (define (print-aux xs acc)
       (match xs
         [(cons (Pair 24 str) xr)
          (if (check-list str acc) ;; eliminate a string if it already appears in the return list
              (print-aux xr acc)   ;; so that we do not get the same expressions multiple times
              (print-aux xr (cons (string-append str "=24") acc)))]
         [(cons (Pair _ str) xr) (print-aux xr acc)]
         ['() acc]))
     (: print-all : (Listof String) -> Void)
     (define (print-all xs)
       (match xs
         [(cons x xr)
          (begin
            (display x)
            (newline)
            (print-all xr))]
         ['() (void)]))
     (define result : (Listof String) (print-aux (result-four a b c d) '()))
     (define len : Integer (length result))}
    (begin
      (display "There ")
      (if (= len 1) (display "is ") (display "are "))
      (display (number->string len))
      (if (= len 1) (display " way ") (display " ways "))
      (display "to calculate 24 from ")
      (display (number->string a))
      (display " ")
      (display (number->string b))
      (display " ")
      (display (number->string c))
      (display " ")
      (display (number->string d))
      (display ":")
      (newline)
      (print-all result)
      (newline))))

(print 10 10 4 4)
(print 6 6 6 6)
(print 5 5 5 1)
(print 6 8 8 9)
(print 1 4 5 6)
(print 20 17 8 8)
(print 2 5 7 9)
(print 3 3 3 24)
