,open srfi-78


;;; Problem 1.31(a)

;;; The sum procedure is only the simplest of a vast number of similar
;;; abstractions that can be captured as higher-order procedures.
;;; Write an analogous procedure called product that returns the
;;; product of the values of a function at points over a given
;;; range.

(define (product term begin next end)
  (if (> begin end)
      1
      (* (term begin)
	 (product term (next begin) next end))))

(check (product (lambda (x) x) 3 (lambda (x) (+ x 1)) 5) => 60)

;;; Show how to define factorial in terms of product.

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial number)
  (product identity 1 inc number))

(check (factorial 5) => 120)

;;; Also use product to compute approximations to PI.

;; See also: http://en.wikipedia.org/wiki/Wallis_product

(define (wallace-term x)
  (let ((double (* x 2.0)))
    (* (/ double (- double 1.0))
       (/ double (+ double 1.0)))))

(define (wallace-product limit)
  (product wallace-term 1.0 inc limit))

(define (compute-pi limit)
  (* 2.0 (wallace-product limit)))

(define (within-tolerance? value target tol)
  (< (abs (- value target)) tol))

(check (within-tolerance? (compute-pi 1000) 3.14 .001) => #t)


;;; Problem 1.31(a)

;;; Show that sum and product (exercise 1.31) are both special cases
;;; of a still more general notion called accumulate that combines a
;;; collection of terms, using some general accumulation function:
;;;
;;;    (accumulate combiner null-value term a next b)
;;;
;;; Accumulate takes as arguments the same term and range
;;; specifications as sum and product, together with a combiner
;;; procedure (of two arguments) that specifies how the current term
;;; is to be combined with the accumulation of the preceding terms and
;;; a null-value that specifies what base value to use when the terms
;;; run out. Write accumulate and show how sum and product can both be
;;; defined as simple calls to accumulate.

(define (accumulate combiner null-value term begin next end)
  (if (> begin end)
      null-value
      (combiner
       (term begin)
       (accumulate combiner null-value term (next begin) next end))))

(define (acc-sum term begin next end)
  (accumulate + 0 term begin next end))

(check (acc-sum identity 1 inc 10) => 55)

(define (acc-product term begin next end)
  (accumulate * 1 term begin next end))

(check (acc-product identity 1 inc 5) => (product identity 1 inc 5))


;;; Problem 1.33

;;; You can obtain an even more general version of accumulate
;;; (exercise 1.32) by introducing the notion of a filter on the terms
;;; to be combined. That is, combine only those terms derived from
;;; values in the range that satisfy a specified condition. The
;;; resulting filtered-accumulate abstraction takes the same arguments
;;; as accumulate, together with an additional predicate of one
;;; argument that specifies the filter. Write filtered-accumulate as a
;;; procedure. Show how to express the following using
;;; filtered-accumulate:

(define (filtered-accumulate combiner null-value filter? term begin next end)

  (define (iterate)
    (filtered-accumulate combiner null-value filter? term (next begin) next end))

  (cond ((> begin end)
	 null-value)

	((not (filter? begin))
	 (iterate))

	(else
	 (combiner (term begin) (iterate)))))

;;; a. the sum of the squares of the prime numbers in the interval a
;;; to b (assuming that you have a prime? predicate already written)

(define (factor? number divisor)
  (= 0 (modulo number divisor)))

(check (factor? 10 5) => #t)
(check (factor? 10 6) => #f)

(define (prime? number)
  (and (> number 1)
       ;; This is a strange way to loop over numbers in the range from
       ;; 2 to the square root of NUMBER and test that all are
       ;; prime. Scheme doesn't have any kind of built in "range" or
       ;; "sequence" operations that make sense, so I re-used
       ;; ACCUMULATE. Not the easiest thing to read, though...
       (accumulate
	(lambda (x y) (and x y))
	#t
	(lambda (n) (not (factor? number n)))
	2
	inc
	(floor (sqrt number)))))

(check (prime? 9) => #f)
(check (prime? 16) => #f)
(check (prime? 17) => #t)

(define (align-odd number)
  (cond ((< number 3) 3)
	((even? number) (inc number))
	(else number)))

(check (align-odd 1) => 3)
(check (align-odd 4) => 5)
(check (align-odd 5) => 5)

(define (next-odd odd)
  (+ odd 2))

(check (next-odd 3) => 5)

(define (sum-primes begin end)
  (filtered-accumulate + 0 prime? identity (align-odd begin) next-odd end))

(check (sum-primes 3 5) => 8)
(check (sum-primes 3 7) => 15)
(check (sum-primes 3 9) => 15)
(check (sum-primes 3 11) => 26)

;;; b. the product of all the positive integers less than n that are
;;; relatively prime to n (i.e., all positive integers i < n such that
;;; GCD(i,n) = 1).

(define (rel-prime? a b)
  (= 1 (gcd a b)))

(check (rel-prime? 5 6) => #t)
(check (rel-prime? 3 6) => #f)

(define (product-relative-primes less-than-number)
  (filtered-accumulate
   * 1
   (lambda (n) (rel-prime? n less-than-number))
   identity 2
   inc (- less-than-number 1)))

(check (product-relative-primes 3) => 2)
(check (product-relative-primes 4) => 3)
(check (product-relative-primes 5) => 24)
