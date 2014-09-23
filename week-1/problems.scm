,open srfi-78


;;; Problem 2

;;; Write a procedure squares that takes a sentence of numbers as its
;;; argument and returns a sentence of the squares of the numbers.

(define (square number)
  (* number number))

(check (square 5) => 25)


(define (map fn list)
  (if (null? list)
      '()
      (cons (fn (car list))
	    (map fn (cdr list)))))

(check (map odd? '(3 4)) => '(#t #f))


(define (squares number-seq)
  (map square number-seq))

(check (squares '(2 3 4 5)) => '(4 9 16 25))


;;; Problem 3

;;; Write a procedure switch that takes a sentence as its argument and
;;; returns a sentence in which every instance of the words I or me is
;;; replaced by you, while every instance of you is replaced by me
;;; except at the beginning of the sentence, where it’s replaced by I.
;;; (Don't worry about capitalization of letters.)

(define (fold fn acc list)
  (if (null? list)
      acc
      (fold
       fn
       (fn (car list) acc)
       (cdr list))))

(check (fold cons '() '(1 2 3)) => '(3 2 1))


(define (switch-word word is-first?)
  (cond ((eq? word 'you)
	 (if is-first? 'I 'me))

	((or (eq? word 'I) (eq? word 'me))
	 'you)

	(else word)))

(check (switch-word 'foo #f) => 'foo)
(check (switch-word 'foo #t) => 'foo)
(check (switch-word 'I #t) => 'you)
(check (switch-word 'I #f) => 'you)
(check (switch-word 'me #t) => 'you)
(check (switch-word 'me #f) => 'you)
(check (switch-word 'you #t) => 'I)
(check (switch-word 'you #f) => 'me)


(define (switch sentence)

  (define (cons-switched word acc)
    (cons (switch-word word (null? acc))
	  acc))

  (reverse (fold cons-switched '() sentence)))

(check
 (switch '(You told me that I should wake you up))
 => '(i told you that you should wake me up))


;;; Problem 4

;;; Write a predicate ordered? that takes a sentence of numbers as its
;;; argument and returns a true value if the numbers are in ascending
;;; order, or a false value otherwise.

(define (ordered? numbers)
  (or (null? numbers)
      (let loop ((head (car numbers))
		 (tail (cdr numbers)))
	(cond ((null? tail)
	       #t)

	      ((>= head (car tail))
	       #f)

	      (else
	       (loop (car tail) (cdr tail)))))))

(check (ordered? '()) => #t)
(check (ordered? '(1)) => #t)
(check (ordered? '(1 2 3 4)) => #t)
(check (ordered? '(1 3 2 4)) => #f)


;;; Problem 5

;;; Write a procedure ends-e that takes a sentence as its argument and
;;; returns a sentence containing only those words of the argument
;;; whose last letter is E.

(define (last list)
  (cond ((null? list)
	 '())

	((null? (cdr list))
	 (car list))

	(else
	 (last (cdr list)))))

(check (last '()) => '())
(check (last '(1)) => 1)
(check (last '(1 2)) => 2)


(define (last-letter word)
  (last (string->list (symbol->string word))))

(check (last-letter 'flux) => #\x)


(define (endswith? word match)
  (char=? (last-letter word) match))

(check (endswith? 'foo #\e) => #f)
(check (endswith? 'foo #\o) => #t)


(define (filter pred? list)

  (define (cons-matching item acc)
    (if (pred? item)
	(cons item acc)
	acc))

  (reverse (fold cons-matching '() list)))

(check (filter odd? '(1 2 3 4)) => '(1 3))


(define (ends-e sentence)

  (define (endswith-e? word)
    (endswith? word #\e))

  (filter endswith-e? sentence))

(check
 (ends-e '(please put the salami above the blue elephant))
 => '(please the above the blue))


;;; Problem 6

;;; Your mission is to devise a test that will tell you whether
;;; Scheme's and and or are special forms or ordinary functions. This
;;; is a somewhat tricky problem, but it'll get you thinking about the
;;; evaluation process more deeply than you otherwise might.

(define (is-OR-a-special-form?)
  (let ((sentinal #f))

    (define (should-not-evaluate)
      (set! sentinal #t))

    (and
     (or #t (should-not-evaluate))
     (not sentinal))))

(check (is-OR-a-special-form?) => #t)

;;; Question: Why might it be advantageous for an interpreter to treat
;;; or as a special form and evaluate its arguments one at a time? Can
;;; you think of reasons why it might be advantageous to treat or as
;;; an ordinary function?

;; Answer: It's advantageous for an interpreter to treat OR as a
;; special form because it allows short-circuit
;; evaluation. Programmers are encouraged to rely on a single OR
;; statement to do the least amount of work possible when evalutating
;; its outcome. This is especially important when evaluating an
;; argument to OR produces a side-effect or is very expensive.
