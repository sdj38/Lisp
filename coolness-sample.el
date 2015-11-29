
* Coolness Sample Code

  Use the following code block as a guide and a starting point for
  your exploration:

#+BEGIN_SRC emacs-lisp :tangle yes
(require 'cl) ; for loop macro

(require 'ment) ; for true and false

(defun for-all-for-all (pred x y)
  (block 'outer-for-all-for-all
    (loop for itemX across x
	 do (loop for itemY across y
		do(if (null (funcall pred itemX itemY))
		      (return-from 'outer-for-all-for-all nil))))
    t))
(for-all-for-all '< [1 2 3] [4 5 6 2])

(defun for-all (pred domain)
  "for-all is the Universal Quantification of a predicate (pred),
   the proposition that is true if and only if pred is true for all
   items in a universe of discourse (domain).
   This function loops across domain (which has finite size)
   to see if pred is always true.
   If it encounters a single item for which pred is false,
   then the loop short-circuits and returns false from for-all.
   Otherwise for-all returns true."
  (block 'outer-for-all
    (loop for item across domain
	  do (if (null (funcall pred item))
		 (return-from 'outer-for-all nil)))
    t))


(defun for-some (pred domain)
  "for-some is the Existential Quantification of a predicate (pred),
   the proposition that is true if and only if pred is true for at
   least one item in a universe of discourse (domain).
   This function loops across domain (which has finite size)
   to see if pred is ever true.
   If it encounters a single item for which pred is true,
   then the loop short-circuits and returns true from for-all.
   Otherwise for-all returns false."
  (block 'outer-for-some
    (loop for item across domain
	  do (if (funcall pred item)
		 (return-from 'outer-for-some t)))
    nil))

; sample calls using evenp and oddp predicates, and vectors of integers for domains
(for-all  'evenp [1 2 3]) ; => nil
(for-some 'evenp [1 2 3]) ; => t
(for-all  'evenp [2 4 6]) ; => t
(for-some 'evenp [1 3 7]) ; => nil
(for-all  'oddp [1 3 5])  ; => t
(for-some 'oddp [2 6 10]) ; => nil
(for-all  'oddp [5 4 3]) ; => nil
(for-some 'oddp [6 4 3]) ; => t

(for-all  'symbolp [a b c]); => t
(for-all  'symbolp [a b c 1 2 3]); => nil

(defun is-prime(n)
  (block 'is-prime-block
  (loop for i from 2 to (- n 1)
	do(if (= 0 (mod n i))
	      (return-from 'is-prime-block nil)))
  t))
  
(defun square (n)
  "Square a number n by multiplying it by itself."
  (* n n))

; sample binary predicate using numbers with a more complex relationship
(defun is-pythagorean (x y)
  "Does x^2 + y^2 = z^2 --- a perfect square?"
  (let* ((z (sqrt (+ (square x) (square y))))
	 (diff (- z (floor z))))
    (zerop diff)))

; sample binary predicate using everyday things
(defun is-typically-eaten (food meal)
  "Very incomplete knowledge-base of food and meals."
  (or (and (eq meal 'breakfast)
	   (eq food 'eggs))
      (and (eq meal 'breakfast)
	   (eq food 'bacon))
      (and (eq meal 'breakfast)
	   (eq food 'toast))
      (and (eq meal 'lunch)
	   (eq food 'sandwich))
      (and (eq meal 'dinner)
	   (eq food 'steak))
      (and (eq meal 'dinner)
	   (eq food 'potatoes))))

; sample call returning nil
(is-typically-eaten 'bacon 'lunch)

; sample call returning t
(is-typically-eaten 'bacon 'breakfast)

#+END_SRC

* What Is True?

  The following code block is a self-assesssment tool that allows you to
  use "fuzzy logic" (truth values from 0 to 100):

#+BEGIN_SRC emacs-lisp :tangle yes
(defun what-is-true-about-my-engagement-with-coolness ()
  "If need be, replace a 100 with a smaller number to reflect how much you feel you deserve."
  (vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is true of my experience in general?                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

["I had fun." 100]
["I collaborated with one or more classmates." 100]
["I learned something new." 100]
["I achieved something meaningful, or something I can build upon at a later time." 100]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is true of my report on what I learned?                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

["I learned the basics of emacs 'org' mode in order to write it." 100]
["I explain usage of my code with plain instructions on how to run WITH command-line parameters to run the various tests." 100]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is true of the mechanical "infelicities" (misspelled words,        ;;
;; grammatical errors, punctuation errors) in my report of what I learned? ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

["There are fewer than four." 100]
["There are fewer than three." 100]
["There are fewer than two." 100]
["There are none." 100]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is true of how my code handles command-line arguments              ;;
;; specifying what is to be tested?                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

["It handles TAA independently or in any combination with the others." 100]
["It handles FAA independently or in any combination with the others." 100]
["It handles TAS independently or in any combination with the others." 100]
["It handles FAS independently or in any combination with the others." 100]
["It handles TSA independently or in any combination with the others." 100]
["It handles FSA independently or in any combination with the others." 100]
["It handles TSS independently or in any combination with the others." 100]
["It handles FSS independently or in any combination with the others." 100]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is true about the correctness and completeness of my code?         ;;
;; Do the four functions, for-all-for-all, for-all-for-some,               ;;
;; for-some-for-all, and for-some-for-some, work correctly?                ;;
;; As verified specifically by testing? Namely that actual output matches  ;;
;; expected output for ...                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

["at least one *true* case of for-all-for-all." 100]
["at least one *false* case of for-all-for-all." 100]
["at least one *true* case of for-all-for-some." 100]
["at least one *false* case of for-all-for-some." 100]
["at least one *true* case of for-some-for-all." 100]
["at least one *false* case of for-some-for-all." 100]
["at least one *true* case of for-some-for-some." 100]
["at least one *false* case of for-some-for-some." 100]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is true about the clarity of my test output?                       ;;
;; Is it true that my test output is formatted such that                   ;;
;; it is crystal clear [at a glance] how for each case below               ;;
;; the expected values match their actual values?                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

["true for-all-for-all --- test TAA." 100]
["false for-all-for-all --- test FAA." 100]
["true for-all-for-some --- test TAS." 100]
["false for-all-for-some --- test FAS." 100]
["true for-some-for-all --- test TSA." 100]
["false for-some-for-all --- test FSA." 100]
["true for-some-for-some --- test TSS." 100]
["false for-some-for-some --- test FSS." 100]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is true about the elegance of my code?                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

["It has some by virtue of its correctness, as first and foremost, an elegant solution is a correct solution." 100]
["It increases by means of helper functions that increase cohesion, and minimize the complexity of the logic." 100]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is true about the creativity of my code?                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

["It is minimal, but at least it shows how the greater-than predicate works." 100]
["It increases by further using a predicate of my own devising that uses arithmetic and logic, or just numbers related by a compound relational expression." 100]
["It increases by including one or more predicates that relate to my everyday world or domain of discourse." 100]

))
#+END_SRC
