;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.


;* Coolness Sample Code

; Use the following code block as a guide and a starting point for
;  your exploration:

;#+BEGIN_SRC emacs-lisp :tangle yes
(require 'cl); for loop macro
;"For-some-for-some
;* if at least one item in domain x meets the criteria
;* of the predicate return true
;* else return false"
(defun for-some (pred itemX domain)
 ; "for-some is the Existential Quantification of a predicate (pred),
 ;  the proposition that is true if and only if pred is true for at
 ;  least one item in a universe of discourse (domain).
 ;  This function loops across domain (which has finite size)
 ;  to see if pred is ever true.
 ;  If it encounters a single item for which pred is true,
 ;  then the loop short-circuits and returns true from for-all.
 ;  Otherwise for-all returns false."
  (block 'outer-for-some
    (loop for itemY across domain
	  do (if (funcall pred itemX itemY)
		 (return-from 'outer-for-some t)))
    nil))

(defun for-all (pred itemX domain)
;  "for-all is the Universal Quantification of a predicate (pred),
 ;  the proposition that is true if and only if pred is true for all
  ; items in a universe of discourse (domain).
  ; This function loops across domain (which has finite size)
  ; to see if pred is always true.
  ; If it encounters a single item for which pred is false,
  ; then the loop short-circuits and returns false from for-all.
  ; Otherwise for-all returns true."
  (block 'outer-for-all
    (loop for itemY across domain
	  do (if (null (funcall pred itemX itemY))
		 (return-from 'outer-for-all nil)))
    t))
(defun for-some-for-some (pred x y)
  (block 'outer-for-some-for-some
    (loop for itemX across x
	 do (loop for itemY across y
		do(if(funcall pred itemX itemY)
		      (return-from 'outer-for-some-for-some t))))
    nil))
(for-some-for-some '> [1 2 3] [4 5 6]); should return nil
(for-some-for-some '< [1 2 3] [4 5 6]); should return t

;"For-all-for-all
;if all of the itmes in domain x meet the criteria
;for the predicate for all of domain y return true"
(defun for-all-for-all (pred x y)
  (block 'outer-for-all-for-all
    (loop for itemX across x
	 do (loop for itemY across y
		do(if (null (funcall pred itemX itemY))
		      (return-from 'outer-for-all-for-all nil))))
    t))
(for-all-for-all '> [1 2 3] [4 5 6]); should return nil
(for-all-for-all '< [1 2 3] [4 5 6]); should return t

;"If at all of domain X meet the criteria for the predicte
;for at least one item of domain Y return true else return nil"
(defun f-a-f-s (pred x y)
  (block 'outer
    (loop for itemX across x
	  do(if (null(for-some pred itemX y))
		(return-from 'outer nil)
	      ))t))
(f-a-f-s '> [1 2 3] [4 5 6]); should return nil
(f-a-f-s '< [1 2 3] [4 2 1]); should return t

"If at least 1 item in domain x meets the predicate
for all of domain y return true else return false"
(defun f-s-f-a (pred x y)
  (block 'outer-fsfa
	 (loop for itemX across x
	       do   (if (for-all pred itemX y)
	       (return-from 'outer-fsfa t)
	       ))nil))
(f-s-f-a '> [1 2 3] [4 5 1]); should return nil
(f-s-f-a '< [1 2 3] [4 2 7]); should return t


"Returns true if a number greater than 1 is prime"
(defun is-prime(n)
  (block 'is-prime-block
  (loop for i from 2 to (- n 1)
	do(if (= 0 (mod n i))
	      (return-from 'is-prime-block nil)))
  t))
(is-prime 317);should return t 
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

"***********
*Command-Line examples
* myScript TAA --> example of a true for-all-for-all
* myScript FAA --> example of a false for-all-for-all
* myScript TAS --> example of a true for-all-for-some
* myScript FAS --> example of a false for-all-for-some
* myScript TSA --> example of a true for-some-for-all
* myScript FSA --> example of a false for-some-for-all
* myScript TSS --> example of a true for-some-for-some
* myScript FSS --> example of a false for-some-for-some
**********"
(defun command-line()
  (let* ((args (getenv "ARGS"))
	 (arg-list (split-string args " " t)))
    (dolist (arg arg-list)
      (if (string= arg "TAA")
	  (TAA))
      (if (string= arg "FAA")
	  (FAA))
      (if (string= arg "TAS")
	  (TAS))
      (if (string= arg "FAS")
	  (FAS))
      (if (string= arg "TSA")
	  (TSA))
      (if (string= arg "FSA")
	  (FSA))
      (if (string= arg "TSS")
	  (TSS))
      (if (string= arg "FSS")
	  (FSS))
      (princ "\n"))))

(defun TAA()
  (princ "TRUE-FOR-ALL-FOR-ALL\n")
  (princ "TEST (FOR-ALL-FOR-ALL '< [1 2 3] [4 5 6])\n")
  (princ (for-all-for-all '< [1 2 3] [4 5 6])))
(defun FAA()
  (princ "FalSE-FOR-ALL-FOR-ALL\n")
  (princ "TEST (FOR-ALL-FOR-ALL '> [1 2 3] [4 5 6])\n")
  (princ (for-all-for-all '> [1 2 3] [4 5 6])))
(defun TAS()
  (princ "TRUE-FOR-ALL-FOR-SOME\n")
  (princ "TEST (FOR-ALL-FOR-SOME '< [1 2 3] [4 5 6])\n")
  (princ (f-a-f-s '< [1 2 3] [4 5 6])))
(defun FAS()
  (princ "FALSE-FOR-ALL-FOR-SOME\n")
  (princ "TEST (FOR-ALL-FOR-SOME '> [1 2 3] [4 5 6])\n")
  (princ (f-a-f-s '> [1 2 3] [4 5 6])))
(defun TSA()
  (princ "TRUE-FOR-SOME-FOR-ALL\n")
  (princ "TEST (FOR-SOME-FOR-ALL '< [1 2 3] [4 5 6])\n")
  (princ (f-s-f-a '< [1 2 3] [4 5 6])))
(defun FSA()
  (princ "FALSE-FOR-SOME-FOR-ALL\n")
  (princ "TEST (FOR-SOME-FOR-ALL '> [1 2 3] [4 5 6])\n")
  (princ (f-s-f-a '> [1 2 3] [4 5 6])))
(defun TSS()
  (princ "TRUE-FOR-SOME-FOR-SOME\n")
  (princ "TEST (FOR-SOME-FOR-SOME '< [1 2 3] [4 5 6])\n")
  (princ (for-some-for-some '< [1 2 3] [4 5 6])))
(defun FSS()
  (princ "FALSE-FOR-SOME-FOR-SOME\n")
  (princ "TEST (FOR-SOME-FOR-SOME '> [1 2 3] [4 5 6])\n")
  (princ (for-some-for-some '> [1 2 3] [4 5 6])))

;#+END_SRC

;* What Is True?

 ; The following code block is a self-assesssment tool that allows you to
 ; use "fuzzy logic" (truth values from 0 to 100):

;#+BEGIN_SRC emacs-lisp :tangle yes
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
;#+END_SRC
