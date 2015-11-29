;*******************************************************************************
; CS 237 - Discrete Math
;   Assignment: Exploration 1 - Coolness
;   Professor: 
;       Br. Neff
;   Auther:
;       Hunter Marshall
;*******************************************************************************

(require 'cl) ; for loop macro
;; (require 'ment) ; for true and false

;*******************************************************************************
; COMMAND-LINE INSTRUCTIONS
; Allows the user to enter in the command line what the user wants to test for.
;  myScript TAA -- Tests for-all-for-all for a true expression
;  myScript FAA -- Tests for-all-for-all for a false expression
;  myScript TAS -- Tests for-all-for-some for a true expression
;  myScript FAS -- Tests for-all-for-some for a false expression
;  myScript TSA -- Tests for-some-for-all for a true expression
;  myScript FSA -- Tests for-some-for-all for a false expression
;  myScript TSS -- Tests for-some-for-some for a true expression
;  myScript FSS -- Tests for-some-for-some for a false expression
;  myScript ALL -- Tests all of the above at once
;*******************************************************************************

;*******************************************************************************
; PRINT_ARGS 
; This was just a test to see if user can call functions from the command line
;*******************************************************************************
(defun print-args ()                    ;
  (let* ((args (getenv "ARGS"))
	 (arg-list (split-string args " " t)))
    (dolist (arg arg-list)
      (princ arg)
      (princ "\n"))))

;*******************************************************************************
; TEST
; Tests all the functions from the command line. This allows the user what they
; want to test for, and then calls the correct function.
;*******************************************************************************
(defun test()
  (let* ((args (getenv "ARGS"))
         (arg-list (split-string args " " t)))
    (dolist (arg arg-list)
      (if (string= arg "TAA")
          (testTAA))
      (if (string= arg "FAA")
          (testFAA))
      (if (string= arg "TAS")
          (testTAS))
      (if (string= arg "FAS")
          (testFAS))
      (if (string= arg "TSA")
          (testTSA))
      (if (string= arg "FSA")
          (testFSA))
      (if (string= arg "TSS")
          (testTSS))
      (if (string= arg "FSS")
          (testFSS))
      (if (string= arg "ALL")
          (testALL)
           )
      (princ "\n"))))

;*******************************************************************************
; testALL
; Tests all of the tests that the user can test
;*******************************************************************************
(defun testALL()
 (testTAA)
 (princ "\n")
 (testFAA)
 (princ "\n")
 (testTAS)
 (princ "\n")
 (testFAS)
 (princ "\n")
 (testTSA)
 (princ "\n")
 (testFSA)
 (princ "\n")
 (testTSS)
 (princ "\n")
 (testFSS)
)

;*******************************************************************************
; TestTAA
; Tests for-all-for-all for a true expression
;*******************************************************************************
(defun testTAA()
  (princ "This tests for-all-for-all\n")
  (princ "The first test is [1 2 3] < [4 5 6]\n")
  (princ "Expected Result: TRUE\n")
  (if (for-all-for-all '< [1 2 3] [4 5 6])
      (princ "Acutal result: TRUE\n")
    (princ "Actual result: FALSE\n"))
)

;*******************************************************************************
; TESTFAA
; Tests for-all-for-all for a false expression
;*******************************************************************************
(defun testFAA()
  (princ "This tests for-all-for-all\n")
  (princ "The first test is [1 2 3] > [4 5 6]\n")
  (princ "Expected Result: FALSE\n")
  (if (null (for-all-for-all '> [1 2 3] [4 5 6]))
      (princ "Acutal result: FALSE\n")
    (princ "Actual result: TRUE\n"))
)

;*******************************************************************************
; TESTTAS
; Tests for-all-for-some for a true expression
;*******************************************************************************
(defun testTAS()
  (princ "This tests for-all-for-some\n")
  (princ "The first test is [1 2 3] < [4 5 6]\n") 
  (princ "Expected Result: TRUE\n")
  (if (for-all-for-some '< [1 2 3] [4 5 6])
      (princ "Acutal result: TRUE\n")
    (princ "Actual result: FALSE\n"))
)

;*******************************************************************************
; TESTFAS
; Tests for-all-for-some for a false expression
;*******************************************************************************
(defun testFAS()
  (princ "This tests for-all-for-some\n")
  (princ "The first test is [1 2 3] > [4 5 6]\n")
  (princ "Expected Result: FALSE\n")
  (if (null (for-all-for-all '> [1 2 3] [4 5 6]))
      (princ "Acutal result: FALSE\n")
    (princ "Actual result: TRUE\n"))
)

;*******************************************************************************
; TESTTSA
; Tests for-some-for-all for a true expression
;*******************************************************************************
(defun testTSA()
  (princ "This tests for-some-for-all\n")
  (princ "The first test is [1 2 3] < [4 5 6]\n") 
  (princ "Expected Result: TRUE\n")
  (if (for-all-for-some '< [1 2 3] [4 5 6])
      (princ "Acutal result: TRUE\n")
    (princ "Actual result: FALSE\n"))
)

;*******************************************************************************
; TESTFSA
; Tests for-some-for-all for a false expression
;*******************************************************************************
(defun testFSA()
  (princ "This tests for-some-for-all\n")
  (princ "The first test is [1 2 3] > [4 5 6]\n")
  (princ "Expected Result: FALSE\n")
  (if (null (for-all-for-all '> [1 2 3] [4 5 6]))
      (princ "Acutal result: FALSE\n")
    (princ "Actual result: TRUE\n"))
)

;*******************************************************************************
; TESTTSS
; Tests for-some-for-some for a true expression
;*******************************************************************************
(defun testTSS()
  (princ "This tests for-some-for-some\n")
  (princ "The first test is [1 2 3] < [4 5 6]\n") 
  (princ "Expected Result: TRUE\n")
  (if (for-all-for-some '< [1 2 3] [4 5 6])
      (princ "Acutal result: TRUE\n")
    (princ "Actual result: FALSE\n"))
)

;*******************************************************************************
; TESTFSS
; Tests for-some-for-some for a false expression
;*******************************************************************************
(defun testFSS()
  (princ "This tests for-some-for-some\n")
  (princ "The first test is [1 2 3] > [4 5 6]\n")
  (princ "Expected Result: FALSE\n")
  (if (null (for-all-for-all '> [1 2 3] [4 5 6]))
      (princ "Acutal result: FALSE\n")
    (princ "Actual result: TRUE\n"))
  )


;*******************************************************************************
; This section is the functions that do the actual caluculations. 
; We will start with two helper functions that will to help make the
; functions doing the calculations more elegant. 
;*******************************************************************************
;*******************************************************************************
; HELPERS
;   FOR-SOME-HELPER
;   FOR-ALL-HELPER
; Two helper functions that will to help make the functions doing the 
; calculations more elegant.
;*******************************************************************************
;FOR-SOME_HELPER
(defun for-some-helper (pred itemx y)
  (block 'outer-for-some-helper
    (loop for itemy across y
          do (if (funcall pred itemx itemy)
                      (return-from 'outer-for-some-helper t)))
                nil))

; Example for the for-some-helper
(for-some-helper '< 12 [1 2]) ; should return false

;FOR-ALL-HELPER
(defun for-all-helper (pred itemx y)
  (block 'outer-for-all-helper
    (loop for itemy across y
          do(if (null (funcall pred itemx itemy))
                (return-from 'outer-for-all-helper nil)))
    t))

; Example for the for-all-helper
(for-all-helper '< 10 [9])      ; should return false
(for-all-helper '> 5 [1 2 3 4]) ; should return true

;***************************************************************************
; FOR-ALL-FOR ALL
; "For all x for all y" wants to find (P x y) always true. That's what it
; means for the nested quantification to be true, and naturally, this only
; works if the domains of x and y are finite. Even then, it really only works
; if these domains are reasonably finite  not too big. Iteration is serial,
; after all, and time is short.
; So, for-all-for-all loops through x's domain, and for each x loops through 
; each y in y's domain. On each inner-loop iteration it calls (P x y) and
; checks the value. If the value is ever false, then for-all-for-all is false
; immediately  no need to check any further. Some y has been found for some x
; where the predicate is false. If both loops finish with nary a false
; evaluation, for-all-for-all is ultimately true. There is no x for which, 
; for any y, (P x y) is false.
;****************************************************************************
(defun for-all-for-all (pred x y)
  (block 'outer-for-all-for-all
         (loop for itemx across x
               do (if (null (for-all-helper pred itemx y))
                      (return-from 'outer-for-all-for-all nil)))
         t))

; examples to show that it works
(for-all-for-all '> [1 2 3] [4 5 6]) ; should return false (nil)
(for-all-for-all '< [1 2 3] [4 5 6]) ; should return true (t)

;******************************************************************************
; FOR-SOME-FOR-SOME
; This function loops through x's domain, and for each x loops through each y
; in y's domain. On each inner-loop iteration it calls (P x y) and checks the 
; value. If a true value is found, then for-some-for-some is true  immediately
; no need to check any further. If both loops finish never having triggered
; true, for-some-for-some is ultimately false. There is no x for which there is
; some y for which (P x y) is true.
;******************************************************************************
(defun for-some-for-some (pred x y)
  (block 'outer-for-some-for-some
         (loop for itemx across x
               do (if (for-some-helper pred itemx y)
                      (return-from 'outer-for-some-for-some t)))
         nil))

; example to show it works
(for-some-for-some '> [1 2 3] [4 5 6]) ; SHOULD RETURN FALSE(nil)
(for-some-for-some '< [1 2 3] [4 5 6]) ; SHOULD RETURN TRUE(t)

;******************************************************************************
; FOR-ALL-FOR-some
; "For all x for some y" wants (P x y) to always be true sometimes, and "for
; some x for all y" wants (P x y) to sometimes be true always. AGP to find the 
; best, most elegant way to implement for-all-for-some and for-some-for-all.
;******************************************************************************
(defun for-all-for-some (pred x y)
  (block 'outer-for-all-for-some
    (loop for itemx across x
          do (if (null (for-some-helper pred itemx y))
                 (return-from 'outer-for-all-for-some nil)))
    t))

; example to show it works
(for-all-for-some '> [1 2 3] [4 5 6]) ; should return false (ni)
(for-all-for-some '< [1 2 3] [4 5 6]) ; should return true (t)

;******************************************************************************
; FOR-SOME-FOR-ALL
; "For some x for all y" wants (P x y) to sometimes be true always.
;******************************************************************************
(defun for-some-for-all (pred x y)
  (block 'outer-for-some-for-all
         (loop for itemx across x
               do (if (for-all-helper pred itemx y)
                      (return-from 'outer-for-some-for-all t)))
         nil))
; examples to show it works
(for-some-for-all '> [1 2 3] [4 5 6]) ; should return false (nil)
(for-some-for-all '< [1 2 3] [4 5 6]) ; should return true (t)
