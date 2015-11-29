(defun print-args ()
  (let* ((args (getenv "ARGS"))
	 (arg-list (split-string args " " t)))
    (dolist (arg arg-list)
      (princ arg)
      (princ "\n"))))

(defun test()
  (let* ((args (getenv "ARGS"))))
  (princ args))

(defun testTAA()