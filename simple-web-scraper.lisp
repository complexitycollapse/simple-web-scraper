;;;; simple-web-scraper.lisp

(in-package #:simple-web-scraper)

(defvar url-patterns* nil)

(defun generate-urls (patterns)
  (if (endp patterns) nil
      (let ((p (car patterns)))
	(append (if (stringp p) (list p) (apply #'apply-url-pattern p))
		(generate-urls (cdr patterns))))))

(defun apply-url-pattern (pattern start end)
  (loop for i from start upto end
     collect (construct-url (parse-url-pattern pattern) (format nil "~A" i))))

(defun parse-url-pattern (pattern &optional (start 0))
  (let ((pos (position #\space pattern :start start)))
    (if pos
	(cons (subseq pattern start pos) (cons nil (parse-url-pattern pattern (1+ pos))))
	(list (subseq pattern start)))))

(defun construct-url (parsed-url value)
  (apply #'concatenate 'string (substitute value nil parsed-url)))
