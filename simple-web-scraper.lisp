;;;; simple-web-scraper.lisp

(in-package #:simple-web-scraper)

(defvar url-patterns* nil)

(defun clear () (setf url-patterns* nil))
(defun add-url (url) (push url url-patterns*))
(defun add-pattern (pattern start end) (push (list pattern start end) url-patterns*))

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

(defun fetch-all-urls (urls)
  (let ((urls (reverse urls)))
    (loop for url in urls
       for rest on urls
       for stream = nil then (nth 4 values)
       for values = (multiple-value-list (http-request url :stream stream :close (endp (cdr rest))))
       do (format T "Fetching ~S" url)
       collect (cons url (car values)))))

(defun fetch-all ()
  (let ((urls (generate-urls url-patterns*)))
    (format T "Fetching the following URLs: ~S~%" urls)
    (fetch-all-urls urls)))
