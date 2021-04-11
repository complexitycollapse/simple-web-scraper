;;;; simple-web-scraper.lisp

(in-package #:simple-web-scraper)

(defun safe-subseq (sequence start &optional end)
  (if (not end) (subseq sequence start)
      (subseq sequence start (min end (length sequence)))))

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

(defun http-request-with-retries (url stream close)
  (handler-case (http-request url :stream stream :close close)
    (T () (http-request url :close close))))

(defun fetch-all-urls (urls)
  (let ((urls (reverse urls))
	(total (length urls)))
    (loop for url in urls
       for rest on urls
       for count from 1
       for stream = nil then (nth 4 values)
       for values = (multiple-value-list (http-request-with-retries url stream (endp (cdr rest))))
       do (format T "Fetching ~S of ~S: ~S~%" count total url)
       collect (cons url (car values)))))

(defun fetch-all (url-patterns &optional batch-size (first-batch 1) last-batch)
  (let* ((urls (generate-urls url-patterns))
	 (number-of-batches (if batch-size (ceiling (length urls) batch-size) 1))
	 (results nil))
    (if batch-size (format T "Fetching ~S URLs in ~S batches~%" (length urls) number-of-batches))
    (handler-case
	(do ((b first-batch (1+ b))) ((> b (or last-batch number-of-batches)))
	  (let ((batch (if batch-size (safe-subseq urls (* (1- b) batch-size) (* b batch-size)) urls)))
	    (format T "Fetching the following ~S URLs~S: ~S~%"
		    (length batch)
		    (if batch-size (format nil " (batch number ~S)" b) "")
		    batch)
	    (setf results (nconc (fetch-all-urls batch) results)))
	  (sleep 1))
      (T (e) (format T "Error: ~A~%" e)))
    results))

(defun walk-tree (fun tree)
  (subst-if t
            (constantly nil)
            tree
            :key fun))

(defun has-attribute (elt attribute-name class &optional search-in-value)
  (if (not (consp elt)) nil
      (let ((tag (car elt)))
	(if (consp tag)
	    (let ((pos-of-class (position attribute-name tag)))
	      (if pos-of-class
		  (let ((attribute-value (nth (1+ pos-of-class) tag)))
		    (if search-in-value
			(let ((classes (split-sequence #\space attribute-value)))
			  (find class classes :test #'string=))
			(string= class attribute-value)))))))))

(defun get-by-attribute (html attribute-name value &optional search-in-value)
  (collecting
    (walk-tree (lambda (e) (if (has-attribute e attribute-name value search-in-value)
			       (collect e)))
	       html)))
