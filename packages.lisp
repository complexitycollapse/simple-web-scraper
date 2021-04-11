;;;; package.lisp

(defpackage #:simple-web-scraper
  (:use #:cl #:drakma #:cl-html-parse #:cl-utilities)
  (:export #:get-by-attribute #:fetch-all))
