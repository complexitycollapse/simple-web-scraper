;;;; simple-web-scraper.asd

(asdf:defsystem #:simple-web-scraper
  :description "Describe simple-web-scraper here"
  :author "complexitycollapse <complexitycollapse@github.com>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (:drakma :cl-html-parse :cl-utilities)
  :components ((:file "packages")
               (:file "simple-web-scraper")))
