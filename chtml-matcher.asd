;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage chtml-matcher-system
  (:use :cl :asdf))

(in-package :chtml-matcher-system)

(defsystem chtml-matcher
  :name "chtml-matcher"
  :version "1.0"
  :maintainer "Ian Eslick"
  :author "Ian Eslick"
  :license "MIT style license"
  :description "A unifying template matcher based on closure-html for web scraping and extraction"
  :components ((:file "package")
	       (:file "bindings")
	       (:file "matcher"))
  :depends-on (:closure-html :stdutils :f-underscore))