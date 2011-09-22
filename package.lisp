(defpackage :chtml-matcher
  (:use :cl :stdutils :f-underscore)
  (:export ;; Some helpers for lhtml
           :html->lhtml :lhtml->html
	   :lhtml-node-name :lhtml-node-attributes
	   :lhtml-node-body
	   :lhtml-node-attribute-name 
	   :lhtml-node-attribute-value
	   :lhtml-constant-node-p
	   :lhtml-node-string 
           ;; Templates and lhtml
           :find-in-lhtml :match-template 
	   ;; Bindings
	   :make-bindings :binding-dictionary
	   :get-binding :set-binding 
	   :set-bindings :get-bindings
	   :with-bindings :clear-bindings
	   :make-bindings))