(in-package :chtml-matcher)

;;
;; Variable bindings
;;

(defclass binding-dictionary ()
  ((binds :accessor bindings :initarg :bindings :initform nil)))

(defmethod print-object ((dict binding-dictionary) stream)
  (let ((bindings (bindings dict)))
    (format stream "#<DICT ~A ~A>" (length bindings) (caar bindings))))

(defun make-bindings (&optional variable value)
  "Make bindings, optionally with a seed variable and value"
  (aprog1 (make-instance 'binding-dictionary)
    (when variable
      (set-binding (clean-var variable) value it))))

(defmacro with-bindings (vars bindings &body body)
  `(alist-bind ,vars (bindings ,bindings)
     ,@body))

(defun set-binding (var value dict)
  (assoc-setf (bindings dict) (clean-var var) value))

(defmethod set-bindings (bind1 bind2)
  (declare (ignore bind1 bind2))
  nil)

(defmethod set-bindings (bindings (dict binding-dictionary))
  (declare (ignore bindings)) 
  dict)

(defmethod set-bindings ((bindings cons) (dict binding-dictionary))
  "Set all bindings in bindings list and return the dict. 
   First argument dominates."
  (loop for (var . value) in bindings
       do (set-binding var value dict))
  dict)

(defmethod set-bindings ((dict1 binding-dictionary) dict2)
  (if (or (eq dict2 t) (null dict2)) dict1
      (set-bindings (bindings dict1) dict2)))

(defun get-binding (var dict)
  (when dict
    (assoc-get var (bindings dict))))

(defun get-bindings (dict)
  "Return an alist of bindings"
  (bindings dict))

(defun clear-bindings (dict)
  "Clear all bindings"
  (setf (bindings dict) nil)
  dict)

(defun clean-var (var)
  (if (variable-p var)
      (symbol->base-keyword var)
      var))