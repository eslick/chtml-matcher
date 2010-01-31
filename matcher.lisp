(in-package :chtml-matcher)

;;
;; LHTML Template Unification
;;

;;
;; Some functions for those who don't want to learn cxml/chtml
;;

(defun html->lhtml (html)
  (chtml:parse html (chtml:make-lhtml-builder)))

(defun lhtml->html (lhtml)
  (chtml:serialize-lhtml lhtml (chtml:make-string-sink)))


;;
;; Parser State 
;;

(defstruct parser-state tree path body)

(defun make-state (lhtml)
  "The initial state consists of a virtual body
   of which the current node is the top level node of
   the tree.  We keep track of the root of the tree."
  (make-parser-state :tree lhtml :path nil
		     :body (list lhtml)))

(defmethod print-object ((state parser-state) stream)
  (format stream "#<STATE ~A d:~A>"
	  (awhen (parser-state-body state)
	    (if (listp it)
		(if (listp (car it))
		    (caar it) 
		    (type-of (car it)))))
	  (length (parser-state-path state))))

(defun current-node (state)
  "Current node is always first element of the body list (Invariant)"
  (first (parser-state-body state)))

(defun reset-state (state)
  "Reset state to the initial state"
  (setf (parser-state-path state) nil)
  (setf (parser-state-body state) 
	(list (parser-state-tree state)))
  state)

(defun copy-state (state)
  "Make a duplicate of the current state"
  (with-struct-slots (parser-state tree path body) state
    (make-parser-state :tree tree :path (copy-list path)
		       :body (copy-list body))))

(defun make-local-state (state)
  "Make a new state object rooted at the current node"
  (copy-state state))

(defmacro with-state ((state) &body body)
  `(with-struct-slots (parser-state path body) ,state
     ,@body))

(defun current-path-tags (state)
  "List of tags from root to current"
  (reverse
   (mapcar #'lhtml-node-name 
	   (list (current-node state)
		 (parser-state-path state)))))

(defun children-done-p (state)
  "If body is empty or has one element, return t"
  (with-state (state)
    (null (cdr body))))

(defun state-done-p (state)
  (with-state (state)
    (and (null body) (null path))))


;;
;; Update parser state
;;

(defmacro assert-state ()
  "Test the invariant properties of the state"
  `(assert (or body (null path))))

(defun next-node (state)
  "Depth first tree walker.  Given the current state, 
   update the state so that (first body) contains the 
   next node in the tree.  Returns the side effected state"
  (with-state (state)
    (assert-state)
    (acond ((state-done-p state)
	    (return-from next-node nil))
	   ((lhtml-node-body (current-node state))   ;; descend?
	    (start-body state it))
	   ((rest body) (pop body))            ;; continue
	   (t (finish-body state)))
    (assert-state))
  state)

(defun next-child (state)
  "Linear walk of the current child list, nil on end of list"
  (with-state (state)
;;    (assert-state)
    (cond ((children-done-p state)
	   (setf body nil)
	   (return-from next-child nil))
	  ((rest body) (pop body))
	  (t (error "Invalid state ~A for next-child" state)))
    (assert-state))
  state)

(defun start-body (state node-body)
  "Modify state to make the first node of the current node's
   body the current node and record the state of the current
   body variable to the path variable.  When we pop, we 
   the next node is at the top so we push the rest of the 
   current body"
  (with-state (state)
    (push (copy-list (rest body)) path)
    (setf body node-body))
  state)

(defun start-current-body (state)
  "Modify state to make the current node the first "
  (let ((node (current-node state)))
    (awhen (and (listp node) (lhtml-node-body node))
      (start-body state it))))

(defun finish-body (state)
  "When we're done with the body, return to prior path,
   popping as necessary"
  (with-state (state)
    (setf body (pop path))
    (when (and (null body) path)
      (finish-body state)))
  state)

    
;;
;; Matching nodes
;;

(defun node-match-p (state tag attributes)
  "Match current node to tag and attributes"
  (let ((node (current-node state)))
    (typecase node
      (cons (and (tag-equal-p (lhtml-node-name node) tag)
		 (attributes-equal-p attributes (lhtml-node-attributes node))))
      (string (tag-equal-p tag 'string)))))

(defun tag-equal-p (tag1 tag2)
  "Ensure that two tags are equal"
  (equalp (ensure-string tag1)
	  (ensure-string tag2)))

(defun attributes-equal-p (tattrs nattrs)
  "Verify that attrs1 is a proper subset of attrs2
   under equalp of string form of names.  Ignore variable
   attribute values"
  (loop for tattr in tattrs
     for nattr = (get-attribute (lhtml-node-attribute-name tattr) nattrs)
     unless (and nattr
		 (or (variable-p (lhtml-node-attribute-value tattr))
		     (equalp (lhtml-node-attribute-value tattr)
			     (lhtml-node-attribute-value nattr))))
     do (return-from attributes-equal-p nil))
  t)

(defun get-attribute (name attributes)
  "Given a name, equalp match string forms of name and attribute nmaes"
  (flet ((key-fn (attr) (ensure-string (first attr))))
    (find (ensure-string name) attributes 
	  :test #'equalp
	  :key #'key-fn)))


;; ===========================================
;;  Finding nodes manually
;; ===========================================

(defun find-in-lhtml (lhtml tag attributes &optional (n 1))
  "Convenience function for generating state from an lhtml tree"
  (find-node (make-state lhtml) tag attributes n))

(defun find-node (state tag attributes &optional (n 1))
  "Find the nth occurance of tag and attributes from current
   state via next-node"
  (labels ((rec (count)
	     (if (node-match-p state tag attributes)
		 (if (= count 1)
		     (current-node state)
		     (progn (next-node state)
			    (rec (1- count))))
		 (progn (if (next-node state)
			    (rec count)
			    nil)))))
    (rec n)))

(defun find-all-nodes (state tag attributes)
  "Find all matching instances of a node in the tree"
  (loop 
     for found = (find-node state tag attributes)
     for node = (current-node state)
     while node collect node
     do (next-node state)))

(defun find-child (state tag attributes)
  "Walk the child list of the parser until a match is found.
   If no more children, returns nil."
  (when (next-child state) 
    (if (node-match-p state tag attributes)
	(current-node state)
	(find-child state tag attributes))))

;;
;; Binding nodes and attributes
;;

(defun find-and-bind-node (state tag attributes variable bindings &optional (n 1))
  "Find a node and bind it and it's attributes if provided"
  (find-node state tag attributes n)
  (awhen (current-node state)
    (bind-node it variable attributes bindings)))

(defun bind-node (node variable attributes-template bindings)
  "Bind the node to the variable including attributes if wanted"
  (when variable (set-binding variable node bindings))
  (bind-attributes node attributes-template bindings))

(defun bind-node-body (node variable bindings)
  "Bind the body list to the variable in bindings"
  (set-binding variable (lhtml-node-body node) bindings))

(defun bind-attributes (node attr-template bindings)
  "Given an attribute template and the current node, when
   bindings exist and a variable occurs in the template
   attribute value position, add it to the bindings"
  (when (listp node)
    (when (or (null bindings) (eq bindings t))
      (setf bindings (make-bindings)))
    (let ((attributes (lhtml-node-attributes node)))
      (loop 
	 for (t-name t-value) in attr-template
	 for attribute = (get-attribute t-name attributes)
	 when (and attribute (variable-p t-value))
	 do (set-binding t-value (lhtml-node-attribute-value attribute) bindings)))
    bindings))

;; ===========================================
;;  Templates
;; ===========================================

;;
;; Recursive template unification
;;
;; All these functions return a closure.  The closure takes a parser 
;; state and returns, possibly empty, bindings if it matches or nil if not.
;; The provided state is side effected, so if you don't want side effects
;; you can use with-local-state to create a subtree.
;;
;; DEBUGGING
;; - Each tgen function can print an indented version of
;;   the operation, current state and criterion
;; - Each function that detects a non-match (of body, for instance)
;;   prints an optional message indicating what clause failed to match
;;   Is there a clean way to do this?

(defparameter *match-logging* nil)
(defparameter *match-logging-stream* t)
(defparameter *match-logging-indent* 0)

(defmacro match-log-message (op state &rest criterion)
  `(when *match-logging*
     (format *match-logging-stream* 
	     "~A~A ~A ~A~%" (make-string *match-logging-indent* 
					 :initial-element #\Space)
	     ,op ,@criterion (log-state ,state))))

(defun match-log-end (result)
  (when *match-logging*
    (format *match-logging-stream*
	    "~A=> ~A~%" (make-string *match-logging-indent* 
					:initial-element #\Space) 
	    result)))

(defun log-state (state)
  ;; State message here
  (let ((node (current-node state)))
    (if (listp node) (list (first node) (first (second node)) "...")
	(type-of node))))

(defmacro tglambda (args msg &body body)
  (assert (= (length args) 1))
  (assert (every #'symbolp args))
  `(lambda ,args
     (incf *match-logging-indent* 2)
     (match-log-message ',(first msg) ,(first args) (list ,@(rest msg)))
     (unwind-protect 
	  (aprog1 (progn ,@body)
	    (match-log-end it))
       (decf *match-logging-indent* 2))))
       

;;
;; Compact use of state variables
;;

(defmacro with-local-parse-state ((var state) &body body)
  "Make it easy to perform a non-distructive parse operation
   over a subtree based on the current parse state"
  `(let ((,var (make-local-state ,state)))
     ,@body))

(defmacro with-body-binds ((var state fn) &body body)
  (with-gensyms (fnx)
    `(let* ((,fnx ,fn)
	    (,var (if ,fnx (funcall ,fnx ,state) t)))
     ,@body)))

;;
;; Generators
;;

(defun trace-tgen ()
  (trace SET-BINDINGS ATTRIBUTES-EQUAL-P NODE-MATCH-P START-CURRENT-BODY GET-ATTRIBUTE SET-BINDING))

(defun tgen-match-var (variable)
  "Matches anything and binds it to variable in a fresh binding
   Returns: bindings"
  (tglambda (state) (var variable)
    (make-bindings variable (current-node state))))

(defun tgen-match-string (string)
  "Returns: t when it matches"
  (tglambda (state) (str string)
    (when (equal (current-node state) string) t)))

(defun tgen-match-regex (variable expr)
  "Returns: binding with variable matched to regex register result or nil"
  (tglambda (state) (regex expr)
    (let ((node (current-node state)))
      (when (stringp node)
	(mvbind (scan registers)
	    (ppcre:scan-to-strings expr node)
	  (when scan (make-bindings variable (array->list registers))))))))

(defun tgen-match-fn (fn)
  "Returns: result from calling function
   Side Effect: next-child"
  (tglambda (state) (fn)
    (awhen (funcall fn (current-node state)) it)))
	  

(defun tgen-match (tag attributes body-fn)
  "Try to match the current node to tag & attributes if body-fn is satisfied
   and return any bound attributes.  Moves parse state to the next child node."
  (tglambda (state) (match tag attributes)
    (when (node-match-p state tag attributes)
      (with-local-parse-state (fresh state)
	(when (start-current-body fresh)
	  (with-body-binds (binds fresh body-fn)
	    (unless (null binds)
	      (bind-attributes (current-node state) attributes binds))))))))
	  
(defun tgen-match-bind (variable tag attributes body-fn)
  "Match node and add a reference to it to the bindings.  
   Parse state is unchanged.  Relies on tgen-match debug info"
  (let ((matcher (tgen-match tag attributes body-fn)))
    (lambda (state)
      (awhen (funcall matcher state)
	(set-binding variable 
		     (if (instance-variable-p variable)
			 (current-node state)
			 (lhtml-node-body (current-node state)))
		     it)
	it))))

(defun tgen-find (tag attributes body-fn)
  "Find a node by tag and attributes and bind via tgen-match.
   State points to the child node after the bound node"
  (let ((node-matcher (tgen-match tag attributes body-fn)))
    (tglambda (state) (find tag attributes)
      (catch 'found
	(while (find-node state tag attributes)
	  (aif (funcall node-matcher (copy-state state))
	       (throw 'found it)
	       (next-node state)))))))

(defun tgen-find-bind (variable tag attributes body-fn)
  "Like tgen-find, but uses tgen-match-bind"
;;  (error "foo"))
   (let ((node-matcher (tgen-match-bind variable tag attributes body-fn)))
     (lambda (state)
       (catch 'found
	 (while (find-node state tag attributes)
	   (aif (funcall node-matcher (copy-state state))
		(throw 'found it)
		(next-node state)))))))

(defun tgen-match-nth (count body-fn)
  "Find the nth match for the provided state assuming body-fn 
   moves the state to the next relevant node to test.  Basically
   it's a closure that when it's called, recursively calls body-fn
   until counter hits zero and returns the last value of body-fn"
  (assert (> count 0))
  (tglambda (state) (nth count)
    (labels ((rec ()
	       (match-log-message 'nth state (list count))
	       (with-local-parse-state (fresh state)
		 (declare (ignore fresh))
		 (aif (funcall body-fn state)
		      (if (= (decf count) 0) it
			  (progn (next-child state) (rec)))
		      (next-child state)))))
      (rec))))

(defun map-child-bindings (fn state body-fns &aux (count 1000))
  "Map fn across sequential applications of body-fns for
   the body list of the provided state.  Moves state to
   end of child list and returns bindings if all match"
  (labels ((rec (flist)
	     (if (<= (decf count) 0) (error "Count depth exceeded"))
	     (cond ((null flist) t)
		   ((null (current-node state)) nil)
		   (t (acond ((funcall (first flist) state)
			      (setf count 1000)
			      (funcall fn it)
			      (next-child state)
			      (rec (rest flist)))
			     (t (next-child state)
				(rec flist)))))))
    (rec body-fns)))

(defun tgen-merge-children (body-fns)
  "Assumes the parse tree is looking at the first element of a tag body
   and that the body-fns are required sequential matches.  Walks children
   until (current-node subtree) is null or all body-fns have been processed.
   Merges all the bindings returned from each body-fn.  Each body-fn goes to
   next-child."
  (tglambda (state) (merge)
    (with-local-parse-state (subtree state)
      (let ((merged (make-bindings)))
	(and (map-child-bindings 
	      (f (binds) (set-bindings binds merged))
	      subtree body-fns)
	     (> (length (bindings merged)) 0)
	     merged)))))

(defun tgen-bind-children (variable body-fns)
  "Same as tgen-merge-children but records the list of bindings from the
   body-fns to variable in a fresh bindings set"
  (tglambda (state) (all variable)
    (with-local-parse-state (fresh state)
      (let ((results nil))
	(labels ((map-children ()
		   (map-child-bindings 
		    (f (bindings) (push bindings results))
		    fresh body-fns)))
	  (while (map-children))
	  (when results (make-bindings variable (nreverse results))))))))

;;
;; Template "compiler" 
;;

;; Turns template into a big nested closure.
;; Needs proper debug support! 

;; - Each form creates a context for processing it's body
;; - The body is simply a closure called on the current parser state?

(defmacro with-template ((tag args body) template &body rest)
  "Generate local vars for various for template components"
  (assert (every #'symbolp (list tag args body)))
  (with-gensyms (temp)
    `(when template
       (let* ((,temp ,template)
	      (,tag (first ,temp))
	      (,args (template-args ,temp))
	      (,body (template-body ,temp)))
	 (declare (ignorable ,tag ,args ,body))
	 ,@rest))))

;;(bind-all ?records
;;  (nth 2 (<tr :class "foo" :bar ?attribute
;;	      (bind-all ?statements
;;		((<p ?node) :class "bar"
;;	             ?bind-body)

(defmethod generate-template (template)
  "Recursively walk the template, generating nested matcher functions"
  (cond ((null template) nil)
	((variable-p template)
	 (tgen-match-var template))
	((stringp template)
	 (tgen-match-string template))
	((listp template)
	 (gen-template-matcher (as-keyword (first template)) (rest template)))
	(t (error "Unrecognized template object ~A" template))))

(defmethod gen-template-matcher ((head (eql :nth)) tbody)
  (dbind (count body) tbody
    (tgen-match-nth count (generate-template body))))

(defmethod gen-template-matcher ((head (eql :fn)) tbody)
  (dbind (fn) tbody
    (tgen-match-fn fn)))

(defmethod gen-template-matcher ((head (eql :regex)) tbody)
  (dbind (var regex) tbody
    (assert (variable-p var))
    (tgen-match-regex var regex)))

(defmethod gen-template-matcher ((head (eql :all)) tbody)
  (dbind (var &rest forms) tbody
    (assert (variable-p var))
    (tgen-bind-children var (mapcar #'generate-template forms))))

(defmethod gen-template-matcher ((head (eql :merge)) forms)
  (tgen-merge-children (mapcar #'generate-template forms)))

(defmethod gen-template-matcher ((head symbol) tbody)
  (flet ((gen-body-fn (body)
	   (when body
	     (tgen-merge-children 
	      (mapcar #'generate-template body)))))
    (if (variable-p head)
	(dbind (tag attrs &rest body) tbody
	  (let ((body-fn (gen-body-fn body)))
	    (if (search-tag-p tag)
		(tgen-find-bind head (symbol->base tag) attrs body-fn)
		(tgen-match-bind head tag attrs body-fn))))
	(dbind (attrs &rest body) tbody
	  (let ((body-fn (gen-body-fn body)))
	    (if (search-tag-p head)
		(tgen-find (symbol->base head) attrs body-fn)
		(tgen-match head attrs body-fn)))))))
			  
(defun match-template (template datum)
  "Top level matcher"
  (let ((*match-logging-indent* 0)
	(state (typecase datum
		 (string (make-state (html->lhtml datum)))
		 (cons (make-state datum))
		 (t datum))))
    (declare (special *match-logging-indent*))
    (unless (functionp template)
      (setf template (generate-template template)))
    (funcall template state)))

;;(define-compiler-macro match-template
;;    (&whole form &environment env template &rest rest)
;;  "Translate templates to closures at compile time"
;;  (cond ((and (constantp template env) (listp template))
;; 	 `(match-template (load-time-value (generate-template ,template)) ,@rest))
;; 	(t form)))

;;
;; Matching aids
;;

(defun variable-p (symbol)
  "Identify matching variables by leading #\?"
  (and (symbolp symbol)
       (let ((first (char (symbol-name symbol) 0)))
	 (or (eq first #\?) (eq first #\*)))))

(defun instance-variable-p (symbol)
  (and (symbolp symbol)
       (let ((first (char (symbol-name symbol) 0)))
	 (eq first #\?))))
  
(defun symbol-base (var)
  "Return the base string by stripping the leading character"
  (subseq (symbol-name var) 1))

(defun symbol->base (var)
  "Return a symbol minus the leading character"
  (intern (symbol-base var)))

(defun symbol->base-keyword (var)
  "Return a symbol minus the leading character"
  (intern (symbol-base var) (find-package :keyword)))

(defun search-tag-p (tag)
  "Is this tag a search variable?"
  (and (symbolp tag)
       (eq (char (symbol-name tag) 0) #\<)))

;;
;; LHTML node accessors
;;

(defun lhtml-node-name (node) (if (listp node) (first node) nil))
(defun lhtml-node-attributes (node) (second node))
(defun lhtml-node-body (node) (when (listp node) (cddr node)))
(defun lhtml-node-attribute-name (attr) (first attr))
(defun lhtml-node-attribute-value (attr) (second attr))
(defun lhtml-constant-node-p (tree) (stringp tree))
(defun lhtml-node-string (lhtml-node)
  (format nil "(~A ~A ~A)" 
	  (node-name lhtml-node)
	  (mapcar #'node-attribute-name (node-attributes lhtml-node))
	  (mapcar #'node-name (node-body lhtml-node))))

(defun as-keyword (object)
  "Convert a string or symbol to a keyword symbol"
  (typecase object
    (string (intern object (find-package :keyword)))
    (symbol (intern (symbol-name object) (find-package :keyword)))))



