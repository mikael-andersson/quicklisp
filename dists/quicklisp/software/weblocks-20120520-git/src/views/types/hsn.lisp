
(in-package :weblocks)

(export '(*hsns* *use-suggest-for-hsns* hsn
	  hsn-presentation hsn-presentation-use-suggest-p
	  hsn-presentation-input-id
	  hsn-presentation-choices-id hsn-parser))

(defparameter *hsns*
  '(("HSN5" . "HSN5") ("HSN7" . "HSN7") ("HSN12" . "HSN12"))
  "A alist of us state names and their abbreviations.")

;  '(("Alabama" . "AL") ("Alaska" . "AK") ("Arizona" . "AZ") ("Arkansas" . "AR")
;    ("Wisconsin" . "WI") ("Wyoming" . "WY"))

(defparameter *hsns-abreviation->name*
  (mapcar (lambda (state)
	    (cons (cdr state) (car state)))
	  *hsns*)
  "An alist of us state abbreviations mapped to state names.")

(defparameter *use-suggest-for-hsns* t
  "If true, suggest snippet will be used to render hsn types in
forms. A simple dropdown will be used otherwise.")

 (defun hsn-p (str)
   "Used by the type specifier 'hsn' to determine if a string is a
 state."
   (not (null (member str *hsns* :test #'string= :key #'cdr))))

(deftype hsn ()
  '(satisfies hsn-p))

(defclass hsn-presentation (input-presentation)
  ((use-suggest-p :initform *use-suggest-for-hsns*
		  :initarg :use-suggest-p
		  :accessor hsn-presentation-use-suggest-p
		  :documentation "If set to true, suggest snippet will
		  be used as a state input control. Otherwise,
		  dropdown will be used.")
   (input-id :initform (gensym)
	     :initarg :input-id
	     :accessor hsn-presentation-input-id
	     :documentation "An input ID passed to suggest or
	     dropdown.")
   (choices-id :initform (gensym)
	       :initarg :choices-id
	       :accessor hsn-presentation-choices-id
	       :documentation "A choices ID passed to suggest.")))

(defmethod render-view-field-value (value (presentation hsn-presentation) 
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (let ((selections (mapcar #'car *hsns*))
	  (default-value (if intermediate-value-p
			     intermediate-value
			     (append (ensure-list value)
				     (ensure-list
				      (cdr (assoc value *hsns-abreviation->name*
						  :test #'equalp))))))
	  (welcome-name "State"))
      (if (hsn-presentation-use-suggest-p presentation)
	  (render-suggest (if field-info
                            (attributize-view-field-name field-info)
                            (attributize-name(view-field-slot-name field)))
			  selections
			  :default-value default-value
			  :welcome-name welcome-name
			  :max-length (input-presentation-max-length presentation)
			  :input-id (hsn-presentation-input-id presentation)
			  :choices-id (hsn-presentation-choices-id presentation))
	  (render-dropdown (attributize-view-field-name field-info) selections
			   :selected-value default-value
			   :welcome-name welcome-name
			   :id (hsn-presentation-input-id presentation))))))

(defclass hsn-parser (parser)
  ((error-message :initform "a valid US state"))
  (:documentation "A parser designed to parse strings into
  a US state."))

(defmethod parse-view-field-value ((parser hsn-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (let ((value (string-trim +whitespace-characters+ value)))
    (if (empty-p value)
	(values t nil)
	(if (eq (length value) 2)
	    (when (hsn-p (string-upcase value))
	      (values t t (string-upcase value)))
	    (let ((state (assoc value *hsns* :test #'equalp)))
	      (when state
		(values t t (cdr state))))))))

;;; Scaffolding magic
(defmethod typespec->view-field-presentation ((scaffold form-scaffold)
					      (typespec (eql 'hsn)) args)
  (values t (make-instance 'hsn-presentation)))

(defmethod typespec->form-view-field-parser ((scaffold form-scaffold)
					     (typespec (eql 'hsn)) args)
  (values t (make-instance 'hsn-parser)))

(defmethod dependencies append ((self hsn-presentation))
  (when (hsn-presentation-use-suggest-p self)
    (list (make-local-dependency :stylesheet "suggest"))))
