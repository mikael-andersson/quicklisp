:allegro8.2
((:class-default-initargs)
 (:class-direct-default-initargs)
 (:default-superclass-for-funcallable-standard-class-is-funcallable-standard-object)
 (:defgeneric-calls-find-method-combination)
 (:defmethod-calls-make-method-lambda fixed)
 (:dependent-protocol-for-generic-functions fixed)
 (:extensible-allocation fixed)
 (:function-invocation-calls-compute-applicable-methods fixed)
 (:function-invocation-calls-compute-applicable-methods-using-classes fixed)
 (:function-invocation-calls-compute-effective-method fixed)
 (:generic-function-argument-precedence-order-returns-required-arguments fixed)
 (:make-method-lambda fixed)
 (:method-functions-take-processed-parameters fixed)
 (:method-lambdas-are-processed fixed)
 (:reinitialize-instance-calls-compute-discriminating-function fixed)
 (:setf-class-name-calls-reinitialize-instance)
 (:setf-generic-function-name-calls-reinitialize-instance)
 (:slot-makunbound-using-class-specialized-on-slot-definition fixed)
 (:standard-class-and-funcallable-standard-class-are-compatible)
 (:subclasses-of-built-in-class-do-not-inherit-exported-slots)
 (:subclasses-of-forward-referenced-class-do-not-inherit-exported-slots)
 (:subclasses-of-funcallable-standard-class-do-not-inherit-exported-slots)
 (:subclasses-of-method-combination-do-not-inherit-exported-slots)
 (:subclasses-of-standard-accessor-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-class-do-not-inherit-exported-slots)
 (:subclasses-of-standard-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-generic-function-do-not-inherit-exported-slots)
 (:subclasses-of-standard-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-reader-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-writer-method-do-not-inherit-exported-slots)
 (:t-is-always-a-valid-superclass))

:clisp2.49
((:accessor-method-initialized-with-function)
 (:add-method-calls-compute-discriminating-function)
 (:compute-slots-requested-slot-order-honoured)
 (:defmethod-calls-make-method-lambda fixed)
 (:forward-referenced-class-changed-by-change-class)
 (:initialize-instance-calls-compute-discriminating-function)
 (:make-method-lambda fixed)
 (:method-initialized-with-function)
 (:method-lambdas-are-processed fixed)
 (:reinitialize-instance-calls-compute-discriminating-function)
 (:remove-method-calls-compute-discriminating-function)
 (:subclasses-of-method-combination-do-not-inherit-exported-slots))

:clozure-common-lisp1.7
((:add-method-calls-compute-discriminating-function fixed)
 (:compute-slots-requested-slot-order-honoured)
 (:defmethod-calls-generic-function-method-class fixed)
 (:defmethod-calls-make-method-lambda fixed)
 (:discriminating-functions-can-be-closures fixed)
 (:discriminating-functions-can-be-funcalled fixed)
 (:function-invocation-calls-compute-applicable-methods fixed)
 (:function-invocation-calls-compute-applicable-methods-using-classes fixed)
 (:function-invocation-calls-compute-effective-method fixed)
 (:generic-functions-can-be-empty fixed)
 (:initialize-instance-calls-compute-discriminating-function fixed)
 (:make-method-lambda fixed)
 (:method-functions-take-processed-parameters fixed)
 (:method-lambdas-are-processed fixed)
 (:reinitialize-instance-calls-compute-discriminating-function fixed)
 (:reinitialize-instance-calls-finalize-inheritance fixed)
 (:reinitialize-lambda-list-reinitializes-argument-precedence-order fixed)
 (:remove-method-calls-compute-discriminating-function fixed)
 (:slot-definition-documentation fixed)
 (:subclasses-of-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-generic-function-do-not-inherit-exported-slots)
 (:subclasses-of-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-accessor-method-do-not-inherit-exported-slot)
 (:subclasses-of-standard-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-generic-function-do-not-inherit-exported-slots)
 (:subclasses-of-standard-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-reader-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-accessor-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-writer-method-do-not-inherit-exported-slots))

:cmu20c
((:accessor-method-initialized-with-function fixed)
 (:accessor-method-initialized-with-lambda-list fixed)
 (:accessor-method-initialized-with-slot-definition fixed)
 (:accessor-method-initialized-with-specializers fixed)
 (:anonymous-classes fixed)
 (:class-default-initargs)
 (:class-direct-default-initargs)
 (:class-initialization-calls-reader-method-class fixed)
 (:class-initialization-calls-writer-method-class fixed)
 (:discriminating-functions-can-be-closures)
 (:discriminating-functions-can-be-funcalled)
 (:documentation-passed-to-effective-slot-definition-class)
 (:effective-slot-definition-initialized-with-documentation)
 (:method-initialized-with-function)
 (:multiple-slot-options-passed-as-list-to-direct-slot-definition-class) ; fix with fix-slot-initargs
 (:reinitialize-instance-calls-compute-discriminating-function fixed)
 (:reinitialize-instance-calls-finalize-inheritance)
 (:setf-class-name-calls-reinitialize-instance)
 (:setf-generic-function-name-calls-reinitialize-instance)
 (:slot-definition-documentation fixed)
 (:standard-class-and-funcallable-standard-class-are-compatible)
 (:subclasses-of-built-in-class-do-not-inherit-exported-slots)
 (:subclasses-of-class-do-not-inherit-exported-slots)
 (:subclasses-of-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-eql-specializer-do-not-inherit-exported-slots)
 (:subclasses-of-forward-referenced-class-do-not-inherit-exported-slots)
 (:subclasses-of-funcallable-standard-class-do-not-inherit-exported-slots)
 (:subclasses-of-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-specializer-do-not-inherit-exported-slots)
 (:subclasses-of-standard-accessor-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-class-do-not-inherit-exported-slots)
 (:subclasses-of-standard-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-generic-function-do-not-inherit-exported-slots)
 (:subclasses-of-standard-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-reader-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-writer-method-do-not-inherit-exported-slots))

:ecl12.2.1
((:add-direct-method fixed)
 (:add-method-calls-add-direct-method fixed)
 (:add-method-calls-compute-discriminating-function fixed)
 (:add-method-calls-remove-method fixed)
 (:add-method-updates-specializer-direct-generic-functions fixed)
 (:add-method-updates-specializer-direct-methods fixed)
 (:class-initialization-calls-reader-method-class)
 (:class-initialization-calls-writer-method-class)
 (:class-reinitialization-calls-remove-direct-subclass fixed)
 (:classes-are-always-their-own-valid-superclasses fixed)
 (:compute-applicable-methods-is-generic fixed)
 (:compute-applicable-methods-using-classes fixed)
 (:compute-effective-method-is-generic fixed)
 (:defgeneric-calls-find-method-combination)
 (:defmethod-calls-generic-function-method-class fixed)
 (:defmethod-calls-make-method-lambda fixed)
 (:dependent-protocol-for-classes fixed)
 (:dependent-protocol-for-generic-functions fixed)
 (:discriminating-functions-can-be-closures fixed)
 (:discriminating-functions-can-be-funcalled fixed)
 (:eql-specializer) ;; partially fixed
 (:eql-specializer-object fixed)
 (:eql-specializers-are-objects)
 (:extract-lambda-list fixed)
 (:extract-specializer-names fixed)
 (:find-method-combination) ;; partially fixed
 (:find-method-is-generic fixed)
 (:function-invocation-calls-compute-applicable-methods fixed)
 (:function-invocation-calls-compute-applicable-methods-using-classes fixed)
 (:function-invocation-calls-compute-effective-method fixed)
 (:generic-function-declarations fixed)
 (:generic-function-method-class-is-generic fixed)
 (:generic-functions-can-be-empty fixed)
 (:initialize-instance-calls-compute-discriminating-function fixed)
 (:intern-eql-specializer fixed)
 (:make-method-lambda fixed)
 (:metaobject)
 (:method-functions-take-processed-parameters)
 (:method-initialized-with-documentation fixed)
 (:method-lambdas-are-processed)
 (:multiple-slot-options-passed-as-list-to-direct-slot-definition-class)
 (:reinitialize-instance-calls-compute-discriminating-function fixed)
 (:remove-direct-method fixed)
 (:remove-method-calls-compute-discriminating-function fixed)
 (:remove-method-calls-remove-direct-method fixed)
 (:remove-method-is-generic fixed)
 (:setf-class-name-calls-reinitialize-instance fixed)
 (:setf-generic-function-name fixed)
 (:setf-generic-function-name-calls-reinitialize-instance fixed)
 (:slot-reader-calls-slot-value-using-class fixed)
 (:slot-writer-calls-slot-value-using-class fixed)
 (:specializer)
 (:specializer-direct-generic-functions fixed)
 (:specializer-direct-methods fixed)
 (:standard-class-and-funcallable-standard-class-are-compatible fixed)
 (:subclasses-of-built-in-class-do-not-inherit-exported-slots)
 (:subclasses-of-class-do-not-inherit-exported-slots)
 (:subclasses-of-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-forward-referenced-class-do-not-inherit-exported-slots)
 (:subclasses-of-funcallable-standard-class-do-not-inherit-exported-slots)
 (:subclasses-of-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-accessor-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-class-do-not-inherit-exported-slots)
 (:subclasses-of-standard-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-generic-function-do-not-inherit-exported-slots)
 (:subclasses-of-standard-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-reader-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-writer-method-do-not-inherit-exported-slots)
 (:t-is-always-a-valid-superclass fixed)
 (:validate-superclass fixed))

:lispworks5.1-5.1.2
((:add-method-calls-compute-discriminating-function)
 (:add-method-updates-specializer-direct-generic-functions fixed)
 (:class-default-initargs)
 (:class-direct-default-initargs)
 (:compute-applicable-methods-using-classes fixed)
 (:compute-default-initargs)
 (:defgeneric-calls-find-method-combination)
 (:eql-specializer) ; partially fixed
 (:eql-specializer-object fixed)
 (:eql-specializers-are-objects)
 (:finalize-inheritance-calls-compute-default-initargs)
 (:find-method-combination fixed) ; partially
 (:funcallable-standard-instance-access fixed)
 (:function-invocation-calls-compute-applicable-methods fixed)
 (:function-invocation-calls-compute-applicable-methods-using-classes fixed)
 (:initialize-instance-calls-compute-discriminating-function)
 (:intern-eql-specializer fixed) ; partially
 (:make-method-lambda fixed)
 (:method-functions-take-processed-parameters fixed)
 (:reinitialize-instance-calls-compute-discriminating-function)
 (:remove-method-calls-compute-discriminating-function)
 (:setf-slot-value-using-class-specialized-on-slot-definition fixed)
 (:slot-boundp-using-class-specialized-on-slot-definition fixed)
 (:slot-makunbound-using-class-specialized-on-slot-definition fixed)
 (:slot-reader-calls-slot-value-using-class fixed)
 (:slot-value-using-class-specialized-on-slot-definition fixed)
 (:slot-writer-calls-slot-value-using-class fixed)
 (:specializer)
 (:specializer-direct-generic-functions fixed)
 (:standard-class-and-funcallable-standard-class-are-compatible)
 (:standard-instance-access fixed)
 (:subclasses-of-built-in-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-forward-referenced-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-funcallable-standard-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-accessor-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-standard-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-generic-function-do-not-inherit-exported-slots)
 (:subclasses-of-standard-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-reader-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-writer-method-do-not-inherit-exported-slots))

:lispworks6.0.1
((:add-method-calls-compute-discriminating-function)
 (:add-method-updates-specializer-direct-generic-functions fixed)
 (:class-default-initargs)
 (:class-direct-default-initargs)
 (:compute-applicable-methods-using-classes fixed)
 (:defgeneric-calls-find-method-combination)
 (:eql-specializer) ; partially fixed
 (:eql-specializer-object fixed)
 (:eql-specializers-are-objects)
 (:find-method-combination fixed) ; partially
 (:funcallable-standard-instance-access fixed)
 (:function-invocation-calls-compute-applicable-methods fixed)
 (:function-invocation-calls-compute-applicable-methods-using-classes fixed)
 (:initialize-instance-calls-compute-discriminating-function)
 (:intern-eql-specializer fixed) ; partially
 (:make-method-lambda fixed)
 (:method-functions-take-processed-parameters fixed)
 (:reinitialize-instance-calls-compute-discriminating-function)
 (:remove-method-calls-compute-discriminating-function)
 (:setf-slot-value-using-class-specialized-on-slot-definition fixed)
 (:slot-boundp-using-class-specialized-on-slot-definition fixed)
 (:slot-makunbound-using-class-specialized-on-slot-definition fixed)
 (:slot-reader-calls-slot-value-using-class fixed)
 (:slot-value-using-class-specialized-on-slot-definition fixed)
 (:slot-writer-calls-slot-value-using-class fixed)
 (:specializer-direct-generic-functions fixed)
 (:standard-class-and-funcallable-standard-class-are-compatible)
 (:standard-instance-access fixed)
 (:subclasses-of-built-in-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-forward-referenced-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-funcallable-standard-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-accessor-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-class-do-not-inherit-exported-slots fixed)
 (:subclasses-of-standard-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-generic-function-do-not-inherit-exported-slots)
 (:subclasses-of-standard-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-reader-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-writer-method-do-not-inherit-exported-slots))

:mcl5.2.1
((:add-method-calls-compute-discriminating-function)
 (:compute-applicable-methods-using-classes)
 (:compute-slots-requested-slot-order-honoured)
 (:default-superclass-for-funcallable-standard-class-is-funcallable-standard-object fixed)
 (:defmethod-calls-generic-function-method-class)
 (:defmethod-calls-make-method-lambda)
 (:discriminating-functions-can-be-closures)
 (:discriminating-functions-can-be-funcalled)
 (:funcallable-instance-functions-can-be-closures)
 (:funcallable-standard-object fixed)
 (:function-invocation-calls-compute-applicable-methods)
 (:function-invocation-calls-compute-applicable-methods-using-classes)
 (:function-invocation-calls-compute-effective-method)
 (:generic-function-declarations)
 (:generic-function-initialized-with-declarations)
 (:generic-functions-can-be-empty)
 (:initialize-instance-calls-compute-discriminating-function)
 (:make-method-lambda)
 (:method-functions-take-processed-parameters)
 (:method-lambdas-are-processed)
 (:reinitialize-instance-calls-compute-discriminating-function)
 (:reinitialize-instance-calls-finalize-inheritance)
 (:reinitialize-lambda-list-reinitializes-argument-precedence-order)
 (:remove-method-calls-compute-discriminating-function)
 (:set-funcallable-instance-function)
 (:setf-generic-function-name)
 (:setf-generic-function-name-calls-reinitialize-instance)
 (:slot-reader-calls-slot-value-using-class fixed)
 (:slot-writer-calls-slot-value-using-class fixed)
 (:subclasses-of-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-accessor-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-generic-function-do-not-inherit-exported-slots)
 (:subclasses-of-standard-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-reader-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-writer-method-do-not-inherit-exported-slots))

:sbcl1.0.55
#| all features implemented |#

:scl1.3.9
((:add-direct-method fixed)
 (:add-method-calls-add-direct-method fixed)
 (:class-default-initargs)
 (:class-direct-default-initargs)
 (:compute-effective-method)
 (:compute-effective-method-is-generic)
 (:defmethod-calls-make-method-lambda)
 (:dependent-protocol-for-classes fixed)
 (:dependent-protocol-for-generic-functions fixed)
 (:discriminating-functions-can-be-funcalled)
 (:eql-specializer)
 (:extensible-allocation)
 (:funcallable-standard-instance-access)
 (:function-invocation-calls-compute-applicable-methods)
 (:function-invocation-calls-compute-effective-method)
 (:make-method-lambda)
 (:method-lambdas-are-processed)
 (:multiple-slot-options-passed-as-list-to-direct-slot-definition-class)
 (:reinitialize-instance-calls-compute-discriminating-function)
 (:remove-direct-method fixed)
 (:remove-method-calls-remove-direct-method fixed)
 (:setf-class-name-calls-reinitialize-instance)
 (:setf-generic-function-name-calls-reinitialize-instance)
 (:specializer)
 (:standard-class-and-funcallable-standard-class-are-compatible)
 (:standard-instance-access)
 (:subclasses-of-built-in-class-do-not-inherit-exported-slots)
 (:subclasses-of-class-do-not-inherit-exported-slots)
 (:subclasses-of-forward-referenced-class-do-not-inherit-exported-slots)
 (:subclasses-of-funcallable-standard-class-do-not-inherit-exported-slots)
 (:subclasses-of-method-combination-do-not-inherit-exported-slots)
 (:subclasses-of-standard-accessor-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-class-do-not-inherit-exported-slots)
 (:subclasses-of-standard-direct-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-effective-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-generic-function-do-not-inherit-exported-slots)
 (:subclasses-of-standard-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-reader-method-do-not-inherit-exported-slots)
 (:subclasses-of-standard-slot-definition-do-not-inherit-exported-slots)
 (:subclasses-of-standard-writer-method-do-not-inherit-exported-slots))
