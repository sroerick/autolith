(in-package #:autolith)

;;;; -- Resource Conditions --

(define-condition resource-uri-malformed (autolith-error)
  ((uri
    :initarg :uri
    :reader resource-uri-malformed-uri
    :type t
    :documentation "The value rejected as a resource URI.")
   (reason
    :initarg :reason
    :reader resource-uri-malformed-reason
    :type non-empty-string
    :documentation "The specific URI grammar violation."))
  (:default-initargs :message "The resource URI is malformed.")
  (:documentation "A resource URI does not satisfy the simple scheme:identifier grammar.")
  (:report (lambda (condition stream)
             (format stream "Malformed resource URI ~S: ~A"
                     (resource-uri-malformed-uri condition)
                     (resource-uri-malformed-reason condition)))))

(define-condition resource-scheme-unknown (autolith-error)
  ((uri
    :initarg :uri
    :reader resource-scheme-unknown-uri
    :type non-empty-string
    :documentation "The complete URI whose scheme has no registered resolver.")
   (scheme
    :initarg :scheme
    :reader resource-scheme-unknown-scheme
    :type non-empty-string
    :documentation "The syntactically valid unregistered scheme."))
  (:default-initargs :message "The resource URI scheme is not registered.")
  (:documentation "No resolver is registered for a syntactically valid resource URI.")
  (:report (lambda (condition stream)
             (format stream "No resource resolver is registered for scheme ~S in ~S."
                     (resource-scheme-unknown-scheme condition)
                     (resource-scheme-unknown-uri condition)))))

(define-condition resource-operation-unsupported (autolith-error)
  ((uri
    :initarg :uri
    :reader resource-operation-unsupported-uri
    :type non-empty-string
    :documentation "The URI of the resource that rejected the operation.")
   (operation
    :initarg :operation
    :reader resource-operation-unsupported-operation
    :type keyword
    :documentation "The unsupported resource protocol operation."))
  (:default-initargs :message "The resource does not support this operation.")
  (:documentation "A resource does not implement a requested protocol operation.")
  (:report (lambda (condition stream)
             (format stream "Resource ~S does not support ~S."
                     (resource-operation-unsupported-uri condition)
                     (resource-operation-unsupported-operation condition)))))

(define-condition resource-revision-stale (autolith-error)
  ((uri
    :initarg :uri
    :reader resource-revision-stale-uri
    :type non-empty-string
    :documentation "The URI whose observed revision is stale.")
   (expected-revision
    :initarg :expected-revision
    :reader resource-revision-stale-expected-revision
    :type non-empty-string
    :documentation "The revision supplied by the operation caller.")
   (actual-revision
    :initarg :actual-revision
    :reader resource-revision-stale-actual-revision
    :type (option non-empty-string)
    :documentation "The current revision, or NIL when the resource no longer exists."))
  (:default-initargs :message "The observed resource revision is stale.")
  (:documentation "A revision-gated resource operation no longer matches current state.")
  (:report (lambda (condition stream)
             (format stream "Resource ~S changed from revision ~S to ~S."
                     (resource-revision-stale-uri condition)
                     (resource-revision-stale-expected-revision condition)
                     (resource-revision-stale-actual-revision condition)))))


;;;; -- Resource Protocol --

(defclass resource ()
  ((uri
    :initarg :uri
    :reader resource-uri
    :type non-empty-string
    :documentation "The stable URI identifying this resource, not authority to access it."))
  (:documentation "An authority-neutral resource resolved for one explicit context."))

(defclass resource-observation ()
  ((uri
    :initarg :uri
    :reader resource-observation-uri
    :type non-empty-string
    :documentation "The stable URI identifying the observed resource.")
   (revision
    :initarg :revision
    :reader resource-observation-revision
    :type non-empty-string
    :documentation "The opaque revision identifying this observed state.")
   (content
    :initarg :content
    :reader resource-observation-content
    :type t
    :documentation "The resource-specific content visible in this observation.")
   (metadata
    :initarg :metadata
    :initform nil
    :reader resource-observation-metadata
    :type list
    :documentation "Resource-specific metadata associated with the observation."))
  (:documentation "A revisioned observation whose identity is separate from its content."))

(-> resource-capabilities (resource t) list)
(defgeneric resource-capabilities (resource context)
  (:documentation
   "Return RESOURCE operations available under explicit authority CONTEXT."))

(defmethod resource-capabilities ((resource resource) context)
  "Report no operations for the abstract RESOURCE protocol object."
  (declare (ignore resource context))
  nil)

(-> resource-observe (resource t) resource-observation)
(defgeneric resource-observe (resource context)
  (:documentation
   "Observe RESOURCE under explicit authority CONTEXT and return a revisioned value."))

(defmethod resource-observe ((resource resource) context)
  "Reject observation when RESOURCE has no concrete observation method."
  (declare (ignore context))
  (error 'resource-operation-unsupported
         :uri       (resource-uri resource)
         :operation ':observe))

(-> resource-apply-operations
    (resource t &key (:base-revision non-empty-string) (:operations list))
    resource-observation)
(defgeneric resource-apply-operations
    (resource context &key base-revision operations)
  (:documentation
   "Apply OPERATIONS to RESOURCE at BASE-REVISION under explicit authority CONTEXT."))

(defmethod resource-apply-operations
    ((resource resource) context &key base-revision operations)
  "Reject mutation when RESOURCE has no concrete operation method."
  (declare (ignore context base-revision operations))
  (error 'resource-operation-unsupported
         :uri       (resource-uri resource)
         :operation ':apply-operations))


;;;; -- URI Resolution --

(-> resource-uri--scheme-character-p (character boolean) boolean)
(defun resource-uri--scheme-character-p (character initial-p)
  "Return true when CHARACTER is valid at this position in a resource scheme."
  (and (or (and (char>= character #\a)
                (char<= character #\z))
           (and (not initial-p)
                (or (digit-char-p character)
                    (find character "+.-" :test #'char=))))
       t))

(-> resource-uri--valid-scheme-p (string) boolean)
(defun resource-uri--valid-scheme-p (scheme)
  "Return true when SCHEME satisfies the strict resource scheme grammar."
  (and (plusp (length scheme))
       (loop for character across scheme
             for initial-p = t then nil
             always (resource-uri--scheme-character-p character initial-p))))

(-> resource-uri--identifier-character-p (character) boolean)
(defun resource-uri--identifier-character-p (character)
  "Return true when CHARACTER is allowed in a simple resource identifier."
  (and (graphic-char-p character)
       (not (find character '(#\Space #\Tab #\Newline #\Return #\Page)))))

(-> resource-uri-parse (t) (values non-empty-string non-empty-string))
(defun resource-uri-parse (uri)
  "Parse strict lowercase SCHEME:IDENTIFIER URI and return both components."
  (unless (stringp uri)
    (error 'resource-uri-malformed
           :uri    uri
           :reason "the URI must be a string"))
  (let ((separator (position #\: uri)))
    (unless separator
      (error 'resource-uri-malformed
             :uri    uri
             :reason "the URI must contain a scheme separator"))
    (when (zerop separator)
      (error 'resource-uri-malformed
             :uri    uri
             :reason "the scheme must not be empty"))
    (when (= separator (1- (length uri)))
      (error 'resource-uri-malformed
             :uri    uri
             :reason "the identifier must not be empty"))
    (let ((scheme     (subseq uri 0 separator))
          (identifier (subseq uri (1+ separator))))
      (unless (resource-uri--valid-scheme-p scheme)
        (error 'resource-uri-malformed
               :uri    uri
               :reason "the scheme must use lowercase URI scheme characters"))
      (unless (every #'resource-uri--identifier-character-p identifier)
        (error 'resource-uri-malformed
               :uri    uri
               :reason "the identifier must contain only graphic non-space characters"))
      (values scheme identifier))))

(defmethod initialize-instance :after ((resource resource) &key)
  "Validate RESOURCE URI identity after initialization."
  (resource-uri-parse (resource-uri resource)))

(defmethod initialize-instance :after ((observation resource-observation) &key)
  "Validate OBSERVATION URI identity after initialization."
  (resource-uri-parse (resource-observation-uri observation)))

(defclass resource-resolver ()
  ((scheme
    :initarg :scheme
    :reader resource-resolver-scheme
    :type non-empty-string
    :documentation "The strict lowercase URI scheme handled by this resolver."))
  (:documentation "A CLOS resolver for one resource URI scheme."))

(defmethod initialize-instance :after ((resolver resource-resolver) &key)
  "Validate RESOLVER's scheme after initialization."
  (unless (resource-uri--valid-scheme-p (resource-resolver-scheme resolver))
    (error 'resource-uri-malformed
           :uri    (resource-resolver-scheme resolver)
           :reason "the resolver scheme must use lowercase URI scheme characters")))

(-> resource-resolver-resolve (resource-resolver non-empty-string t) resource)
(defgeneric resource-resolver-resolve (resolver identifier context)
  (:documentation
   "Resolve IDENTIFIER with RESOLVER under explicit authority CONTEXT."))

(defmethod resource-resolver-resolve
    ((resolver resource-resolver) identifier context)
  "Reject resolution when RESOLVER has no concrete method."
  (declare (ignore context))
  (error 'resource-operation-unsupported
         :uri       (format nil "~A:~A"
                            (resource-resolver-scheme resolver)
                            identifier)
         :operation ':resolve))

(defclass resource-registry ()
  ((resolvers
    :initform (make-hash-table :test #'equal)
    :reader resource-registry-resolvers
    :type hash-table
    :documentation "Strict URI schemes mapped to their active resolver objects."))
  (:documentation "A per-agent registry of resource resolver objects."))

(-> make-resource-registry () resource-registry)
(defun make-resource-registry ()
  "Return an empty resource resolver registry."
  (make-instance 'resource-registry))

(-> resource-registry-register
    (resource-registry resource-resolver)
    (values resource-resolver (option resource-resolver)))
(defun resource-registry-register (registry resolver)
  "Register RESOLVER by scheme, replacing and returning any previous resolver."
  (let ((scheme (resource-resolver-scheme resolver)))
    (unless (resource-uri--valid-scheme-p scheme)
      (error 'resource-uri-malformed
             :uri    scheme
             :reason "the resolver scheme must use lowercase URI scheme characters"))
    (multiple-value-bind (previous present-p)
        (gethash scheme (resource-registry-resolvers registry))
      (setf (gethash scheme (resource-registry-resolvers registry)) resolver)
      (values resolver (and present-p previous)))))

(-> resource-registry-resolve (resource-registry t t) resource)
(defun resource-registry-resolve (registry uri context)
  "Resolve URI through REGISTRY while passing explicit authority CONTEXT."
  (multiple-value-bind (scheme identifier)
      (resource-uri-parse uri)
    (let ((resolver (gethash scheme (resource-registry-resolvers registry))))
      (unless resolver
        (error 'resource-scheme-unknown
               :uri    uri
               :scheme scheme))
      (resource-resolver-resolve resolver identifier context))))
