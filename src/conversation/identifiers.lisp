(in-package #:autolith)

;;;; -- Conversation Identifier Format --

(-> conversation-identifier-normalize (t) string)
(defun conversation-identifier-normalize (value)
  "Return VALUE as a canonical stored identifier, accepting its display form."
  (handler-case
      (identifier-normalize value)
    (identifier-error ()
      (error 'conversation-identifier-error
             :message
             "A conversation identifier must contain seven case-sensitive Bitcoin Base58 characters, with an optional hyphen after the first."
             :value value))))

(-> conversation-identifier-display (string) string)
(defun conversation-identifier-display (identifier)
  "Return IDENTIFIER with its visual hyphen, retaining legacy values verbatim."
  (handler-case
      (identifier-display identifier)
    (identifier-error ()
      identifier)))

(-> conversation-identifier-path-fragment (string) (option string))
(defun conversation-identifier-path-fragment (identifier)
  "Return canonical IDENTIFIER unchanged for case-sensitive private paths."
  (and (identifier-p identifier) identifier))


;;;; -- Conversation Identifier Allocation --

(-> conversation-identifier--pathname (pathname string) pathname)
(defun conversation-identifier--pathname (storage-root identifier)
  "Return IDENTIFIER's conversation file pathname beneath STORAGE-ROOT."
  (merge-pathnames (make-pathname :name identifier :type "sexp")
                   storage-root))

(-> conversation-identifier--occupied-p (pathname string) boolean)
(defun conversation-identifier--occupied-p (storage-root identifier)
  "Return true when IDENTIFIER already has a conversation file beneath STORAGE-ROOT."
  (not (null (probe-file (conversation-identifier--pathname storage-root
                                                            identifier)))))

(-> conversation-identifier--reserved-p (pathname string) boolean)
(defun conversation-identifier--reserved-p (storage-root identifier)
  "Return true when IDENTIFIER is occupied or reserved beneath STORAGE-ROOT."
  (let ((root (uiop:ensure-directory-pathname storage-root)))
    (or (conversation-identifier--occupied-p root identifier)
        (identifier-reserved-p identifier :namespace (namestring root)))))

(-> conversation-identifier-generate
    (pathname &key (:timestamp timestamp) (:reserved-identifiers list))
    string)
(defun conversation-identifier-generate
    (storage-root &key (timestamp (get-universal-time)) reserved-identifiers)
  "Allocate one stored identifier for TIMESTAMP beneath STORAGE-ROOT.

The identifier stays reserved for the life of this process. Its conversation
file is written after allocation returns, so releasing the reservation earlier
would let a second allocation in the same second choose the same identifier."
  (let ((root (uiop:ensure-directory-pathname storage-root)))
    (handler-case
        (identifier-generate
         :timestamp timestamp
         :namespace (namestring root)
         :reserved-identifiers reserved-identifiers
         :occupied-p (lambda (candidate)
                       (conversation-identifier--occupied-p root candidate)))
      (identifier-space-exhausted ()
        (error 'conversation-identifier-space-exhausted
               :message
               (format nil
                       "All conversation identifier seeds are occupied for Universal Time ~D."
                       timestamp)
               :pathname root
               :sequence nil
               :timestamp timestamp))
      (identifier-error (condition)
        (error 'conversation-identifier-error
               :message (identifier-error-message condition)
               :value timestamp)))))


;;;; -- Conversation Picker Sidecar Paths --

(-> conversation-picker-metadata-pathname (pathname) pathname)
(defun conversation-picker-metadata-pathname (conversation-pathname)
  "Return the compact picker-cache pathname for CONVERSATION-PATHNAME."
  (let* ((conversation-root
           (uiop:pathname-directory-pathname conversation-pathname))
         (data-root
           (uiop:pathname-parent-directory-pathname conversation-root)))
    (merge-pathnames
     (make-pathname :name (pathname-name conversation-pathname) :type "sexp")
     (merge-pathnames "conversation-picker/" data-root))))


(-> conversation-picker-revision-pathname (pathname) pathname)
(defun conversation-picker-revision-pathname (conversation-pathname)
  "Return the durable picker-cache revision pathname for CONVERSATION-PATHNAME."
  (make-pathname :type "revision"
                 :defaults
                 (conversation-picker-metadata-pathname conversation-pathname)))


(-> conversation-picker-search-pathname (pathname) pathname)
(defun conversation-picker-search-pathname (conversation-pathname)
  "Return the durable message-search pathname for CONVERSATION-PATHNAME."
  (let ((metadata-pathname
          (conversation-picker-metadata-pathname conversation-pathname)))
    (make-pathname :name (format nil "~A.search"
                                 (pathname-name metadata-pathname))
                   :type "sexp"
                   :defaults metadata-pathname)))


(-> conversation-picker-search-revision-pathname (pathname) pathname)
(defun conversation-picker-search-revision-pathname (conversation-pathname)
  "Return the durable message-search revision path for CONVERSATION-PATHNAME."
  (make-pathname :type "revision"
                 :defaults
                 (conversation-picker-search-pathname conversation-pathname)))


(-> conversation-picker-sidecar-pathnames (pathname) list)
(defun conversation-picker-sidecar-pathnames (conversation-pathname)
  "Return every picker sidecar pathname owned by CONVERSATION-PATHNAME."
  (list (conversation-picker-metadata-pathname conversation-pathname)
        (conversation-picker-revision-pathname conversation-pathname)
        (conversation-picker-search-pathname conversation-pathname)
        (conversation-picker-search-revision-pathname conversation-pathname)))
