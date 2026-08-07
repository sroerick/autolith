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
