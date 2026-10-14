(in-package #:autolith)

;;;; -- Versioned Onboarding Acknowledgements --

(defparameter *onboarding-state-page-limit* 4096
  "Maximum number of distinct onboarding pages in one state file.")

(defparameter *onboarding-state-identifier-limit* 256
  "Maximum character count of one onboarding page identifier.")

(deftype onboarding-state-status ()
  "The two durable ways to acknowledge a page revision."
  '(member :completed :refused))

(define-condition onboarding-state-error (autolith-error)
  ((pathname :initarg :pathname :reader onboarding-state-error-pathname
             :documentation "The onboarding state pathname.")
   (operation :initarg :operation :reader onboarding-state-error-operation
              :documentation "The operation that failed.")
   (cause :initarg :cause :initform nil :reader onboarding-state-error-cause
          :documentation "The underlying storage condition, if present."))
  (:documentation "Onboarding state could not be validated or persisted."))

(-> onboarding-state-path (configuration) pathname)
(defun onboarding-state-path (configuration)
  "Return CONFIGURATION's single durable onboarding snapshot pathname."
  (merge-pathnames "onboarding.sexp" (config :state-root configuration)))

(-> onboarding-state--list-p (t integer) boolean)
(defun onboarding-state--list-p (value limit)
  "Return true for a proper list of at most LIMIT elements, including NIL."
  (loop repeat (1+ limit)
        do (cond
             ((null value)
              (return-from onboarding-state--list-p t))
             ((consp value)
              (setf value (rest value)))
             (t
              (return-from onboarding-state--list-p nil))))
  nil)

(-> onboarding-state--page-p (t &key (:persisted-p boolean)) boolean)
(defun onboarding-state--page-p (page &key persisted-p)
  "Validate a bounded PAGE identity, and its exact durable shape if persisted."
  (and (sexp-store:plist-schema-p
        page :required-keys (if persisted-p '(:id :version :status) '(:id :version))
        :maximum-length 128)
       (or (not persisted-p)
           (and (= (length page) 6)
                (typep (getf page :status) 'onboarding-state-status)))
       (let ((identifier (getf page :id)))
         (and (stringp identifier)
              (<= 1 (length identifier) *onboarding-state-identifier-limit*)))
       (typep (getf page :version) '(integer 1 2147483647))
       t))

(-> onboarding-state--pages-p (t &key (:persisted-p boolean)) boolean)
(defun onboarding-state--pages-p (pages &key persisted-p)
  "Return true for bounded PAGES with unique identifiers and valid revisions."
  (and (onboarding-state--list-p pages *onboarding-state-page-limit*)
       (let ((seen (make-hash-table :test #'equal)))
         (every (lambda (page)
                  (and (onboarding-state--page-p page :persisted-p persisted-p)
                       (let ((identifier (getf page :id)))
                         (unless (gethash identifier seen)
                           (setf (gethash identifier seen) t)))))
                pages))
       t))

(-> onboarding-state--form-p (t) boolean)
(defun onboarding-state--form-p (form)
  "Accept only complete schema version 1 onboarding records."
  (and (onboarding-state--list-p form 5)
       (= (length form) 5)
       (eq (first form) ':onboarding)
       (eq (second form) ':version)
       (eql (third form) 1)
       (eq (fourth form) ':pages)
       (onboarding-state--pages-p (fifth form) :persisted-p t)
       t))

(-> onboarding-state--store (configuration) sexp-store:snapshot-store)
(defun onboarding-state--store (configuration)
  "Construct a locked snapshot store with explicit corrupt-state recovery.
RESET-ONBOARDING-STATE treats an unreadable snapshot as empty for this operation.
A read alone never publishes that reset; a successful record replaces the file."
  (let ((pathname (onboarding-state-path configuration))
        (reset-p nil))
    (make-instance
     'sexp-store:snapshot-store
     :pathname pathname
     :lock-pathname (readable-state-lock-pathname pathname "onboarding.lock")
     :initial-state #'list
     :validator #'onboarding-state--form-p
     :decoder #'fifth
     :encoder (lambda (pages) (list ':onboarding ':version 1 ':pages pages))
     :recover-read-error
     (lambda (cause)
       (unless reset-p
         (restart-case
             (error 'onboarding-state-error
                    :message "Could not read supported onboarding state."
                    :pathname pathname :operation ':read :cause cause)
           (reset-onboarding-state ()
             :report "Treat the unreadable onboarding state as empty for this operation."
             (setf reset-p t))))
       nil))))

(-> onboarding-state--call (configuration function) t)
(defun onboarding-state--call (configuration function)
  "Call FUNCTION with a fresh store, translating failures and permitting retry."
  (loop
    (restart-case
        (return
          (handler-case
              (funcall function (onboarding-state--store configuration))
            (sexp-store:store-error (cause)
              (error 'onboarding-state-error
                     :message (sexp-store:store-error-message cause)
                     :pathname (onboarding-state-path configuration)
                     :operation (sexp-store:store-error-operation cause)
                     :cause cause))))
      (retry-onboarding-state ()
        :report "Retry the onboarding state operation from disk."))))

(-> onboarding-state--check-pages (configuration t) null)
(defun onboarding-state--check-pages (configuration pages)
  "Signal a domain condition unless PAGES is a bounded collection of identities."
  (unless (onboarding-state--pages-p pages)
    (error 'onboarding-state-error
           :message "Invalid onboarding page identities."
           :pathname (onboarding-state-path configuration) :operation ':validate))
  nil)

(-> onboarding-state-pending (configuration list) list)
(defun onboarding-state-pending (configuration pages)
  "Return PAGES whose versions have not been completed or refused, in input order.
Page objects and opaque lesson fields are returned unchanged. Missing state
acknowledges nothing. Invalid or unsupported state signals ONBOARDING-STATE-ERROR."
  (onboarding-state--check-pages configuration pages)
  (onboarding-state--call
   configuration
   (lambda (store)
     (let ((acknowledged (sexp-store:store-read store)))
       (remove-if
        (lambda (page)
          (let ((previous (find (getf page :id) acknowledged
                                :key (lambda (entry) (getf entry :id))
                                :test #'string=)))
            (and previous (>= (getf previous :version) (getf page :version)))))
        pages)))))

(-> onboarding-state-record (configuration list onboarding-state-status) null)
(defun onboarding-state-record (configuration pages status)
  "Atomically acknowledge PAGES as STATUS, merging fresh process-shared state.
Only identity, version and status are persisted. Versions never decrease;
completion wins over refusal at equal versions. Unrelated pages are preserved.
Record completed slides individually during onboarding; interruptions need no mark."
  (onboarding-state--check-pages configuration pages)
  (unless (typep status 'onboarding-state-status)
    (error 'onboarding-state-error
           :message "Invalid onboarding acknowledgement status."
           :pathname (onboarding-state-path configuration) :operation ':validate))
  (onboarding-state--call
   configuration
   (lambda (store)
     (sexp-store:store-transact
      store
      (lambda (current)
        (dolist (page pages)
          (let* ((identifier (getf page :id))
                 (version (getf page :version))
                 (previous (find identifier current :test #'string=
                                 :key (lambda (entry) (getf entry :id)))))
            (when (or (not previous)
                      (> version (getf previous :version))
                      (and (= version (getf previous :version))
                           (eq status ':completed)))
              (setf current
                    (cons (list :id (copy-seq identifier) :version version :status status)
                          (remove previous current :test #'eq))))))
        (unless (onboarding-state--pages-p current :persisted-p t)
          (error 'onboarding-state-error
                 :message "Onboarding state exceeds the page limit."
                 :pathname (onboarding-state-path configuration) :operation ':validate))
        (values (sort current #'string< :key (lambda (entry) (getf entry :id)))
                nil t)))))
  nil)
