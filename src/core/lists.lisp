(in-package #:autolith)

;;;; -- Finite Lists and Property Schemas --

(eval-when (:compile-toplevel :load-toplevel :execute)
  (-> proper-list-p (t &key (:nonempty-p boolean)) boolean)
  (defun proper-list-p (value &key nonempty-p)
    "Return true for a finite proper list, requiring an element when NONEMPTY-P."
    (and (if nonempty-p (consp value) (listp value))
         (handler-case
             (integerp (list-length value))
           (type-error ()
             nil))))

  (-> plist-schema-problem
      (t &key (:allowed-keys list) (:required-keys list)
              (:keyword-keys-p boolean) (:maximum-length (option integer)))
      (values (option keyword) t))
  (defun plist-schema-problem
      (properties &key (allowed-keys nil allowed-keys-p) required-keys
                       (keyword-keys-p t) maximum-length)
    "Return a schema problem and its key, or two NIL values for valid PROPERTIES.

Problems are :IMPROPER, :ODD, :TOO-LONG, :NON-KEYWORD, :UNKNOWN, :DUPLICATE
and :MISSING. Key validation follows property order; required keys follow
REQUIRED-KEYS order. An omitted ALLOWED-KEYS accepts any keyword key."
    (block nil
      (unless (proper-list-p properties)
        (return (values ':improper nil)))
      (unless (evenp (length properties))
        (return (values ':odd nil)))
      (when (and maximum-length (> (length properties) maximum-length))
        (return (values ':too-long nil)))
      (let ((seen (make-hash-table :test #'eq)))
        (loop for key in properties by #'cddr
              do (when (and keyword-keys-p (not (keywordp key)))
                   (return-from plist-schema-problem (values ':non-keyword key)))
                 (when (and allowed-keys-p (not (member key allowed-keys :test #'eq)))
                   (return-from plist-schema-problem (values ':unknown key)))
                 (when (gethash key seen)
                   (return-from plist-schema-problem (values ':duplicate key)))
                 (setf (gethash key seen) t))
        (dolist (key required-keys)
          (unless (gethash key seen)
            (return-from plist-schema-problem (values ':missing key)))))
      (values nil nil)))

  (-> plist-schema-p
      (t &key (:allowed-keys list) (:required-keys list)
              (:keyword-keys-p boolean) (:maximum-length (option integer)))
      boolean)
  (defun plist-schema-p (properties &rest options &key allowed-keys required-keys
                                                    keyword-keys-p maximum-length)
    "Return true when PROPERTIES satisfies the supplied schema options."
    (declare (ignore allowed-keys required-keys keyword-keys-p maximum-length))
    (null (apply #'plist-schema-problem properties options))))
