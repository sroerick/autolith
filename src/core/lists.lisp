(in-package #:autolith)

;;;; -- Finite Lists --

(eval-when (:compile-toplevel :load-toplevel :execute)
  (-> proper-list-p (t &key (:nonempty-p boolean)) boolean)
  (defun proper-list-p (value &key nonempty-p)
    "Return true for a finite proper list, requiring an element when NONEMPTY-P."
    (and (if nonempty-p (consp value) (listp value))
         (handler-case
             (integerp (list-length value))
           (type-error ()
            nil)))))
