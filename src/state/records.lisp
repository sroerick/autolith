(in-package #:autolith)

;;;; -- Private Readable State --

(-> readable-state-property-present-p (list keyword) boolean)
(defun readable-state-property-present-p (properties property)
  "Return true when PROPERTIES contains PROPERTY as a property-list key."
  (not (null (loop for tail on properties by #'cddr
                   thereis (eq (first tail) property)))))


(-> readable-state-lock-pathname (pathname string) pathname)
(defun readable-state-lock-pathname (state-pathname lock-name)
  "Return the sibling LOCK-NAME used to serialize access to STATE-PATHNAME."
  (merge-pathnames lock-name
                   (uiop:pathname-directory-pathname state-pathname)))
