(in-package #:autolith)

;;;; -- Private Readable State --

(-> readable-state-lock-pathname (pathname string) pathname)
(defun readable-state-lock-pathname (state-pathname lock-name)
  "Return the sibling LOCK-NAME used to serialize access to STATE-PATHNAME."
  (merge-pathnames lock-name
                   (uiop:pathname-directory-pathname state-pathname)))
