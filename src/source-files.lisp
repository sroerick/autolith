(in-package #:autolith)

;;;; -- Source File Discovery --

(-> source-lisp-pathnames (pathname) list)
(defun source-lisp-pathnames (directory)
  "Return sorted Lisp source pathnames recursively beneath DIRECTORY."
  (labels ((collect-pathnames (current-directory)
             "Return Lisp files beneath CURRENT-DIRECTORY in arbitrary order."
             (append
              (uiop:directory-files current-directory "*.lisp")
              (mapcan #'collect-pathnames
                      (uiop:subdirectories current-directory)))))
    (sort (collect-pathnames (uiop:ensure-directory-pathname directory))
          #'string<
          :key #'namestring)))
