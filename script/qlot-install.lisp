(require :asdf)

(let* ((script-path (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root (uiop:pathname-parent-directory-pathname script-directory))
       (quicklisp-setup (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
  (unless (probe-file quicklisp-setup)
    (error "Autolith bootstrap needs Quicklisp at ~A" quicklisp-setup))
  (load quicklisp-setup)
  (uiop:symbol-call '#:ql '#:quickload :cffi :silent t)
  (let ((profile-library-directory
          (merge-pathnames ".guix-profile/lib/" (user-homedir-pathname)))
        (library-directories
          (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI")))
    (when (probe-file profile-library-directory)
      (pushnew profile-library-directory
               (symbol-value library-directories)
               :test #'equal)))
  (uiop:symbol-call '#:ql '#:quickload :qlot :silent t)
    (let ((qlot-project-root (find-symbol "*PROJECT-ROOT*" "QLOT")))
      (unless qlot-project-root
        (error "The loaded Qlot does not expose its project root."))
      (progv (list qlot-project-root) (list source-root)
        (uiop:with-current-directory (source-root)
          (if (member :bsd *features*)
              (uiop:symbol-call '#:qlot '#:install :jobs 1)
                (uiop:symbol-call '#:qlot '#:install))))))
