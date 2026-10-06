(require :asdf)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

(defun relocate-image-manifests--read (pathname)
  "Read exactly one portable manifest from PATHNAME."
  (with-open-file (stream pathname :external-format ':utf-8)
    (let* ((*read-eval* nil)
           (end (gensym "END"))
           (record (read stream nil end)))
      (unless (and (consp record) (eq (read stream nil end) end))
        (error "Invalid image manifest at ~A." pathname))
      record)))

(defun relocate-image-manifests (stage destination)
  "Record DESTINATION's core paths before publishing the complete STAGE directory."
  (let ((updates
          (loop for (directory name tag version) in
                '(("active/" "autolith-active.core" :sbcl-generations-image-manifest 1)
                  ("recovery/" "autolith-recovery.core" :recovery-image 2))
                for relative = (concatenate 'string directory name)
                for manifest = (merge-pathnames (concatenate 'string directory "manifest.sexp") stage)
                for record = (relocate-image-manifests--read manifest)
                do (unless (and (eq (first record) tag)
                                (eql (getf (rest record) :version) version)
                                (uiop:pathname-equal (getf (rest record) :core)
                                                     (merge-pathnames relative stage)))
                     (error "The staged manifest does not describe its core at ~A." manifest))
                collect (progn
                          (setf (getf (rest record) :core)
                                (namestring (merge-pathnames relative destination)))
                          (cons manifest record)))))
    (dolist (update updates)
      (let* ((manifest (first update))
             (temporary (make-pathname :name ".manifest-relocation" :type "tmp" :defaults manifest)))
        (unwind-protect
             (progn
               (with-open-file (stream temporary :direction ':output :if-exists ':error
                                                :external-format ':utf-8)
                 (let ((*print-readably* t) (*print-pretty* nil))
                   (prin1 (rest update) stream)
                   (terpri stream)))
               (autolith-script-set-file-mode temporary #o444)
               (autolith-script-replace-file temporary manifest))
          (when (probe-file temporary)
            (delete-file temporary)))))))

(let ((arguments (uiop:command-line-arguments)))
  (unless (= (length arguments) 2)
    (error "Usage: relocate-image-manifests.lisp STAGE DESTINATION"))
  (let ((stage (uiop:ensure-directory-pathname (first arguments)))
        (destination (uiop:ensure-directory-pathname (second arguments))))
    (unless (and (uiop:absolute-pathname-p stage) (uiop:absolute-pathname-p destination))
      (error "Image publication directories must be absolute."))
    (relocate-image-manifests stage destination)))
