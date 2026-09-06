(in-package #:cl-user)

(require :sb-posix)
(require :asdf)

(defvar *check-script-library-mode* nil
  "Bind true when loading CLI helpers without running the command.")

(let ((*check-script-library-mode* t))
  (load (merge-pathnames "check.lisp"
                         (uiop:pathname-directory-pathname *load-truename*))))

(defun test-worker-main (arguments source-root)
  "Run one assigned shard and atomically publish its portable result."
  (handler-case
      (progn
        (unless (= (length arguments) 2)
          (check--fail "Usage: test-worker.lisp REQUEST RESULT"))
        (let* ((request (check--read-single-form (first arguments)))
               (result-path (pathname (second arguments)))
               (staging-path (make-pathname :type "pending" :defaults result-path)))
          (unless (and (check--plist-p request '(:version :cases))
                       (eql (getf request :version) 1)
                       (check--proper-list-p (getf request :cases))
                       (getf request :cases)
                       (every #'stringp (getf request :cases))
                       (= (length (getf request :cases))
                          (length (remove-duplicates (getf request :cases)
                                                     :test #'string-equal))))
            (check--fail "Invalid test worker request."))
          (check--load-tests source-root)
          (let* ((names (getf request :cases))
                 (cases (uiop:symbol-call '#:autolith '#:tests-select :tests names)))
            (unless (equal (mapcar #'string-downcase cases) names)
              (check--fail "Worker selection does not match its assigned cases."))
            (let ((result (uiop:symbol-call '#:autolith '#:tests-run-cases cases)))
              (check--validate-result result names)
              (unwind-protect
                   (progn
                     (check--write-form staging-path result)
                     (uiop:rename-file-overwriting-target staging-path result-path))
                (when (probe-file staging-path)
                  (delete-file staging-path)))
              (if (getf result :failures) 1 0)))))
    (error (condition)
      (format *error-output* "~&Test worker failed: ~A~%" condition)
      2)))

(unless *check-script-library-mode*
  (uiop:quit
   (test-worker-main (uiop:command-line-arguments)
                     (uiop:pathname-parent-directory-pathname
                      (uiop:pathname-directory-pathname (truename *load-truename*))))))