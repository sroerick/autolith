(in-package #:autolith)

;;;; -- Preloaded Active Image Tests --

(-> test-active-image-build-record () null)
(defun test-active-image-build-record ()
  "Test exact source identity, runtime compatibility, and manifest projection."
  (let* ((source-root (asdf:system-source-directory :autolith))
         (record (active-image-build-record-create source-root))
         (source-files (getf (rest record) :source-files))
         (probe (active-image-probe-record record))
         (manifest (active-image-manifest-form
                    #P"/tmp/autolith-active-test.core"
                    record)))
    (test-assert (active-image-build-record-p record)
                 "active-image build records are complete portable data")
    (test-assert (active-image-build-record-compatible-p record source-root)
                 "the active-image record matches its exact source and runtime")
    (test-assert (equal (mapcar #'first source-files)
                        (active-image-source-paths source-root))
                 "active-image identities cover every compiled source input")
    (test-assert (and (eq (first probe) :autolith-active-image)
                      (= (getf (rest probe) :version)
                         *active-image-protocol-version*))
                 "the active-image probe exposes the current protocol")
    (test-assert (and (eq (first manifest) :active-image)
                      (equal (getf (rest manifest) :source-files)
                             source-files))
                 "active-image manifests retain exact source identities")
    (let ((wrong-source (copy-tree record)))
      (setf (second (first (getf (rest wrong-source) :source-files)))
            "0000000000000000000000000000000000000000")
      (test-assert
       (not (active-image-build-record-compatible-p wrong-source source-root))
       "active-image compatibility rejects a changed source blob"))
    (let ((wrong-runtime (copy-tree record)))
      (setf (getf (rest wrong-runtime) :sbcl-version) "0.0.0")
      (test-assert
       (not (active-image-build-record-compatible-p wrong-runtime source-root))
       "active-image compatibility rejects another SBCL runtime"))
    (let ((wrong-os-version (copy-tree record)))
      (setf (getf (rest wrong-os-version) :operating-system-version) "0.0.0")
      (test-assert
       (not (active-image-build-record-compatible-p wrong-os-version source-root))
       "active-image compatibility rejects another OS version")))
  nil)

(-> test-active-image-process-command () null)
(defun test-active-image-process-command ()
  "Test fresh Autolith processes boot a matching active core and fall back to source."
  (with-test-configuration (configuration root)
    (let* ((core (merge-pathnames "active/autolith-active.core" root))
           (configuration (configuration-copy configuration :active-image-core core))
           (source-root (config :source-root configuration))
           (record (active-image-build-record-create source-root)))
      (flet ((command ()
               "Return the fresh-process argv for one worker argument."
               (active-image-process-command configuration '("--worker")))
             (install (record)
               "Install an empty core whose manifest names RECORD."
               (ensure-directories-exist core)
               (with-open-file (stream core :direction ':output
                                            :if-exists ':supersede
                                            :if-does-not-exist ':create)
                 (write-string "core" stream))
               (snapshot-write (merge-pathnames "manifest.sexp" core)
                               (active-image-manifest-form core record))))
        (test-assert (and (member "--script" (command) :test #'string=)
                          (string= "--worker" (car (last (command)))))
                     "without an installed core the process loads from source")
        (install record)
        (test-assert (equal (rest (command))
                            (list "--noinform" "--core" (namestring core)
                                  "--end-runtime-options" (namestring source-root)
                                  "--worker"))
                     "a core whose manifest matches the source boots directly")
        (let ((stale (copy-tree record)))
          (setf (second (first (getf (rest stale) :source-files)))
                "0000000000000000000000000000000000000000")
          (install stale)
          (test-assert (member "--script" (command) :test #'string=)
                       "a core built from other source is never booted"))
        (snapshot-write (merge-pathnames "manifest.sexp" core) '(:active-image :version 1))
        (test-assert (member "--script" (command) :test #'string=)
                     "an incomplete manifest falls back to source"))))
  nil)

(-> test-image-commit-surface-battery () null)
(defun test-image-commit-surface-battery ()
  "Test the replay surface battery passes live and names missing pieces."
  (test-assert (null (image-commit-surface-verify))
               "the live image passes its own surface battery")
  (let ((*image-commit-surface-functions*
          (list (gensym "MISSING-SURFACE-FUNCTION-"))))
    (test-assert
     (handler-case
         (progn
           (image-commit-surface-verify)
           nil)
       (image-commit-error (condition)
         (and (eq (image-commit-error-stage condition) ':surface-battery)
              (search "missing-surface-function"
                      (autolith-error-message condition)))))
     "a missing core definition fails the battery and is named"))
  (let ((*image-commit-surface-classes* (list ':not-a-class-name)))
    (test-assert
     (handler-case
         (progn
           (image-commit-surface-verify)
           nil)
       (image-commit-error (condition)
         (not (null (search "not-a-class-name"
                            (autolith-error-message condition))))))
     "a missing core class fails the battery and is named"))
  nil)

(-> test-image-commit--write-probe-commit (pathname string list) pathname)
(defun test-image-commit--write-probe-commit (script identifier entries)
  "Write ENTRIES as commit IDENTIFIER's SCRIPT beside a manifest naming it."
  (let ((title (format nil "Multiline metadata~%(error \"Executed title.\")")))
    (image-commit-write-script script :identifier identifier :title title :entries entries)
    (image-commit--write-form-atomically
     (merge-pathnames "manifest.sexp" (uiop:pathname-directory-pathname script))
     (image-commit--manifest-form :identifier                    identifier
                                  :title                         title
                                  :source-commit                 nil
                                  :script-pathname               script
                                  :entries                       entries
                                  :consumed-mutation-identifiers nil
                                  :journal-position              0
                                  :created-at                    (get-universal-time)))
    script))

(-> test-image-commit-replay-probe () null)
(defun test-image-commit-replay-probe ()
  "Test clean-process loading and rejection of private replay scripts."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (identifier (make-identifier))
         (script (merge-pathnames "probe/reconstruct.lisp" root)))
    (unwind-protect
         (progn
           (test-image-commit--write-probe-commit
            script identifier
            (list
             (list :kind ':definition :id "generic"
                   :target "(defgeneric image-commit-test-operation)"
                   :source "(defgeneric image-commit-test-operation (value))")
             (list :kind ':definition
                   :id (format nil "method~%(error \"Executed identifier.\")")
                   :target (format nil "(defmethod image-commit-test-operation nil~%  (integer))")
                   :source "(defmethod image-commit-test-operation ((value integer)) (+ value 7))")
             (list :kind ':legacy :id "assertion" :target "result"
                   :source "(assert (= 42 (image-commit-test-operation 35)))")))
           (test-assert
            (null (image-commit-replay-probe configuration script identifier))
            "generated replay executes methods without evaluating multiline metadata")
           (platform-delete-directory-tree *platform* (uiop:pathname-directory-pathname script)
                                           :validate t)
           (test-image-commit--write-probe-commit
            script identifier
            (list
             (list :kind ':definition :id "stale" :tracked nil
                   :target "(defun image-commit-replay-probe-output)"
                   :source "(defun image-commit-replay-probe-output (identifier) identifier \"stale\")")))
           (test-assert
            (null (image-commit-replay-probe configuration script identifier))
            "the probe skips a stale definition that startup would skip")
           (delete-file script)
           (with-open-file (stream script
                                   :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create
                                   :external-format ':utf-8)
             (format stream "(in-package #:autolith)~%(error \"Broken replay.\")~%"))
           (test-assert
            (handler-case
                (progn
                  (image-commit-replay-probe configuration script identifier)
                  nil)
              (image-commit-error (condition)
                (and (eq (image-commit-error-stage condition) ':replay-probe)
                     (search "Broken replay."
                             (autolith-error-message condition)))))
            "a rejected replay script carries the probe output in its error")
           (let ((log-pathname
                   (merge-pathnames
                    "replay-probe.log"
                    (uiop:pathname-directory-pathname script))))
             (test-assert
              (and (probe-file log-pathname)
                   (search "Broken replay."
                           (uiop:read-file-string log-pathname)))
              "a rejected replay probe persists its complete output beside the script")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
