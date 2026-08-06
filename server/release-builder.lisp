(in-package #:autolith)

;;;; -- Release Builder Configuration --

(defparameter *release-builder-default-poll-seconds* 300
  "The default delay between remote release-tag checks.")

(defparameter *release-builder-default-repository*
  "https://github.com/luciusmagn/autolith.git"
  "The public source repository inspected for release tags.")

(defparameter *release-builder-container-image*
  "autolith-release-builder:ubuntu-22.04"
  "The local container image used for portable release builds.")

(defparameter *release-builder-default-container-timeout-seconds* 1800
  "The maximum runtime of one isolated release build and check.")

(defvar *release-builder-command-function* #'uiop:run-program
  "The command runner used by the release builder and replaced by tests.")

(defclass release-builder-configuration ()
  ((source-root
    :initarg :source-root
    :reader release-builder-configuration-source-root
    :type pathname
    :documentation "The deployed checkout containing the builder container files.")
   (state-root
    :initarg :state-root
    :reader release-builder-configuration-state-root
    :type pathname
    :documentation "The private root for checkouts, caches, and staging files.")
   (public-root
    :initarg :public-root
    :reader release-builder-configuration-public-root
    :type pathname
    :documentation "The root receiving complete atomic release publications.")
   (repository
    :initarg :repository
    :reader release-builder-configuration-repository
    :type string
    :documentation "The Git repository queried for release tags.")
   (poll-seconds
    :initarg :poll-seconds
    :reader release-builder-configuration-poll-seconds
    :type (integer 1)
    :documentation "The delay after each remote-tag scan.")
   (container-command
    :initarg :container-command
    :reader release-builder-configuration-container-command
    :type string
    :documentation "The container engine executable used for isolated builds.")
   (container-timeout-seconds
    :initarg :container-timeout-seconds
    :reader release-builder-configuration-container-timeout-seconds
    :type (integer 1)
    :documentation "The maximum seconds allowed for one container build and check."))
  (:documentation "Private settings for the automatic release builder."))

(defclass release-source-tag ()
  ((name
    :initarg :name
    :reader release-source-tag-name
    :type string
    :documentation "The semantic Git tag name.")
   (commit
    :initarg :commit
    :reader release-source-tag-commit
    :type string
    :documentation "The exact forty-character Git commit identity."))
  (:documentation "One immutable remote source tag selected for building."))

(define-condition release-builder-error (error)
  ((stage
    :initarg :stage
    :reader release-builder-error-stage
    :type keyword
    :documentation "The builder stage that failed.")
   (tag
    :initarg :tag
    :initform nil
    :reader release-builder-error-tag
    :type (option string)
    :documentation "The release tag being built, when one was selected.")
   (cause
    :initarg :cause
    :reader release-builder-error-cause
    :type t
    :documentation "The underlying failure condition or diagnostic value."))
  (:report
   (lambda (condition stream)
     (format stream "Release builder failed during ~(~A~)~@[ for ~A~]: ~A"
             (release-builder-error-stage condition)
             (release-builder-error-tag condition)
             (release-builder-error-cause condition))))
  (:documentation "A structured automatic release build failure."))

(-> release-builder--positive-integer
    ((option string) integer string)
    (integer 1))
(defun release-builder--positive-integer (value fallback name)
  "Parse positive integer VALUE, using FALLBACK when absent and naming NAME."
  (if (null value)
      fallback
      (handler-case
          (let ((parsed (parse-integer value :junk-allowed nil)))
            (unless (plusp parsed)
              (error 'parse-error))
            parsed)
        (error ()
          (error 'configuration-error
                 :message (format nil "Invalid ~A ~S." name value))))))

(-> release-host--call-with-lock (pathname function) t)
(defun release-host--call-with-lock (state-root function)
  "Call FUNCTION while holding the release host's process-shared operation lock."
  (let* ((pathname
           (merge-pathnames "host-operation.lock"
                            (uiop:ensure-directory-pathname state-root)))
         (descriptor nil))
    (ensure-directories-exist pathname)
    (unwind-protect
         (progn
           (setf descriptor
                 (sb-posix:open
                  (namestring pathname)
                  (logior sb-posix:o-creat sb-posix:o-rdwr)
                  #o600))
           (sb-posix:lockf descriptor sb-posix:f-lock 0)
           (funcall function))
      (when descriptor
        (ignore-errors (sb-posix:lockf descriptor sb-posix:f-ulock 0))
        (ignore-errors (sb-posix:close descriptor))))))

(-> release-builder-configuration-create
    (&key (:source-root (option pathname))
          (:state-root (option pathname))
          (:public-root (option pathname))
          (:repository (option string))
          (:poll-seconds (option integer))
          (:container-command (option string))
          (:container-timeout-seconds (option integer)))
    release-builder-configuration)
(defun release-builder-configuration-create
    (&key source-root state-root public-root repository poll-seconds
          container-command container-timeout-seconds)
  "Create automatic release builder settings from arguments and environment."
  (make-instance
   'release-builder-configuration
   :source-root
   (uiop:ensure-directory-pathname
    (or source-root
        (let ((configured (uiop:getenv "AUTOLITH_RELEASE_SOURCE_ROOT")))
          (and configured (pathname configured)))
        (asdf:system-source-directory :autolith)))
   :state-root
   (uiop:ensure-directory-pathname
    (or state-root
        (let ((configured (uiop:getenv "AUTOLITH_RELEASE_STATE_ROOT")))
          (and configured (pathname configured)))
        #p"/var/lib/autolith-release-server/"))
   :public-root
   (uiop:ensure-directory-pathname
    (or public-root
        (let ((configured (uiop:getenv "AUTOLITH_RELEASE_PUBLIC_ROOT")))
          (and configured (pathname configured)))
        #p"/srv/autolith-release-server/"))
   :repository (or repository
                   (uiop:getenv "AUTOLITH_RELEASE_REPOSITORY")
                   *release-builder-default-repository*)
   :poll-seconds
   (or poll-seconds
       (release-builder--positive-integer
        (uiop:getenv "AUTOLITH_RELEASE_POLL_SECONDS")
        *release-builder-default-poll-seconds*
        "release poll interval"))
   :container-command (or container-command
                          (uiop:getenv "AUTOLITH_RELEASE_CONTAINER_COMMAND")
                          "docker")
   :container-timeout-seconds
   (or container-timeout-seconds
       (release-builder--positive-integer
        (uiop:getenv "AUTOLITH_RELEASE_CONTAINER_TIMEOUT_SECONDS")
        *release-builder-default-container-timeout-seconds*
        "release container timeout"))))


;;;; -- Remote Tags --

(-> release-builder--commit-valid-p (string) boolean)
(defun release-builder--commit-valid-p (commit)
  "Return true when COMMIT is a lowercase forty-character Git identity."
  (and (= (length commit) 40)
       (not
        (null
         (every (lambda (character)
                  (or (digit-char-p character)
                      (find character "abcdef")))
                commit)))))

(-> release-builder--parse-remote-tags (string) list)
(defun release-builder--parse-remote-tags (output)
  "Parse Git ls-remote OUTPUT into semantic release source tags.

Annotated tags publish both the tag object and a peeled commit line ending in
^{}. Prefer the peeled commit so checkout validation matches HEAD after clone.
Lightweight tags only publish one line and keep that commit identity."
  (let ((commits (make-hash-table :test #'equal))
        (peeled (make-hash-table :test #'equal)))
    (dolist (line (uiop:split-string output :separator '(#\Newline #\Return)))
      (unless (zerop (length line))
        (let* ((separator (position #\Tab line))
               (commit (and separator (subseq line 0 separator)))
               (reference (and separator (subseq line (1+ separator))))
               (prefix "refs/tags/")
               (suffix "^{}")
               (tag nil)
               (peeled-p nil))
          (when (and reference (uiop:string-prefix-p prefix reference))
            (let ((name (subseq reference (length prefix))))
              (when (and (>= (length name) (length suffix))
                         (string= name suffix
                                  :start1 (- (length name) (length suffix))))
                (setf peeled-p t
                      name (subseq name 0 (- (length name) (length suffix)))))
              (setf tag name)))
          (unless (and commit
                       tag
                       (release-builder--commit-valid-p commit)
                       (release-tag-valid-p tag))
            (error 'release-builder-error
                   :stage ':tag-discovery
                   :cause (format nil "Malformed remote tag line ~S." line)))
          (if peeled-p
              (progn
                (setf (gethash tag commits) commit
                      (gethash tag peeled) t))
              (unless (gethash tag peeled)
                (setf (gethash tag commits) commit))))))
    (sort
     (loop for tag being the hash-keys of commits using (hash-value commit)
           collect (make-instance 'release-source-tag
                                  :name tag
                                  :commit commit))
     #'release-tag<
     :key #'release-source-tag-name)))

(-> release-builder-remote-tags (release-builder-configuration) list)
(defun release-builder-remote-tags (configuration)
  "Return semantic release tags currently published by the source remote."
  (handler-case
      (release-builder--parse-remote-tags
       (uiop:run-program
        ;; Omit --refs so annotated tags also emit peeled ^{} commit lines.
        (list "git" "ls-remote" "--tags"
              (release-builder-configuration-repository configuration)
              "v*.*.*")
        :output :string
        :error-output :output))
    (release-builder-error (condition)
      (error condition))
    (error (cause)
      (error 'release-builder-error
             :stage ':tag-discovery
             :cause cause))))

(-> release-builder-pending-tags
    (release-builder-configuration list)
    list)
(defun release-builder-pending-tags (configuration remote-tags)
  "Return REMOTE-TAGS newer than publication state and needing a build.

An empty publication root starts at the newest remote tag instead of rebuilding
historical releases. Once one release exists, every newer tag is built in
semantic order so a temporary builder outage cannot skip a version."
  (let* ((server-configuration
           (release-server-configuration-create
            :source-root (release-builder-configuration-source-root configuration)
            :public-root (release-builder-configuration-public-root configuration)))
         (published (release-server-published-tags server-configuration))
         (latest (first (last published))))
    (if latest
        (remove-if-not
         (lambda (source-tag)
           (and (release-tag< latest (release-source-tag-name source-tag))
                (not
                 (release-server--release-complete-p
                  server-configuration
                  (release-source-tag-name source-tag)))))
         remote-tags)
        (last remote-tags))))


;;;; -- Source Checkout --

(-> release-builder--checkout-path
    (release-builder-configuration release-source-tag)
    pathname)
(defun release-builder--checkout-path (configuration source-tag)
  "Return the private reusable checkout path for SOURCE-TAG."
  (merge-pathnames
   (format nil "checkouts/~A/" (release-source-tag-name source-tag))
   (release-builder-configuration-state-root configuration)))

(-> release-builder--git-output (pathname list) string)
(defun release-builder--git-output (directory arguments)
  "Return trimmed output from one Git command in DIRECTORY."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return)
   (uiop:run-program (append (list "git" "-C" (namestring directory)) arguments)
                     :output :string
                     :error-output :output)))

(-> release-builder--checkout-valid-p
    (pathname release-source-tag)
    boolean)
(defun release-builder--checkout-valid-p (checkout source-tag)
  "Return true when CHECKOUT is clean and exactly matches SOURCE-TAG."
  (handler-case
      (and (uiop:directory-exists-p checkout)
           (string= (release-builder--git-output checkout '("rev-parse" "HEAD"))
                    (release-source-tag-commit source-tag))
           (zerop
            (length
             (release-builder--git-output
              checkout '("status" "--porcelain" "--untracked-files=no")))))
    (error ()
      nil)))

(-> release-builder--prepare-checkout
    (release-builder-configuration release-source-tag)
    pathname)
(defun release-builder--prepare-checkout (configuration source-tag)
  "Return a clean exact checkout for SOURCE-TAG, cloning it when necessary."
  (let ((checkout (release-builder--checkout-path configuration source-tag)))
    (unless (release-builder--checkout-valid-p checkout source-tag)
      (when (uiop:directory-exists-p checkout)
        (uiop:delete-directory-tree checkout
                                    :validate t
                                    :if-does-not-exist :ignore))
      (ensure-directories-exist checkout)
      (uiop:delete-empty-directory checkout)
      (handler-case
          (uiop:run-program
           (list "git" "clone"
                 "--depth" "1"
                 "--branch" (release-source-tag-name source-tag)
                 "--single-branch"
                 (release-builder-configuration-repository configuration)
                 (namestring checkout))
           :output :interactive
           :error-output :interactive)
        (error (cause)
          (error 'release-builder-error
                 :stage ':checkout
                 :tag (release-source-tag-name source-tag)
                 :cause cause)))
      (unless (release-builder--checkout-valid-p checkout source-tag)
        (error 'release-builder-error
               :stage ':checkout
               :tag (release-source-tag-name source-tag)
               :cause "The cloned checkout does not match its remote tag.")))
    checkout))

(-> release-builder--source-version (pathname) string)
(defun release-builder--source-version (checkout)
  "Read the single ASDF version declared by CHECKOUT."
  (let ((prefix "  :version \"")
        (versions nil))
    (with-open-file (stream (merge-pathnames "autolith.asd" checkout)
                            :direction :input
                            :external-format :utf-8)
      (loop for line = (read-line stream nil nil)
            while line
            when (and (uiop:string-prefix-p prefix line)
                      (uiop:string-suffix-p line "\""))
              do (push (subseq line (length prefix) (1- (length line)))
                       versions)))
    (unless (and (= (length versions) 1)
                 (release-tag-valid-p (format nil "v~A" (first versions))))
      (error 'release-builder-error
             :stage ':source-validation
             :cause "The checkout does not declare one semantic ASDF version."))
    (first versions)))


;;;; -- Container Build --

(-> release-builder--container-image (release-builder-configuration) null)
(defun release-builder--container-image (configuration)
  "Build or refresh the pinned portable release-builder container image."
  (let* ((source-root
           (release-builder-configuration-source-root configuration))
         (server-root (merge-pathnames "server/" source-root))
         (containerfile (merge-pathnames "Containerfile" server-root)))
    (handler-case
        (uiop:run-program
         (list (release-builder-configuration-container-command configuration)
               "build"
               "--tag" *release-builder-container-image*
               "--file" (namestring containerfile)
               (namestring server-root))
         :output :interactive
         :error-output :interactive)
      (error (cause)
        (error 'release-builder-error
               :stage ':container-image
               :cause cause))))
  nil)

(-> release-builder--staging-path
    (release-builder-configuration release-source-tag)
    pathname)
(defun release-builder--staging-path (configuration source-tag)
  "Return SOURCE-TAG's private release output staging directory."
  (merge-pathnames
   (format nil "staging/~A/" (release-source-tag-name source-tag))
   (release-builder-configuration-state-root configuration)))

(-> release-builder--container-volume (pathname string) string)
(defun release-builder--container-volume (pathname target)
  "Return one Docker bind-mount argument from PATHNAME to TARGET."
  (format nil "~A:~A" (namestring (truename pathname)) target))

(-> release-builder--clear-shared-source-fasl-cache (pathname) null)
(defun release-builder--clear-shared-source-fasl-cache (state-root)
  "Delete shared /source ASDF fasls so successive tags cannot poison each other.

Every release checkout is bind-mounted at the same container path, /source.
ASDF therefore keys compiled fasls under one shared cache tree. Leaving that
tree intact lets one tag's object files fail a later tag's checks."
  (let ((cache-root (merge-pathnames "cache/common-lisp/" state-root)))
    (when (uiop:directory-exists-p cache-root)
      (dolist (implementation (uiop:subdirectories cache-root))
        (let ((source-cache (merge-pathnames "source/" implementation)))
          (when (uiop:directory-exists-p source-cache)
            (uiop:delete-directory-tree source-cache
                                        :validate t
                                        :if-does-not-exist :ignore)
            (format t "~&Cleared shared source fasl cache at ~A.~%"
                    source-cache)
            (finish-output))))))
  nil)

(-> release-builder--container-name (release-source-tag) string)
(defun release-builder--container-name (source-tag)
  "Return a unique managed container name for SOURCE-TAG."
  (format nil "autolith-release-builder-~A-~D"
          (release-source-tag-name source-tag)
          (sb-posix:getpid)))

(-> release-builder--container-diagnostics
    (release-builder-configuration string)
    null)
(defun release-builder--container-diagnostics (configuration container-name)
  "Print bounded process and log diagnostics for managed CONTAINER-NAME."
  (dolist (arguments
           (list (list "top" container-name
                       "-eo" "pid,ppid,pgid,sid,user,stat,wchan:32,etime,cmd")
                 (list "logs" "--tail" "200" container-name)))
    (ignore-errors
      (funcall *release-builder-command-function*
               (cons
                (release-builder-configuration-container-command configuration)
                arguments)
               :output ':interactive
               :error-output ':interactive)))
  nil)

(-> release-builder--remove-container
    (release-builder-configuration string)
    null)
(defun release-builder--remove-container (configuration container-name)
  "Force-remove managed CONTAINER-NAME after a failed or timed-out build."
  (ignore-errors
    (funcall *release-builder-command-function*
             (list (release-builder-configuration-container-command configuration)
                   "rm" "--force" container-name)
             :output ':interactive
             :error-output ':interactive))
  nil)

(-> release-builder--run-container
    (release-builder-configuration release-source-tag pathname)
    pathname)
(defun release-builder--run-container (configuration source-tag checkout)
  "Build and check SOURCE-TAG in the pinned container, returning its output."
  (let* ((state (merge-pathnames "container/"
                                 (release-builder-configuration-state-root
                                  configuration)))
         (staging (release-builder--staging-path configuration source-tag))
         (container-name (release-builder--container-name source-tag))
         (container-command
           (release-builder-configuration-container-command configuration)))
    (when (uiop:directory-exists-p staging)
      (uiop:delete-directory-tree staging
                                  :validate t
                                  :if-does-not-exist :ignore))
    (uiop:ensure-all-directories-exist (list state staging))
    (release-builder--clear-shared-source-fasl-cache state)
    (handler-case
        (funcall
         *release-builder-command-function*
         (list "timeout"
               "--signal=TERM"
               "--kill-after=30"
               (princ-to-string
                (release-builder-configuration-container-timeout-seconds
                 configuration))
               container-command
               "run" "--name" container-name "--rm" "--privileged"
               "--volume" (release-builder--container-volume checkout "/source")
               "--volume" (release-builder--container-volume state "/state")
               "--volume" (release-builder--container-volume staging "/output")
               *release-builder-container-image*)
         :output ':interactive
         :error-output ':interactive)
      (error (cause)
        (release-builder--container-diagnostics configuration container-name)
        (release-builder--remove-container configuration container-name)
        (error 'release-builder-error
               :stage ':container-build
               :tag (release-source-tag-name source-tag)
               :cause cause)))
    staging))


;;;; -- Atomic Publication --

(-> release-builder--artifact-pathnames
    (pathname string)
    (values pathname pathname))
(defun release-builder--artifact-pathnames (directory tag)
  "Return TAG's archive and checksum pathnames below DIRECTORY."
  (let ((archive (merge-pathnames (release-server--archive-name tag) directory)))
    (values archive
            (merge-pathnames
             (format nil "~A.sha256" (file-namestring archive))
             directory))))

(-> release-builder--validate-artifacts (pathname string) null)
(defun release-builder--validate-artifacts (directory tag)
  "Require DIRECTORY to contain exactly verifiable release artifacts for TAG."
  (multiple-value-bind (archive checksum)
      (release-builder--artifact-pathnames directory tag)
    (unless (and (uiop:file-exists-p archive)
                 (uiop:file-exists-p checksum)
                 (plusp
                  (with-open-file (stream archive :direction :input
                                                 :element-type '(unsigned-byte 8))
                    (file-length stream))))
      (error 'release-builder-error
             :stage ':artifact-validation
             :tag tag
             :cause "The build did not produce its archive and checksum."))
    (handler-case
        (uiop:run-program
         (list "sha256sum" "--check" "--status" (file-namestring checksum))
         :directory directory
         :output :interactive
         :error-output :interactive)
      (error (cause)
        (error 'release-builder-error
               :stage ':artifact-validation
               :tag tag
               :cause cause))))
  nil)

(-> release-builder-publish
    (release-builder-configuration pathname string)
    pathname)
(defun release-builder-publish (configuration staging tag)
  "Validate and atomically publish TAG from STAGING, returning its directory."
  (release-builder--validate-artifacts staging tag)
  (let* ((releases-root
           (merge-pathnames "releases/"
                            (release-builder-configuration-public-root
                             configuration)))
         (target (merge-pathnames (format nil "~A/" tag) releases-root))
         (temporary
           (merge-pathnames
            (format nil ".~A.~D/" tag (sb-posix:getpid))
            releases-root)))
    (when (uiop:directory-exists-p target)
      (error 'release-builder-error
             :stage ':publication
             :tag tag
             :cause "An incomplete publication directory already exists."))
    (when (uiop:directory-exists-p temporary)
      (uiop:delete-directory-tree temporary
                                  :validate t
                                  :if-does-not-exist :ignore))
    (uiop:ensure-all-directories-exist (list temporary))
    (multiple-value-bind (archive checksum)
        (release-builder--artifact-pathnames staging tag)
      (let ((published-archive
              (merge-pathnames (file-namestring archive) temporary))
            (published-checksum
              (merge-pathnames (file-namestring checksum) temporary)))
        (uiop:copy-file archive published-archive)
        (uiop:copy-file checksum published-checksum)
        (sb-posix:chmod (namestring published-archive) #o444)
        (sb-posix:chmod (namestring published-checksum) #o444)))
    (sb-posix:chmod (namestring temporary) #o555)
    (rename-file temporary target)
    (format t "~&Published Autolith ~A at ~A.~%" tag target)
    (finish-output)
    target))


;;;; -- Builder Loop --

(-> release-builder-build (release-builder-configuration release-source-tag) pathname)
(defun release-builder-build (configuration source-tag)
  "Build, verify, and publish one immutable SOURCE-TAG."
  (let* ((tag (release-source-tag-name source-tag))
         (checkout (release-builder--prepare-checkout configuration source-tag))
         (version (release-builder--source-version checkout)))
    (unless (string= tag (format nil "v~A" version))
      (error 'release-builder-error
             :stage ':source-validation
             :tag tag
             :cause (format nil "The checkout declares version ~A." version)))
    (release-builder--container-image configuration)
    (release-builder-publish
     configuration
     (release-builder--run-container configuration source-tag checkout)
     tag)))

(-> release-builder-build-pending (release-builder-configuration) list)
(defun release-builder-build-pending (configuration)
  "Build every newly discovered release and return its publication paths."
  (release-host--call-with-lock
   (release-builder-configuration-state-root configuration)
   (lambda ()
     (loop for source-tag in
           (release-builder-pending-tags
            configuration
            (release-builder-remote-tags configuration))
           collect (release-builder-build configuration source-tag)))))

(-> release-builder-run (release-builder-configuration) null)
(defun release-builder-run (configuration)
  "Poll forever, building and publishing newly tagged releases."
  (format t "~&Autolith release builder polling ~A every ~D seconds.~%"
          (release-builder-configuration-repository configuration)
          (release-builder-configuration-poll-seconds configuration))
  (finish-output)
  (loop
    (handler-case
        (release-builder-build-pending configuration)
      (error (condition)
        (format *error-output* "~&~A~%" condition)
        (finish-output *error-output*)))
    (sleep (release-builder-configuration-poll-seconds configuration))))
