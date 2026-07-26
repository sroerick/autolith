(in-package #:autolith)

;;;; -- macOS Seatbelt Backend --

(defparameter *seatbelt-command* #P"/usr/bin/sandbox-exec"
  "The macOS launcher used to apply a Seatbelt sandbox profile.")

(-> seatbelt-available-p () boolean)
(defun seatbelt-available-p ()
  "Return true when this host provides the macOS Seatbelt launcher."
  (and (member :darwin *features*)
       (not (null (probe-file *seatbelt-command*)))))

(-> seatbelt--unavailable (string keyword) null)
(defun seatbelt--unavailable (message capability)
  "Signal that Seatbelt cannot enforce CAPABILITY, reporting MESSAGE."
  (error 'sandbox-unavailable
         :message message
         :capability capability))

(-> seatbelt--canonical-directory (pathname) pathname)
(defun seatbelt--canonical-directory (path)
  "Return PATH as an absolute canonical directory when it exists."
  (let ((directory (uiop:ensure-directory-pathname path)))
    (or (ignore-errors
          (uiop:ensure-directory-pathname (truename directory)))
        directory)))

(-> seatbelt--path-string (pathname) string)
(defun seatbelt--path-string (path)
  "Return PATH as a Seatbelt subpath string without a trailing slash."
  (let ((namestring (namestring path)))
    (if (string= namestring "/")
        namestring
        (string-right-trim '(#\/) namestring))))

(-> seatbelt--quoted-string (string) string)
(defun seatbelt--quoted-string (text)
  "Return TEXT quoted and escaped for a Seatbelt profile string."
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across text
          do (when (or (char= character #\\)
                       (char= character #\"))
               (write-char #\\ stream))
             (write-char character stream))
    (write-char #\" stream)))

(-> seatbelt--subpath-filters (list) string)
(defun seatbelt--subpath-filters (paths)
  "Return Seatbelt subpath filters for PATHS."
  (with-output-to-string (stream)
    (dolist (path paths)
      (format stream
              " (subpath ~A)"
              (seatbelt--quoted-string
               (seatbelt--path-string path))))))

(-> seatbelt--metadata-paths (list list) list)
(defun seatbelt--metadata-paths (workspace-roots names)
  "Return protected metadata directories NAMES beneath WORKSPACE-ROOTS."
  (loop for root in workspace-roots
        append
        (loop for name in names
              collect
              (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "~A/" name) root)))))

(-> seatbelt--policy-profile (sandbox-policy) string)
(defun seatbelt--policy-profile (policy)
  "Translate restricted POLICY into a macOS Seatbelt profile.

Seatbelt uses last-match-wins rules, so protected metadata denies are emitted
only after every broader workspace and temporary-directory write allowance."
  (unless (eq (sandbox-policy-filesystem-kind policy) ':restricted)
    (seatbelt--unavailable
     "Seatbelt only translates restricted command sandbox policies."
     ':filesystem-policy))
  (when (eq (sandbox-policy-network policy) ':proxy-only)
    (seatbelt--unavailable
     "Seatbelt cannot enforce a proxy-only network policy."
     ':network-policy))
  (let ((workspace-roots
          (mapcar #'seatbelt--canonical-directory
                  (sandbox-policy-workspace-roots policy)))
        (read-root-p nil)
        (writable-paths nil))
    (dolist (rule (sandbox-policy-filesystem-rules policy))
      (unless (eq (filesystem-rule-kind rule) ':special)
        (seatbelt--unavailable
         "Seatbelt currently supports Autolith's special filesystem rules only."
         ':filesystem-policy))
      (let ((path (filesystem-rule-path rule))
            (access (filesystem-rule-access rule)))
        (case path
          (:root
           (unless (eq access ':read)
             (seatbelt--unavailable
              "Seatbelt requires the host root to remain read-only."
              ':filesystem-policy))
           (setf read-root-p t))
          (:workspace-roots
           (unless (eq access ':write)
             (seatbelt--unavailable
              "Seatbelt only supports writable workspace roots."
              ':filesystem-policy))
           (setf writable-paths
                 (append writable-paths
                         (if (filesystem-rule-subpath rule)
                             (mapcar
                              (lambda (root)
                                (uiop:ensure-directory-pathname
                                 (merge-pathnames
                                  (filesystem-rule-subpath rule)
                                  root)))
                              workspace-roots)
                             workspace-roots))))
          (:tmpdir
           (unless (eq access ':write)
             (seatbelt--unavailable
              "Seatbelt only supports a writable temporary directory."
              ':filesystem-policy))
           (push (seatbelt--canonical-directory (uiop:temporary-directory))
                 writable-paths))
          (:slash-tmp
           (unless (eq access ':write)
             (seatbelt--unavailable
              "Seatbelt only supports writable /tmp access."
              ':filesystem-policy))
           (push (seatbelt--canonical-directory #P"/tmp/")
                 writable-paths))
          (otherwise
           (seatbelt--unavailable
            (format nil "Seatbelt cannot translate special path ~S." path)
            ':filesystem-policy)))))
    (unless read-root-p
      (seatbelt--unavailable
       "Seatbelt command policies must grant read-only host access."
       ':filesystem-policy))
    (setf writable-paths
          (remove-duplicates writable-paths :test #'equal))
    (let ((metadata-paths
            (seatbelt--metadata-paths
             workspace-roots
             (sandbox-policy-protected-metadata-names policy))))
      (with-output-to-string (stream)
        (format stream
                "(version 1)~%(deny default)~%~
                 (allow file-read*)~%~
                 (allow file-read* file-write* (subpath \"/dev\"))~%~
                 (allow process*)~%~
                 (allow signal)~%~
                 (allow sysctl-read)~%~
                 (allow mach-lookup)~%")
        (when writable-paths
          (format stream
                  "(allow file-write*~A)~%"
                  (seatbelt--subpath-filters writable-paths)))
        (when metadata-paths
          (format stream
                  "(deny file-write*~A)~%"
                  (seatbelt--subpath-filters metadata-paths)))
        (format stream
                "(~A network*)~%"
                (ecase (sandbox-policy-network policy)
                  (:enabled "allow")
                  (:isolated "deny")))))))

(-> seatbelt-run-sandboxed
    (string list
     &key
     (:policy sandbox-policy)
     (:working-directory pathname)
     (:timeout real)
     (:merge-output-p boolean))
    sandbox-result)
(defun seatbelt-run-sandboxed
    (program arguments
     &key policy working-directory timeout merge-output-p)
  "Run PROGRAM and ARGUMENTS under POLICY through macOS Seatbelt."
  (unless (seatbelt-available-p)
    (seatbelt--unavailable
     "The macOS Seatbelt launcher is unavailable."
     ':platform-backend))
  (run-sandboxed
   (namestring *seatbelt-command*)
   (append (list "-p" (seatbelt--policy-profile policy) program)
           arguments)
   :policy (external-sandbox-policy)
   :working-directory working-directory
   :timeout timeout
   :merge-output-p merge-output-p))

(-> command-sandbox-run
    (string list
     &key
     (:policy sandbox-policy)
     (:working-directory pathname)
     (:timeout real)
     (:merge-output-p boolean))
    sandbox-result)
(defun command-sandbox-run
    (program arguments
     &key policy working-directory timeout merge-output-p)
  "Run PROGRAM under POLICY using the host command sandbox backend."
  (if (and (member :darwin *features*)
           (eq (sandbox-policy-filesystem-kind policy) ':restricted))
      (seatbelt-run-sandboxed
       program arguments
       :policy policy
       :working-directory working-directory
       :timeout timeout
       :merge-output-p merge-output-p)
      (run-sandboxed
       program arguments
       :policy policy
       :working-directory working-directory
       :timeout timeout
       :merge-output-p merge-output-p)))
