(in-package #:autolith)

;;;; -- Persistent Command Permissions --

(defparameter *permissions-version* 1
  "The readable command permission file format version.")

(defclass command-permission ()
  ((command
    :initarg :command
    :reader command-permission-command
    :type non-empty-string
    :documentation "The exact shell command approved by the user.")
   (directory
    :initarg :directory
    :reader command-permission-directory
    :type non-empty-string
    :documentation "The canonical working directory in which COMMAND is approved."))
  (:documentation "One exact persistent shell command approval."))

(defclass permission-state ()
  ((rules
    :initarg :rules
    :initform nil
    :accessor permission-state-rules
    :type list
    :documentation "The exact command and working-directory approvals."))
  (:documentation "Validated persistent command approvals for one user."))

(-> permissions--directory-name ((or pathname string)) string)
(defun permissions--directory-name (directory)
  "Return DIRECTORY as a canonical directory namestring."
  (let ((existing (uiop:directory-exists-p directory)))
    (unless existing
      (error 'permissions-error
             :message (format nil "Permission directory ~A does not exist."
                              directory)
             :pathname (pathname directory)
             :operation ':validate
             :cause nil))
    (namestring (uiop:ensure-directory-pathname (truename existing)))))

(-> permissions--rule-form-p (t) boolean)
(defun permissions--rule-form-p (form)
  "Return true when FORM is one exact command permission record."
  (and (listp form)
       (= (length form) 4)
       (eq (first form) ':command)
       (non-empty-string-p (second form))
       (eq (third form) ':directory)
       (non-empty-string-p (fourth form))))

(-> permissions--form-p (t) boolean)
(defun permissions--form-p (form)
  "Return true when FORM is one complete supported permission state."
  (handler-case
      (and (listp form)
           (= (length form) 5)
           (eq (first form) ':permissions)
           (eq (second form) ':version)
           (= (third form) *permissions-version*)
           (eq (fourth form) ':rules)
           (listp (fifth form))
           (every #'permissions--rule-form-p (fifth form)))
    (error ()
      nil)))

(-> permissions--form->state (list) permission-state)
(defun permissions--form->state (form)
  "Return the permission state represented by validated FORM."
  (make-instance
   'permission-state
   :rules
   (loop for rule in (fifth form)
         collect (make-instance 'command-permission
                                :command (copy-seq (second rule))
                                :directory (copy-seq (fourth rule))))))

(-> permissions--read (configuration) permission-state)
(defun permissions--read (configuration)
  "Read CONFIGURATION's command permissions or return an empty state."
  (block nil
    (let ((pathname (configuration-permissions-path configuration)))
      (unless (probe-file pathname)
        (return (make-instance 'permission-state)))
      (handler-case
          (multiple-value-bind (form sole-form-p)
              (snapshot-read pathname)
            (unless (and sole-form-p (permissions--form-p form))
              (error 'permissions-error
                     :message (format nil
                                      "Command permissions at ~A are malformed or unsupported."
                                      pathname)
                     :pathname pathname
                     :operation ':read
                     :cause nil))
            (permissions--form->state form))
        (permissions-error (condition)
          (error condition))
        (error (cause)
          (error 'permissions-error
                 :message (format nil "Could not read command permissions at ~A: ~A"
                                  pathname cause)
                 :pathname pathname
                 :operation ':read
                 :cause cause))))))

(-> permissions-load (configuration) permission-state)
(defun permissions-load (configuration)
  "Return saved command permissions, warning and denying after corruption."
  (handler-case
      (permissions--read configuration)
    (permissions-error (condition)
      (warn 'permissions-load-warning
            :pathname (permissions-error-pathname condition)
            :cause condition)
      (make-instance 'permission-state))))

(-> permissions--state-form (permission-state) list)
(defun permissions--state-form (state)
  "Return STATE as one portable readable form."
  (list :permissions
        :version *permissions-version*
        :rules
        (loop for rule in (permission-state-rules state)
              collect (list :command
                            (command-permission-command rule)
                            :directory
                            (command-permission-directory rule)))))

(-> permissions--write (configuration permission-state) null)
(defun permissions--write (configuration state)
  "Atomically persist command permission STATE with private file permissions."
  (let ((pathname (configuration-permissions-path configuration)))
    (handler-case
        (snapshot-write pathname (permissions--state-form state))
      (error (cause)
        (error 'permissions-error
               :message (format nil "Could not persist command permissions at ~A: ~A"
                                pathname cause)
               :pathname pathname
               :operation ':write
               :cause cause))))
  nil)

(-> permissions-allowed-p
    (permission-state string (or pathname string))
    boolean)
(defun permissions-allowed-p (state command directory)
  "Return true when exact COMMAND is permanently approved in DIRECTORY."
  (let ((directory-name (permissions--directory-name directory)))
    (not
     (null
      (find-if (lambda (rule)
                 (and (string= command (command-permission-command rule))
                      (string= directory-name
                               (command-permission-directory rule))))
               (permission-state-rules state))))))

(-> permissions-allow
    (&key
     (:configuration configuration)
     (:state permission-state)
     (:command string)
     (:directory (or pathname string)))
    null)
(defun permissions-allow (&key configuration state command directory)
  "Permanently approve exact COMMAND in DIRECTORY unless already present."
  (unless (non-empty-string-p command)
    (error 'permissions-error
           :message "Cannot approve an empty shell command."
           :pathname (configuration-permissions-path configuration)
           :operation ':validate
           :cause nil))
  (unless (permissions-allowed-p state command directory)
    (let* ((rule (make-instance 'command-permission
                                :command (copy-seq command)
                                :directory
                                (permissions--directory-name directory)))
           (rules (append (permission-state-rules state) (list rule)))
           (replacement (make-instance 'permission-state :rules rules)))
      (permissions--write configuration replacement)
      (setf (permission-state-rules state) rules)))
  nil)

(-> permissions-clear (configuration permission-state) null)
(defun permissions-clear (configuration state)
  "Remove every permanently approved shell command."
  (let ((replacement (make-instance 'permission-state)))
    (permissions--write configuration replacement)
    (setf (permission-state-rules state) nil))
  nil)


;;;; -- Automatic Command Classification --

(defparameter *permissions-auto-safe-commands*
  '("basename" "cat" "cmp" "date" "diff" "dirname" "echo" "false"
    "file" "git" "grep" "head" "ls" "md5sum" "printf" "pwd" "realpath"
    "rg" "sha256sum" "stat" "tail" "test" "true" "uname" "wc" "which")
  "Commands that auto mode may allow inside the workspace sandbox when they stay read-only.")

(defparameter *permissions-auto-safe-git-subcommands*
  '("blame" "branch" "cat-file" "describe" "diff" "log" "ls-files"
    "ls-tree" "name-rev" "rev-parse" "show" "status" "symbolic-ref")
  "Git subcommands that auto mode treats as inspection rather than mutation.")

(defparameter *permissions-auto-deny-command-tokens*
  '("doas" "halt" "mkfs" "mkfs.ext4" "mkfs.xfs" "poweroff" "reboot"
    "shutdown" "su" "sudo")
  "Command tokens that auto mode refuses without asking.")

(-> permissions--command-tokens (string) list)
(defun permissions--command-tokens (command)
  "Return COMMAND split on whitespace for conservative classification."
  (remove ""
          (uiop:split-string (string-trim '(#\Space #\Tab #\Newline) command)
                             :separator '(#\Space #\Tab #\Newline))
          :test #'string=))

(-> permissions--command-contains-p (string list) boolean)
(defun permissions--command-contains-p (command needles)
  "Return true when COMMAND contains one of NEEDLES as a substring."
  (not
   (null
    (find-if (lambda (needle)
               (search needle command :test #'char-equal))
             needles))))

(-> permissions--token-equal-p (string string) boolean)
(defun permissions--token-equal-p (left right)
  "Return true when LEFT and RIGHT are the same command token."
  (string-equal left right))

(-> permissions--first-token (string) (option string))
(defun permissions--first-token (command)
  "Return COMMAND's first whitespace-delimited token, or NIL."
  (first (permissions--command-tokens command)))

(-> permissions--git-subcommand (string) (option string))
(defun permissions--git-subcommand (command)
  "Return the first non-option Git subcommand in COMMAND, or NIL."
  (let ((tokens (permissions--command-tokens command)))
    (when (and tokens (permissions--token-equal-p (first tokens) "git"))
      (find-if (lambda (token)
                 (not (uiop:string-prefix-p "-" token)))
               (rest tokens)))))

(-> permissions--composed-shell-p (string) boolean)
(defun permissions--composed-shell-p (command)
  "Return true when COMMAND composes another command or redirects output."
  (or (not (null (find #\Newline command :test #'char=)))
      (permissions--command-contains-p
       command
       '("|" "&&" ";" "`" "$(" "${" ">" ">>" "<"))))

(-> permissions--download-to-shell-p (string) boolean)
(defun permissions--download-to-shell-p (command)
  "Return true when COMMAND pipes a download into a shell interpreter."
  (and (permissions--command-contains-p command '("|"))
       (permissions--command-contains-p command '("curl" "wget" "fetch"))
       (permissions--command-contains-p command
                                        '("| sh" "|sh" "| bash" "|bash"
                                          "| zsh" "|zsh" "| dash" "|dash"))))

(-> permissions--catastrophic-p (string) boolean)
(defun permissions--catastrophic-p (command)
  "Return true when COMMAND is a privilege, wipe, or host-control action."
  (let ((first (permissions--first-token command))
        (tokens (permissions--command-tokens command)))
    (or (null first)
        (not
         (null
          (find first *permissions-auto-deny-command-tokens*
                :test #'permissions--token-equal-p)))
        (permissions--download-to-shell-p command)
        (permissions--command-contains-p command
                                         '("mkfs" "of=/dev" "if=/dev"
                                           ":(){" "/etc/" "/boot/"
                                           ".ssh/"))
        (and (stringp first)
             (permissions--token-equal-p first "dd")
             (or (not (null (find "of=/dev" tokens :test #'search)))
                 (not (null (find "if=/dev" tokens :test #'search))))))))

(-> permissions--safe-git-p (string) boolean)
(defun permissions--safe-git-p (command)
  "Return true when COMMAND is a read-only Git inspection."
  (let ((subcommand (permissions--git-subcommand command)))
    (and (non-empty-string-p subcommand)
         (not (permissions--composed-shell-p command))
         (not
          (null
           (find subcommand *permissions-auto-safe-git-subcommands*
                 :test #'permissions--token-equal-p)))
         (not (and (permissions--token-equal-p subcommand "branch")
                   (permissions--command-contains-p command
                                                    '("-D" "-d" "--delete")))))))

(-> permissions--safe-simple-p (string) boolean)
(defun permissions--safe-simple-p (command)
  "Return true when COMMAND is a simple sandbox-safe inspection command."
  (let ((first (permissions--first-token command)))
    (and (non-empty-string-p first)
         (not (permissions--composed-shell-p command))
         (not
          (null
           (find first *permissions-auto-safe-commands*
                 :test #'permissions--token-equal-p)))
         (or (not (permissions--token-equal-p first "git"))
             (permissions--safe-git-p command))
         (not (and (permissions--token-equal-p first "find")
                   (permissions--command-contains-p command
                                                    '("-delete" "-exec")))))))

(-> permissions-classify-command (string) (values keyword string))
(defun permissions-classify-command (command)
  "Classify COMMAND for auto permission mode.

Return `:sandboxed`, `:ask`, or `:deny`, never `:full-access`, plus a short
reason. Empty, privilege, wipe, and download-to-shell commands are denied.
Simple inspection stays sandboxed. Everything else asks."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) command)))
    (cond
      ((not (non-empty-string-p trimmed))
       (values ':deny "empty commands are refused"))
      ((permissions--catastrophic-p trimmed)
       (values ':deny "privilege, wipe, or host-control commands are refused"))
      ((permissions--safe-simple-p trimmed)
       (values ':sandboxed "safe inspection may run inside the workspace sandbox"))
      (t
       (values ':ask "this command needs a human decision")))))
