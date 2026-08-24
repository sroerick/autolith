(in-package #:autolith)

;;;; -- Terminal Defaults --

(defparameter *terminal-default-columns* 80
  "The fallback terminal width when no positive width is supplied.")

(defparameter *terminal-default-rows* 24
  "The fallback terminal height when no positive height is supplied.")

(defparameter *terminal-history-limit* 100
  "The maximum number of submitted inputs retained by a line editor.")

(defparameter *terminal-ui-visible-completions* 6
  "The maximum number of candidate rows painted at once.")

(defparameter *terminal-escape-character* (code-char 27)
  "The ASCII escape character used by trusted terminal controls.")

(-> terminal-bracketed-paste-enable-sequence () string)
(defun terminal-bracketed-paste-enable-sequence ()
  "Return Clinedi's trusted bracketed-paste enable control."
  (with-output-to-string (stream)
    (enable-bracketed-paste :stream stream)))

(-> terminal-bracketed-paste-disable-sequence () string)
(defun terminal-bracketed-paste-disable-sequence ()
  "Return Clinedi's trusted bracketed-paste disable control."
  (with-output-to-string (stream)
    (disable-bracketed-paste :stream stream)))

(-> terminal-keyboard-enhancement-enable-sequence () string)
(defun terminal-keyboard-enhancement-enable-sequence ()
  "Return Clinedi's trusted keyboard-enhancement enable controls."
  (with-output-to-string (stream)
    (enable-keyboard-enhancement :stream stream)))

(-> terminal-keyboard-enhancement-disable-sequence () string)
(defun terminal-keyboard-enhancement-disable-sequence ()
  "Return Clinedi's trusted keyboard-enhancement disable controls."
  (with-output-to-string (stream)
    (disable-keyboard-enhancement :stream stream)))


;;;; -- Terminal Objects --

(defclass terminal ()
  ((rows
    :initarg :rows
    :initform *terminal-default-rows*
    :accessor terminal-rows
    :type (integer 1)
    :documentation "The current terminal height in character cells.")
   (columns
    :initarg :columns
    :initform *terminal-default-columns*
    :accessor terminal-columns
    :type (integer 1)
    :documentation "The current terminal width in character cells.")
   (interactive-p
    :initarg :interactive-p
    :initform nil
    :accessor terminal-interactive-p
    :type boolean
    :documentation "Whether this terminal currently accepts noncanonical input.")
   (styled-p
    :initarg :styled-p
    :initform nil
    :accessor terminal-styled-p
    :type boolean
    :documentation "Whether trusted output may include color and emphasis controls.")
   (started-p
    :initform nil
    :accessor terminal-started-p
    :type boolean
    :documentation "Whether this terminal has entered its active lifecycle."))
  (:documentation "A replaceable primary-screen terminal transport."))

(defclass stream-terminal (terminal)
  ((input-stream
    :initarg :input-stream
    :reader stream-terminal-input-stream
    :type stream
    :documentation "The character stream carrying terminal input.")
   (pending-input-stream
    :initform nil
    :accessor stream-terminal-pending-input-stream
    :type (or null stream)
    :documentation "Buffered terminal bytes awaiting semantic event decoding.")
   (output-stream
    :initarg :output-stream
    :reader stream-terminal-output-stream
    :type stream
    :documentation "The character stream receiving terminal output.")
   (input-file-descriptor
    :initarg :input-file-descriptor
    :reader stream-terminal-input-file-descriptor
    :type integer
    :documentation "The POSIX descriptor whose termios state is controlled.")
   (saved-terminal-mode
    :initform nil
    :accessor stream-terminal-saved-terminal-mode
    :type t
    :documentation "The exact termios value restored when the terminal stops."))
  (:documentation "An SBCL stream terminal backed by POSIX file descriptor zero."))

(defclass terminal-ui ()
  ((lock
    :initform (make-recursive-lock "Autolith terminal UI")
    :reader terminal-ui-lock
    :type t
    :documentation "The recursive lock serializing editor state and terminal writes.")
   (terminal
    :initarg :terminal
    :reader terminal-ui-terminal
    :type terminal
    :documentation "The primary-screen terminal transport.")
   (editor
    :initarg :editor
    :reader terminal-ui-editor
    :type line-editor
    :documentation "The Unicode-aware multiline user input editor.")
   (live-region
    :initarg :live-region
    :reader terminal-ui-live-region
    :type live-region
    :documentation "Clinedi region anchoring unfinished content below scrollback.")
   (prompt
    :initarg :prompt
    :reader terminal-ui-prompt
    :type string
    :documentation "The untrusted-text-safe prompt prefix.")
   (prompt-marker-state
    :initform ':closed
    :accessor terminal-ui-prompt-marker-state
    :type (member :closed :prompt :input :executing)
    :documentation "The current semantic OSC 133 prompt-block boundary.")
   (live-output-suspended-p
    :initform nil
    :accessor terminal-ui-live-output-suspended-p
    :type boolean
    :documentation "Whether transient live-region repaint is suspended for direct I/O.")
   (deferred-live-appended-text
    :initform ""
    :accessor terminal-ui-deferred-live-appended-text
    :type string
    :documentation "Plain scrollback deferred while direct terminal I/O owns the display.")
   (deferred-live-appended-display
    :initform ""
    :accessor terminal-ui-deferred-live-appended-display
    :type string
    :documentation "Styled scrollback deferred while direct terminal I/O owns the display.")
   (prompt-render-cache
    :initform nil
    :accessor terminal-ui-prompt-render-cache
    :type (option list)
    :documentation
    "The memoized prompt row, cursor offset, and their exact render inputs.")
   (placeholder
    :initarg :placeholder
    :initform ""
    :reader terminal-ui-placeholder
    :type string
    :documentation "The dim hint shown on the prompt row while input is empty.")
   (completions
    :initarg :completions
    :initform nil
    :reader terminal-ui-completions
    :type list
    :documentation "Completion entries offered while typing an interactive command.")
   (completion-function
    :initarg :completion-function
    :initform nil
    :accessor terminal-ui-completion-function
    :type (option function)
    :documentation
    "Optional function returning the current completion entries on demand.")
   (completion-selector
    :initarg :completion-selector
    :reader terminal-ui-completion-selector
    :type selector
    :documentation "Clinedi navigation state for matching command completions.")
   (completion-active-p
    :initform nil
    :accessor terminal-ui-completion-active-p
    :type boolean
    :documentation "Whether arrows and Tab are choosing among completion candidates.")
   (completion-prefix
    :initform nil
    :accessor terminal-ui-completion-prefix
    :type (option string)
    :documentation "Input restored when active completion selection is cancelled.")
   (completion-history-state
    :initform nil
    :accessor terminal-ui-completion-history-state
    :type (option list)
    :documentation
    "Clinedi history traversal state restored when completion is cancelled.")
   (completion-dismissed-p
    :initform nil
    :accessor terminal-ui-completion-dismissed-p
    :type boolean
    :documentation "Whether Escape has hidden passive completion suggestions.")
   (selector
    :initform nil
    :accessor terminal-ui-selector
    :type (option selector)
    :documentation "Clinedi navigation state for the active modal picker.")
   (selector-title
    :initform nil
    :accessor terminal-ui-selector-title
    :type (option string)
    :documentation "The application-owned title for the active modal picker.")
   (selector-hint
    :initform nil
    :accessor terminal-ui-selector-hint
    :type (option string)
    :documentation "Optional modal picker hint text after the title.")
   (status
    :initform nil
    :accessor terminal-ui-status
    :type (option string)
    :documentation "The optional unfinished activity shown above the prompt.")
   (compacting-p
    :initform nil
    :accessor terminal-ui-compacting-p
    :type boolean
    :documentation "Whether an indeterminate conversation compaction is active.")
   (compaction-started-at
    :initform nil
    :accessor terminal-ui-compaction-started-at
    :type (option real)
    :documentation "The monotonic time at which active compaction began.")
   (notice
    :initform nil
    :accessor terminal-ui-notice
    :type (option string)
    :documentation "The optional transient notice shown above the prompt.")
   (notice-deadline
    :initform nil
    :accessor terminal-ui-notice-deadline
    :type (option real)
    :documentation "The monotonic time at which the transient notice expires.")
   (status-details
    :initform nil
    :accessor terminal-ui-status-details
    :type list
    :documentation "Styled model, effort, and repository details beside the activity.")
   (status-started-at
    :initform nil
    :accessor terminal-ui-status-started-at
    :type (option real)
    :documentation "The monotonic time at which the current activity phase began.")
   (status-progress-at
    :initform nil
    :accessor terminal-ui-status-progress-at
    :type (option real)
    :documentation "The monotonic time of the newest progress within the activity phase.")
   (status-worked-seconds
    :initform nil
    :accessor terminal-ui-status-worked-seconds
    :type (option (integer 0))
    :documentation
    "The conversation's accumulated working seconds when the activity began.")
    (local-activity
     :initform nil
     :accessor terminal-ui-local-activity
     :type (option string)
     :documentation "Explicit local Lisp work shown below the animated status row.")
    (local-activity-started-at
     :initform nil
     :accessor terminal-ui-local-activity-started-at
     :type (option real)
     :documentation "The monotonic time at which explicit local Lisp work began.")
   (agent-activities
    :initform nil
    :accessor terminal-ui-agent-activities
    :type list
    :documentation
    "Sanitized queued and running child-agent summaries in stable display order.")
    (command-activities
     :initform nil
     :accessor terminal-ui-command-activities
     :type list
     :documentation
     "Sanitized queued and running primary command summaries in stable order.")
    (command-unpainted-identifiers
     :initform nil
     :accessor terminal-ui-command-unpainted-identifiers
     :type list
     :documentation
     "Current command identifiers that have not reached a reader-owned paint.")
    (command-pending-completions
     :initform nil
     :accessor terminal-ui-command-pending-completions
     :type list
     :documentation
     "Bounded completed command snapshots awaiting their first visible paint.")
   (status-rendered-signature
    :initform nil
    :accessor terminal-ui-status-rendered-signature
    :type list
    :documentation
    "The command and child-agent values used by the newest live paint.")
   (clock-function
    :initarg :clock-function
    :initform (lambda ()
                (/ (get-internal-real-time)
                   (coerce internal-time-units-per-second 'double-float)))
    :reader terminal-ui-clock-function
    :type function
    :documentation "The injected monotonic clock function returning seconds.")
   (preview-rows
    :initform nil
    :accessor terminal-ui-preview-rows
    :type list
    :documentation "Transient styled rows shown in the live region, never scrollback.")
   (queued-input-previews
    :initform nil
    :accessor terminal-ui-queued-input-previews
    :type list
    :documentation "Sanitized queued follow-up text shown in the live region.")
   (steering-input-previews
    :initform nil
    :accessor terminal-ui-steering-input-previews
    :type list
    :documentation "Sanitized steering text shown in the live region.")
   (image-attachments
    :initform nil
    :accessor terminal-ui-image-attachments
    :type list
    :documentation "Local image pathnames and labels attached to the current draft.")
   (image-history
    :initform nil
    :accessor terminal-ui-image-history
    :type list
    :documentation "Recent editor history text paired with its image attachments.")
   (stream-tail
    :initform nil
    :accessor terminal-ui-stream-tail
    :type (or null string list)
    :documentation "Unfinished streamed text, styled spans, or styled rows continuing the transcript block above.")
   (finalized-identifiers
    :initform (make-hash-table :test #'equal)
    :reader terminal-ui-finalized-identifiers
    :type hash-table
    :documentation "Identifiers whose finalized transcript text was already emitted.")
   (started-p
    :initform nil
    :accessor terminal-ui-started-p
    :type boolean
    :documentation "Whether the UI lifecycle has started."))
  (:documentation
   "A scrollback-preserving UI with immutable transcript output and a bounded live region."))


;;;; -- Terminal Conditions --

(define-condition terminal-error (autolith-error)
  ((operation
    :initarg :operation
    :reader terminal-error-operation
    :type keyword
    :documentation "The terminal operation that could not complete.")
   (cause
    :initarg :cause
    :reader terminal-error-cause
    :type (option condition)
    :documentation "The underlying implementation condition, when available."))
  (:documentation "A terminal mode, input, or output operation failed."))


;;;; -- Terminal Protocol --

(defgeneric terminal-start (terminal)
  (:documentation "Start TERMINAL without entering an alternate screen."))

(defgeneric terminal-stop (terminal)
  (:documentation "Restore TERMINAL input state and finish its lifecycle."))

(defgeneric terminal-read-event (terminal)
  (:documentation "Read and return one semantic input event from TERMINAL."))

(-> terminal-input-ready-p (terminal) boolean)
(defgeneric terminal-input-ready-p (terminal)
  (:documentation "Return true when TERMINAL can read an event without blocking."))

(defmethod terminal-input-ready-p ((terminal terminal))
  "Assume application-provided TERMINAL transports have an event ready."
  (declare (ignore terminal))
  t)

(defgeneric terminal--write (terminal text)
  (:documentation "Write trusted renderer TEXT through the terminal transport."))

(defgeneric terminal-flush (terminal)
  (:documentation "Make all pending TERMINAL output visible."))


(-> terminal--prompt-marker-sequence (keyword integer) string)
(defun terminal--prompt-marker-sequence (marker status)
  "Return Autolith's OSC 133 sequence for MARKER and integer STATUS."
  (check-type status (integer 0))
  (if (eq marker ':prompt-start)
      (format nil "~C]133;A;redraw=0~C~C"
              *terminal-escape-character*
              *terminal-escape-character*
              #\\)
      (semantic-prompt-marker-sequence marker status)))

(-> terminal-write-prompt-marker
    (terminal keyword &optional (integer 0))
    boolean)
(defun terminal-write-prompt-marker (terminal marker &optional (status 0))
  "Write and flush one OSC 133 MARKER for interactive TERMINAL, if applicable."
  (when (terminal-interactive-p terminal)
    (terminal--write terminal
                     (terminal--prompt-marker-sequence marker status))
    (terminal-flush terminal)
    t))
