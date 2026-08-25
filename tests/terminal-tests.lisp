(in-package #:autolith)

;;;; -- Recording Terminal --

(defclass recording-terminal (terminal)
  ((chunks
    :initform nil
    :accessor recording-terminal-chunks
    :type list
    :documentation "Trusted renderer writes captured in reverse chronological order."))
  (:documentation "A deterministic interactive terminal used by terminal seam tests."))

(defmethod terminal--write ((terminal recording-terminal) (text string))
  "Capture trusted TEXT written through TERMINAL."
  (push text (recording-terminal-chunks terminal))
  nil)

(defmethod terminal-flush ((terminal recording-terminal))
  "Finish a recording TERMINAL write batch without external effects."
  (declare (ignore terminal))
  nil)

(defmethod terminal-start ((terminal recording-terminal))
  "Start TERMINAL and emulate only bracketed paste activation."
  (unless (terminal-started-p terminal)
    (setf (terminal-started-p terminal) t
          (terminal-interactive-p terminal) t)
    (terminal--write terminal (terminal-bracketed-paste-enable-sequence)))
  terminal)

(defmethod terminal-stop ((terminal recording-terminal))
  "Stop TERMINAL and emulate only bracketed paste deactivation."
  (when (terminal-started-p terminal)
    (terminal--write terminal (terminal-bracketed-paste-disable-sequence))
    (setf (terminal-started-p terminal) nil
          (terminal-interactive-p terminal) nil))
  terminal)

(defmethod terminal-read-event ((terminal recording-terminal))
  "Return end-of-input because recording terminals have no input queue."
  (declare (ignore terminal))
  :end-of-input)


;;;; -- Scripted Terminal --

(defclass scripted-terminal (recording-terminal)
  ((events
    :initarg :events
    :initform nil
    :accessor scripted-terminal-events
    :type list
    :documentation "Queued semantic input events served to the reader in order.")
   (read-callback
    :initarg :read-callback
    :initform nil
    :reader scripted-terminal-read-callback
    :type (option function)
    :documentation "The optional callback invoked immediately before returning an event."))
  (:documentation "A recording terminal replaying scripted input events."))

(defclass failing-recording-terminal (recording-terminal)
  ((fail-next-write-p
    :initform nil
    :accessor failing-recording-terminal-fail-next-write-p
    :type boolean
    :documentation "Whether the next trusted write should signal a terminal failure."))
  (:documentation "A recording terminal with one explicitly injected write failure."))

(defmethod terminal--write ((terminal failing-recording-terminal) (text string))
  "Fail or capture trusted TEXT according to TERMINAL's injection state."
  (if (failing-recording-terminal-fail-next-write-p terminal)
      (progn
        (setf (failing-recording-terminal-fail-next-write-p terminal) nil)
        (error 'terminal-error
               :message "Injected terminal write failure."
               :operation ':write
               :cause nil))
      (call-next-method)))

(defclass protocol-recording-stream-terminal (stream-terminal)
  ((chunks
    :initform nil
    :accessor protocol-recording-stream-terminal-chunks
    :type list
    :documentation "Protocol writes captured in reverse order.")
   (write-count
    :initform 0
    :accessor protocol-recording-stream-terminal-write-count
    :type (integer 0)
    :documentation "The number of attempted protocol writes.")
   (fail-write-index
    :initarg :fail-write-index
    :initform nil
    :reader protocol-recording-stream-terminal-fail-write-index
    :type (option integer)
    :documentation "The optional one-based write attempt that signals failure.")
   (flush-count
    :initform 0
    :accessor protocol-recording-stream-terminal-flush-count
    :type (integer 0)
    :documentation "The number of attempted protocol flushes."))
  (:documentation "A stream terminal recording and optionally failing protocol writes."))

(defmethod terminal--write
    ((terminal protocol-recording-stream-terminal) (text string))
  "Capture TEXT or signal the configured protocol-write failure."
  (let ((index (incf (protocol-recording-stream-terminal-write-count terminal))))
    (when (eql index
               (protocol-recording-stream-terminal-fail-write-index terminal))
      (error 'terminal-error
             :message "Injected protocol write failure."
             :operation ':write
             :cause nil))
    (push text (protocol-recording-stream-terminal-chunks terminal)))
  nil)

(defmethod terminal-flush ((terminal protocol-recording-stream-terminal))
  "Record one protocol flush without external output."
  (incf (protocol-recording-stream-terminal-flush-count terminal))
  nil)

(defmethod terminal-read-event ((terminal scripted-terminal))
  "Serve the next scripted event, or end of input when exhausted."
  (let ((callback (scripted-terminal-read-callback terminal)))
    (when callback
      (funcall callback)))
  (or (pop (scripted-terminal-events terminal)) :end-of-input))


;;;; -- Test Helpers --

(-> recording-terminal-output (recording-terminal) string)
(defun recording-terminal-output (terminal)
  "Return all output captured by TERMINAL in write order."
  (with-output-to-string (stream)
    (dolist (chunk (reverse (recording-terminal-chunks terminal)))
      (write-string chunk stream))))

(-> recording-terminal-reset (recording-terminal) recording-terminal)
(defun recording-terminal-reset (terminal)
  "Discard output previously captured by TERMINAL."
  (setf (recording-terminal-chunks terminal) nil)
  terminal)

(-> terminal-tests--substring-count (string string) (integer 0))
(defun terminal-tests--substring-count (needle haystack)
  "Return the number of non-overlapping NEEDLE occurrences in HAYSTACK."
  (loop with start = 0
        for position = (search needle haystack :start2 start)
        while position
        count t
        do (setf start (+ position (length needle)))))

(-> terminal-tests--csi-final-index (string integer) (option integer))
(defun terminal-tests--csi-final-index (text start)
  "Return the final-byte index for a CSI in TEXT beginning at START."
  (loop for index from start below (length text)
        when (<= #x40 (char-code (char text index)) #x7e)
          return index))

(-> terminal-tests--private-mode-parameters-p (string integer integer) boolean)
(defun terminal-tests--private-mode-parameters-p (text start end)
  "Return true when TEXT parameters between START and END select an alternate screen."
  (and (< start end)
       (char= (char text start) #\?)
       (loop with parameter-start = (1+ start)
             for separator = (or (position #\; text
                                           :start parameter-start
                                           :end end)
                                 end)
             for value = (parse-integer text
                                        :start parameter-start
                                        :end separator
                                        :junk-allowed t)
             thereis (member value '(47 1047 1049))
             while (< separator end)
             do (setf parameter-start (1+ separator)))))

(-> terminal-tests--forbidden-control-p (string) boolean)
(defun terminal-tests--forbidden-control-p (text)
  "Return true when TEXT enters an alternate screen or erases a display or scrollback."
  (block nil
    (loop with index = 0
          while (< index (length text))
          for character = (char text index)
          for code = (char-code character)
          do (cond
               ((and (= code 27)
                     (< (1+ index) (length text))
                     (char= (char text (1+ index)) #\c))
                (return t))
               ((or (and (= code 27)
                         (< (1+ index) (length text))
                         (char= (char text (1+ index)) #\[))
                    (= code #x9b))
                (let* ((parameter-start (if (= code #x9b)
                                            (1+ index)
                                            (+ index 2)))
                       (final-index
                         (terminal-tests--csi-final-index text parameter-start)))
                  (unless final-index
                    (return t))
                  (let ((final (char text final-index)))
                    (when (or (char= final #\J)
                              (and (member final '(#\h #\l))
                                   (terminal-tests--private-mode-parameters-p
                                    text parameter-start final-index)))
                      (return t)))
                  (setf index final-index)))
               (t
                nil))
             (incf index))
    nil))

(-> terminal-tests--contains-control-character-p (string) boolean)
(defun terminal-tests--contains-control-character-p (text)
  "Return true when TEXT contains an untrusted ESC or C1 control character."
  (loop for character across text
        for code = (char-code character)
        thereis (or (= code 27)
                    (<= 128 code 159))))


;;;; -- Focused Terminal Tests --

(-> test-terminal-primary-screen-controls () null)
(defun test-terminal-primary-screen-controls ()
  "Test primary-screen rendering, bounded live updates, and finalized deduplication."
  (let* ((terminal (make-instance 'recording-terminal :columns 24))
         (ui (terminal-ui-create :terminal terminal :prompt "autolith> ")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working")
      (terminal-ui-process-event active-ui '(:insert "hello"))
      (test-assert
       (terminal-ui-append-finalized active-ui 1 "FINAL-SENTINEL")
       "the first finalized transcript event is emitted")
      (test-assert
       (not (terminal-ui-append-finalized active-ui 1 "DUPLICATE"))
       "a finalized transcript identifier is emitted only once")
      (terminal-ui-set-status active-ui "tool complete")
      (terminal-ui-resize active-ui 12))
    (let ((output (recording-terminal-output terminal)))
      (test-assert
       (= (terminal-tests--substring-count "FINAL-SENTINEL" output) 1)
       "finalized transcript text appears exactly once")
      (test-assert
       (not (search "DUPLICATE" output))
       "duplicate finalized transcript text is absent")
      (test-assert
       (not (terminal-tests--forbidden-control-p output))
       "terminal output never clears a display or enters an alternate screen")))
  nil)

(-> test-terminal-finalized-batch () null)
(defun test-terminal-finalized-batch ()
  "Test finalized batches deduplicate entries and use one transcript payload."
  (let* ((terminal (make-instance 'recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (test-assert
       (= (terminal-ui-append-finalized-batch
           active-ui
           (list (list ':first "BATCH-FIRST")
                 (list ':second "BATCH-SECOND")
                 (list ':first "BATCH-DUPLICATE")))
          2)
       "a finalized batch reports only its distinct new identifiers")
      (let* ((chunks (reverse (recording-terminal-chunks terminal)))
             (payload-chunks
               (remove-if-not
                (lambda (chunk)
                  (or (search "BATCH-FIRST" chunk)
                      (search "BATCH-SECOND" chunk)
                      (search "BATCH-DUPLICATE" chunk)))
                chunks))
             (output (recording-terminal-output terminal)))
        (test-assert
         (and (= (length payload-chunks) 1)
              (search "BATCH-FIRST" (first payload-chunks))
              (search "BATCH-SECOND" (first payload-chunks)))
         "one batch reaches the live region as one combined transcript payload")
        (test-assert
         (and (< (search "BATCH-FIRST" output)
                 (search "BATCH-SECOND" output))
              (not (search "BATCH-DUPLICATE" output)))
         "batch output preserves order and suppresses duplicate identifiers"))
      (recording-terminal-reset terminal)
      (test-assert
       (= (terminal-ui-append-finalized-batch
           active-ui
           (list (list ':first "BATCH-OLD")
                 (list ':third "BATCH-THIRD")))
          1)
       "later batches omit identifiers finalized by an earlier batch")
      (let ((output (recording-terminal-output terminal)))
       (test-assert
         (and (search "BATCH-THIRD" output)
              (not (search "BATCH-OLD" output)))
         "a later batch emits only its newly finalized entry"))))
  (let* ((terminal (make-instance 'failing-recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (setf (failing-recording-terminal-fail-next-write-p terminal) t)
      (test-assert
       (handler-case
           (progn
             (terminal-ui-append-finalized-batch
              active-ui
              (list (list ':retry "BATCH-RETRY")))
             nil)
         (terminal-error ()
           t))
       "a failed terminal write remains visible to the caller")
      (test-assert
       (= (terminal-ui-append-finalized-batch
           active-ui
           (list (list ':retry "BATCH-RETRY")))
          1)
       "a failed batch leaves its identifier available for retry")
      (test-assert
       (= (terminal-tests--substring-count
           "BATCH-RETRY"
           (recording-terminal-output terminal))
          1)
       "retry emits the batch exactly once after a pre-write failure"))
  nil))

(-> test-terminal-untrusted-text () null)
(defun test-terminal-untrusted-text ()
  "Test that every untrusted text path neutralizes terminal control injection."
  (let* ((escape (string *terminal-escape-character*))
         (c1 (string (code-char #x9b)))
         (malicious
           (concatenate 'string
                        "before"
                        escape "[?1049h"
                        escape "[3J"
                        escape "c"
                        c1 "?47h"
                        "after"))
         (terminal (make-instance 'recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal :prompt malicious)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui malicious)
      (terminal-ui-process-event active-ui (list :paste malicious))
      (terminal-ui-append-finalized active-ui :malicious malicious))
    (let ((output (recording-terminal-output terminal))
          (editor-text (line-editor-text (terminal-ui-editor ui))))
      (test-assert
       (not (terminal-tests--forbidden-control-p output))
       "untrusted content cannot inject forbidden terminal controls")
      (test-assert
       (not (terminal-tests--contains-control-character-p editor-text))
       "pasted input stores no ESC or C1 control characters")))
  nil)

(-> test-terminal-finalized-scrollback () null)
(defun test-terminal-finalized-scrollback ()
  "Test that resize and live activity never replay finalized transcript rows."
  (let* ((terminal (make-instance 'recording-terminal :columns 18))
         (ui (terminal-ui-create :terminal terminal)))
    (terminal-ui-start ui)
    (loop for identifier from 1 to 20
          do (terminal-ui-append-finalized
              ui
              identifier
              (format nil "IMMUTABLE-~2,'0D" identifier)))
    (recording-terminal-reset terminal)
    (terminal-ui-set-status ui "streaming token one")
    (terminal-ui-set-status ui "streaming token two")
    (terminal-ui-process-event ui '(:insert "draft"))
    (terminal-ui-resize ui 9)
    (let ((live-output (recording-terminal-output terminal)))
      (loop for identifier from 1 to 20
            do (test-assert
                (not (search (format nil "IMMUTABLE-~2,'0D" identifier)
                             live-output))
                "live repaint does not replay finalized transcript text"))
      (test-assert
       (not (terminal-tests--forbidden-control-p live-output))
       "live repaint and resize preserve terminal scrollback"))
    (terminal-ui-stop ui))
  nil)

(-> test-terminal-resize-frame () null)
(defun test-terminal-resize-frame ()
  "Test that a wider terminal resize replaces the reflowed live region once."
  (let* ((terminal (make-instance 'recording-terminal :columns 8))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-process-event
       active-ui
       '(:insert "a long wrapped input line"))
      (recording-terminal-reset terminal)
      (terminal-ui-resize active-ui 24)
      (test-assert (= (length (recording-terminal-chunks terminal)) 1)
                   "resize reflows and replaces the live region in one frame")))
  nil)

(-> test-terminal-line-editor () null)
(defun test-terminal-line-editor ()
  "Test Autolith event dispatch, submission, control policy, and reader actions."
  (let* ((raw-content
           (format nil "a~Cb~Cc~Cd"
                   #\Tab #\Return *terminal-escape-character*))
         (terminal (make-instance 'recording-terminal :columns 40))
         (editor (line-editor-create :text raw-content))
         (ui (terminal-ui-create :terminal terminal :editor editor)))
    (multiple-value-bind (text display cursor)
        (terminal-ui--live-content ui)
      (declare (ignore display cursor))
      (test-assert
       (not (terminal-tests--contains-control-character-p text))
       "externally supplied editor controls are sanitized before display")))
  (let* ((terminal (make-instance 'recording-terminal :columns 12))
         (editor (line-editor-create :history-limit 2))
         (ui (terminal-ui-create :terminal terminal :editor editor)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-process-event active-ui '(:insert "abc"))
      (terminal-ui-process-event active-ui ':left)
      (terminal-ui-process-event active-ui '(:insert "X"))
      (test-assert (string= (line-editor-text editor) "abXc")
                   "terminal events edit the active input buffer")
      (terminal-ui-process-event active-ui ':insert-newline)
      (terminal-ui-process-event active-ui '(:insert "second"))
      (multiple-value-bind (action submitted)
          (terminal-ui-process-event active-ui ':submit)
        (test-assert (eq action ':submit)
                     "the terminal dispatches submission")
        (test-assert (string= submitted (format nil "abX~%secondc"))
                     "submission returns the complete multiline input"))
      (terminal-ui-process-event active-ui '(:insert "draft"))
      (multiple-value-bind (action payload)
          (terminal-ui-process-event active-ui ':interrupt)
        (declare (ignore payload))
        (test-assert (eq action ':cleared)
                     "interrupt clears non-empty editor input"))
      (multiple-value-bind (action payload)
          (terminal-ui-process-event active-ui ':interrupt)
        (declare (ignore payload))
        (test-assert (eq action ':interrupt)
                     "interrupt propagates when the editor is empty"))
      (multiple-value-bind (action payload)
          (terminal-ui-process-event active-ui ':end-of-input)
        (declare (ignore payload))
        (test-assert (eq action ':end-of-input)
                     "end of input propagates when the editor is empty"))))
  nil)

(-> test-terminal-history-replacement () null)
(defun test-terminal-history-replacement ()
  "Test bounded history loading preserves the active draft and cursor."
  (let* ((terminal (make-instance 'recording-terminal :columns 40))
         (editor
           (line-editor-create
            :history '("older" "newer")
            :history-limit 2))
         (ui (terminal-ui-create :terminal terminal :editor editor)))
    (terminal-ui-set-input ui "draft")
    (terminal-ui-process-event ui :history-previous)
    (terminal-ui-process-event ui :left)
    (terminal-ui-load-history ui '("one" "two" "three"))
    (test-assert
     (equalp (line-editor-history editor) #("two" "three"))
     "history loading honors the target editor's custom limit")
    (test-assert
     (and (string= (line-editor-text editor) "draft")
          (= (line-editor-cursor editor) 5)
          (not (terminal-ui--editor-history-navigating-p editor)))
     "history loading restores the draft and leaves obsolete traversal")
    (terminal-ui-process-event ui :left)
    (terminal-ui-load-history ui '("alpha" "beta"))
    (test-assert
     (and (string= (line-editor-text editor) "draft")
          (= (line-editor-cursor editor) 4))
     "history loading preserves an ordinary draft cursor exactly")
    (terminal-ui-process-event ui :submit)
    (test-assert
     (equalp (line-editor-history editor) #("beta" "draft"))
     "replacement history remains extendable and bounded"))
  nil)


(-> test-terminal-image-attachments () null)
(defun test-terminal-image-attachments ()
  "Test pasted image labels, submission payloads, pruning, and history recall."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (first-image (merge-pathnames "first image.png" root))
         (second-image (merge-pathnames "second image.png" root))
         (terminal (make-instance 'recording-terminal :columns 40))
         (editor (line-editor-create))
         (ui (terminal-ui-create :terminal terminal :editor editor)))
    (unwind-protect
         (progn
           (test-conversation--write-tiny-png first-image)
           (test-conversation--write-tiny-png second-image)
           (with-terminal-ui (active-ui ui)
             (terminal-ui-process-event
              active-ui
              (list :paste (format nil "'~A'" (namestring first-image))))
             (terminal-ui-process-event active-ui '(:insert " describe this"))
             (test-assert
              (string= (line-editor-text editor)
                       "[Image #1] describe this")
              "pasting an image pathname inserts a numbered image label")
             (multiple-value-bind (action submitted)
                 (terminal-ui-process-event active-ui :submit)
               (test-assert (eq action :submit)
                            "an image draft remains an ordinary submission")
               (test-assert
                (and (typep submitted 'user-message-input)
                     (string= (user-message-input-text submitted)
                              "[Image #1] describe this")
                     (equal (user-message-input-image-pathnames submitted)
                            (list (truename first-image))))
                "image submission preserves text and the absolute local path"))
             (terminal-ui-process-event active-ui :history-previous)
             (multiple-value-bind (action recalled)
                 (terminal-ui-process-event active-ui :submit)
               (test-assert
                (and (eq action :submit)
                     (typep recalled 'user-message-input)
                     (equal (user-message-input-image-pathnames recalled)
                            (list (truename first-image))))
                "Clinedi history recall restores image attachment metadata"))
             (terminal-ui-process-event
              active-ui
              (list :paste (format nil "'~A'" (namestring first-image))))
             (terminal-ui-process-event
              active-ui
              (list :paste (format nil "'~A'" (namestring second-image))))
             (line-editor-set-text editor "[Image #2] only")
             (multiple-value-bind (action pruned)
                 (terminal-ui-process-event active-ui :submit)
               (test-assert
                (and (eq action :submit)
                     (typep pruned 'user-message-input)
                     (string= (user-message-input-text pruned)
                              "[Image #1] only")
                     (equal (user-message-input-image-pathnames pruned)
                            (list (truename second-image))))
                "deleted image labels prune attachments and renumber survivors"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-terminal-input-decoding () null)
(defun test-terminal-input-decoding ()
  "Test production key decoding and bracketed or raw paste collection."
  (let* ((escape *terminal-escape-character*)
         (paste-start (format nil "~C[200~~" escape))
         (paste-end (format nil "~C[201~~" escape))
         (input
           (concatenate 'string
                        (format nil "~C[A" escape)
                        paste-start
                        "paste"
                        (format nil "~C[3J" escape)
                        paste-end
                        (string escape)
                        (string #\Return)
                        (format nil "~C[13;2u" escape)
                        (string (code-char 4))
                        (format nil "~C[100;5u" escape)))
         (terminal
           (make-instance 'stream-terminal
                          :input-stream (make-string-input-stream input)
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :interactive-p t
                          :columns 40)))
    (test-assert (eq (terminal-read-event terminal) :up)
                 "the production decoder recognizes an up arrow")
    (let ((paste-event (terminal-read-event terminal)))
      (test-assert (eq (first paste-event) :paste)
                   "the production decoder recognizes bracketed paste")
      (let ((editor (line-editor-create)))
        (line-editor-handle-event editor paste-event)
        (test-assert
         (not (terminal-tests--contains-control-character-p
               (line-editor-text editor)))
         "bracketed paste terminal controls are neutralized before display")))
    (test-assert (eq (terminal-read-event terminal) :insert-newline)
                 "legacy Alt-Enter inserts a newline")
    (test-assert (eq (terminal-read-event terminal) :insert-newline)
                 "Clinedi enhanced Shift-Enter reaches the production decoder")
    (test-assert (eq (terminal-read-event terminal) :end-of-input)
                 "literal Ctrl-D requests end of input")
    (test-assert (eq (terminal-read-event terminal) :end-of-input)
                 "Clinedi enhanced Ctrl-D reaches the production decoder")
    (test-assert (eq (terminal-read-event terminal) :stream-end)
                 "physical interactive stream EOF remains distinct from Ctrl-D"))
  (let ((terminal
          (make-instance 'stream-terminal
                         :input-stream (make-string-input-stream "")
                         :output-stream (make-string-output-stream)
                         :input-file-descriptor 0
                         :interactive-p nil
                         :columns 40)))
    (test-assert (eq (terminal-read-event terminal) :stream-end)
                 "physical fallback stream EOF remains distinct from Ctrl-D"))
  (let* ((payload (format nil "first line~%second line"))
         (terminal
           (make-instance
            'stream-terminal
            :input-stream
            (make-string-input-stream
             (concatenate 'string (string (code-char 22)) payload))
            :output-stream (make-string-output-stream)
            :input-file-descriptor 0
            :interactive-p t
            :columns 40))
         (event (terminal-read-event terminal)))
    (test-assert
     (and (eq (first event) :paste)
          (string= (second event) payload))
     "literal Ctrl-V paste bursts retain embedded newlines without submission"))
  (let* ((payload (format nil "first line~%second line"))
         (terminal
           (make-instance
            'stream-terminal
            :input-stream (make-string-input-stream payload)
            :output-stream (make-string-output-stream)
            :input-file-descriptor 0
            :interactive-p t
            :columns 40))
         (event (terminal-read-event terminal))
         (editor (line-editor-create)))
    (test-assert
     (and (eq (first event) :paste)
          (string= (second event) payload))
     "an unbracketed multiline terminal burst becomes one paste event")
    (multiple-value-bind (action submitted)
        (line-editor-handle-event editor event)
      (test-assert
       (and (eq action :continue)
            (null submitted)
            (string= (line-editor-text editor) payload))
       "an unbracketed multiline terminal paste never submits input")))
  (let* ((payload
           (format nil "first~%second~Cthird~C[A~C"
                   #\Tab *terminal-escape-character* (code-char 3)))
         (sanitized (sanitize-text payload))
         (terminal
           (make-instance
            'stream-terminal
            :input-stream (make-string-input-stream payload)
            :output-stream (make-string-output-stream)
            :input-file-descriptor 0
            :interactive-p t
            :columns 40))
         (event (terminal-read-event terminal))
         (editor (line-editor-create)))
    (test-assert
     (and (eq (first event) :paste)
          (string= (second event) sanitized))
     "multiline paste precedence neutralizes later editing controls")
    (multiple-value-bind (action submitted)
        (line-editor-handle-event editor event)
      (test-assert
       (and (eq action :continue)
            (null submitted)
            (string= (line-editor-text editor) sanitized))
       "multiline controls cannot submit or invoke Clinedi editing commands"))
    (test-assert (eq (terminal-read-event terminal) :stream-end)
                 "a multiline control paste remains one event"))
  (multiple-value-bind (read-descriptor write-descriptor)
      (sb-posix:pipe)
    (let ((input nil)
          (output nil))
      (unwind-protect
           (let ((payload (format nil "pipe first~%pipe second")))
             (setf input
                   (sb-sys:make-fd-stream
                    read-descriptor
                    :input t
                    :element-type 'character
                    :external-format ':utf-8
                    :buffering ':none
                    :auto-close nil)
                   output
                   (sb-sys:make-fd-stream
                    write-descriptor
                    :output t
                    :element-type 'character
                    :external-format ':utf-8
                    :buffering ':none
                    :auto-close nil))
             (write-string payload output)
             (finish-output output)
             (let* ((terminal
                      (make-instance
                       'stream-terminal
                       :input-stream input
                       :output-stream (make-string-output-stream)
                       :input-file-descriptor read-descriptor
                       :interactive-p t
                       :columns 40))
                    (event (terminal-read-event terminal)))
               (test-assert
                (and (eq (first event) :paste)
                     (string= (second event) payload))
                "one OS-level multiline input write never becomes submission")
               (let ((single-line "pipe single-line paste"))
                 (write-string single-line output)
                 (finish-output output)
                 (test-assert
                  (equal (terminal-read-event terminal)
                         (list ':insert single-line))
                  "one OS-level single-line input write becomes one insert event"))))
        (when input
          (close input))
        (when output
          (close output))
        (ignore-errors (sb-posix:close read-descriptor))
        (ignore-errors (sb-posix:close write-descriptor)))))
  (let* ((payload "single-line paste")
         (terminal
           (make-instance
            'stream-terminal
            :input-stream (make-string-input-stream payload)
            :output-stream (make-string-output-stream)
            :input-file-descriptor 0
            :interactive-p t
            :columns 40))
         (event (terminal-read-event terminal))
         (editor (line-editor-create)))
    (test-assert
     (equal event (list ':insert payload))
     "a buffered plain-text burst becomes one insert event")
    (multiple-value-bind (action submitted)
        (line-editor-handle-event editor event)
      (test-assert
       (and (eq action :continue)
            (null submitted)
            (string= (line-editor-text editor) payload))
       "a coalesced plain-text burst reaches Clinedi atomically"))
    (test-assert (eq (terminal-read-event terminal) :stream-end)
                 "a coalesced plain-text burst leaves no per-character events"))
  (let ((terminal
          (make-instance
           'stream-terminal
           :input-stream
           (make-string-input-stream (format nil "a~C" #\Tab))
           :output-stream (make-string-output-stream)
           :input-file-descriptor 0
           :interactive-p t
           :columns 40)))
    (test-assert
     (equal (terminal-read-event terminal) '(:insert "a"))
     "a mixed buffered burst retains text before a control event")
    (test-assert (eq (terminal-read-event terminal) :complete)
                 "a mixed buffered burst retains its control event"))
  (dolist (case
           (list
            (list (string *terminal-escape-character*) ':escape)
            (list (string (code-char 3)) ':interrupt)
            (list (string #\Tab) ':complete)
            (list (string (code-char 127)) ':backspace)
            (list (format nil "~C[B" *terminal-escape-character*) ':down)
            (list (format nil "~C[C" *terminal-escape-character*) ':right)
            (list (format nil "~C[D" *terminal-escape-character*) ':left)))
    (destructuring-bind (input expected) case
      (let ((terminal
              (make-instance 'stream-terminal
                             :input-stream (make-string-input-stream input)
                             :output-stream (make-string-output-stream)
                             :input-file-descriptor 0
                             :interactive-p t
                             :columns 40)))
        (test-assert (eq (terminal-read-event terminal) expected)
                     "ordinary raw and CSI keys retain semantic input events"))))
  (let* ((output (make-string-output-stream))
         (terminal
           (make-instance 'stream-terminal
                          :input-stream (make-string-input-stream "")
                          :output-stream output
                          :input-file-descriptor 0))
         (expected
           (concatenate
            'string
            (terminal-keyboard-enhancement-enable-sequence)
            (terminal-bracketed-paste-enable-sequence)
            (terminal-bracketed-paste-disable-sequence)
            (terminal-keyboard-enhancement-disable-sequence))))
    (terminal--enable-input-protocols terminal)
    (terminal--disable-input-protocols terminal)
    (test-assert (string= (get-output-stream-string output) expected)
                 "terminal input protocols enable and disable once in order"))
  (let* ((terminal
           (make-instance 'protocol-recording-stream-terminal
                          :input-stream (make-string-input-stream "")
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :fail-write-index 2))
         (failure nil))
    (handler-case
        (terminal--enable-input-protocols terminal)
      (terminal-error (condition)
        (setf failure condition)))
    (let ((controls
            (format nil "~{~A~}"
                    (reverse
                     (protocol-recording-stream-terminal-chunks terminal)))))
      (test-assert failure
                   "partial protocol activation preserves its original failure")
      (test-assert
       (and (search (terminal-bracketed-paste-disable-sequence) controls)
            (search (terminal-keyboard-enhancement-disable-sequence) controls)
            (= (protocol-recording-stream-terminal-flush-count terminal) 1))
       "partial protocol activation attempts every cleanup control")))
  (let* ((terminal
           (make-instance 'protocol-recording-stream-terminal
                          :input-stream (make-string-input-stream "")
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :fail-write-index 1))
         (failure nil))
    (handler-case
        (terminal--disable-input-protocols terminal)
      (terminal-error (condition)
        (setf failure condition)))
    (test-assert failure
                 "protocol shutdown reports its first cleanup failure")
    (test-assert
     (and (equal (protocol-recording-stream-terminal-chunks terminal)
                 (list (terminal-keyboard-enhancement-disable-sequence)))
          (= (protocol-recording-stream-terminal-flush-count terminal) 1))
     "protocol shutdown continues after one cleanup write fails"))
  nil)


(-> test-terminal-bounded-editor-repaint () null)
(defun test-terminal-bounded-editor-repaint ()
  "Test atomic repaint and cursor-following height bounds for long drafts."
  (let* ((terminal (make-instance 'recording-terminal :rows 5 :columns 8))
         (ui (terminal-ui-create :terminal terminal :prompt "❯ "))
         (draft (make-string 160 :initial-element #\x)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-append-finalized active-ui :sentinel "HISTORY-SENTINEL")
      (recording-terminal-reset terminal)
      (terminal-ui-process-event active-ui (list :insert draft))
      (let ((output (recording-terminal-output terminal)))
        (test-assert (= (length (recording-terminal-chunks terminal)) 1)
                     "one editor change is one terminal write")
        (test-assert
         (= (terminal-tests--substring-count
             (format nil "~C[?25l" *terminal-escape-character*)
             output)
            1)
         "one editor repaint hides the cursor once")
        (test-assert
         (= (terminal-tests--substring-count
             (format nil "~C[?25h" *terminal-escape-character*)
             output)
            1)
         "one editor repaint restores the cursor once")
        (test-assert (not (search "HISTORY-SENTINEL" output))
                     "long draft repaint never replays scrollback")
        (test-assert (not (terminal-tests--forbidden-control-p output))
                     "long draft repaint never erases the display"))
      (test-assert (= (live-region-maximum-rows
                       (terminal-ui-live-region active-ui))
                      4)
                   "the editor leaves one viewport row outside its live region")
      (test-assert (<= (terminal-ui-live-row-count active-ui) 4)
                   "a long draft remains inside its terminal-height budget")
      (terminal-ui-process-event active-ui :home)
      (test-assert (<= (terminal-ui-live-row-count active-ui) 4)
                   "the bounded viewport follows the cursor to the draft start")))
  nil)


(-> test-terminal-transient-notice () null)
(defun test-terminal-transient-notice ()
  "Test transient notices expire without entering terminal scrollback."
  (let* ((clock 0)
         (terminal (make-instance 'recording-terminal :columns 60))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () clock))))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-notice
       active-ui
       "Press Ctrl-C again within 2.5 seconds to force exit."
       :duration-seconds 5/2)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (search "Press Ctrl-C again" text)
                     "the transient notice appears in the live region"))
      (setf clock 5/2)
      (with-terminal-ui-locked (active-ui)
        (terminal-ui-set-input active-ui "draft"))
      (test-assert (null (terminal-ui-notice active-ui))
                   "any locked repaint clears a notice at its deadline")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "Press Ctrl-C again" text))
                     "the expired notice vanishes from the live region"))
      (setf clock 10)
      (terminal-ui-set-notice
       active-ui
       "Press Ctrl-C again within 2.5 seconds to force exit."
       :duration-seconds 5/2)
      (setf clock 25/2)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader refresh also repaints an expired notice")
      (test-assert (null (terminal-ui-notice active-ui))
                   "the reader refresh clears the renewed notice")))
  nil)

(-> test-terminal-notice-lock-contention () null)
(defun test-terminal-notice-lock-contention ()
  "Test notice updates never wait behind ordinary presentation work."
  (let* ((terminal (make-instance 'recording-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal))
         (state-lock (make-lock "Autolith notice contention test"))
         (condition (make-condition-variable
                     :name "Autolith notice contention test"))
         (ui-lock-held-p nil)
         (release-ui-lock-p nil)
         (notice-call-returned-p nil)
         (holder nil)
         (setter nil))
    (terminal-ui-start ui)
    (unwind-protect
         (progn
           (setf holder
                 (make-thread
                  (lambda ()
                    (with-terminal-ui-locked (ui)
                      (with-lock-held (state-lock)
                        (setf ui-lock-held-p t)
                        (condition-notify condition)
                        (unless release-ui-lock-p
                          (condition-wait condition state-lock :timeout 2)))))
                  :name "Autolith notice lock holder"))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (with-lock-held (state-lock) ui-lock-held-p))
             1)
            "the contention test holds the presentation lock")
           (setf setter
                 (make-thread
                  (lambda ()
                    (terminal-ui-set-notice
                     ui "must not appear" :duration-seconds 5/2)
                    (with-lock-held (state-lock)
                      (setf notice-call-returned-p t)
                      (condition-notify condition)))
                  :name "Autolith nonblocking notice setter"))
           (test-assert
            (task-tests--wait-until
             (lambda ()
               (with-lock-held (state-lock) notice-call-returned-p))
             1)
            "a notice update returns while presentation remains locked")
           (test-assert (null (terminal-ui-notice ui))
                        "a contended notice is dropped instead of delayed"))
      (with-lock-held (state-lock)
        (setf release-ui-lock-p t)
        (condition-notify condition))
      (when setter
        (ignore-errors (join-thread setter)))
      (when holder
        (ignore-errors (join-thread holder)))
      (ignore-errors (terminal-ui-stop ui))))
  nil)

(-> test-terminal-timed-status () null)
(defun test-terminal-timed-status ()
  "Test status animation, elapsed activity, and stale progress timing."
  (let* ((clock 0)
         (clock-calls 0)
         (terminal (make-instance 'recording-terminal :columns 60))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda ()
                                (incf clock-calls)
                                clock))))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working")
      (test-assert (= clock-calls 1)
                   "starting activity samples the monotonic clock once")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (search "READ  ∙ 00:00" text)
                     "live activity starts with its spinner and elapsed clock")
        (let ((lines (uiop:split-string text :separator '(#\Newline))))
          (test-assert
           (and (not (search "working" (second lines)))
                (search "working" (third lines)))
           "provider activity stays below rather than inside the modeline")))
      (setf clock 0.24)
      (test-assert (not (terminal-ui-refresh-status active-ui))
                   "time within one spinner frame does not repaint activity")
      (setf clock 0.25)
      (let ((calls-before-refresh clock-calls))
        (test-assert (terminal-ui-refresh-status active-ui)
                     "a new spinner frame repaints activity")
        (test-assert (= clock-calls (1+ calls-before-refresh))
                     "one timestamp drives both status signature and paint"))
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore display cursor))
        (test-assert (search "EVAL  ∙ 00:00" text)
                     "the spinner advances without shifting the elapsed clock"))
      (setf clock 1)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "a new elapsed second repaints activity")
      (setf clock 29)
      (terminal-ui-note-status-progress active-ui)
      (terminal-ui-refresh-status active-ui)
      (setf clock 58)
      (terminal-ui-refresh-status active-ui)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "no update" text))
                     "recent progress keeps the activity from looking stale"))
      (setf clock 59)
      (terminal-ui-refresh-status active-ui)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert
         (search "00:59 · no update 00:30" text)
         "stale activity states how long no progress has arrived")
        (let ((lines (uiop:split-string text :separator '(#\Newline))))
          (test-assert
           (and (not (search "working" (second lines)))
                (search "working" (third lines)))
           "stale timing keeps provider activity below the modeline")))
       (terminal-ui-set-agent-activities
        active-ui
        (list
         (list :id "active-child"
               :index 1
               :agent "reviewer"
               :state ':running
               :recent-tools nil
               :request-count 1
               :duration-ms 30000
               :assignment "Review the change."
               :detached t)))
       (multiple-value-bind (text display cursor)
           (terminal-ui--live-content active-ui)
         (declare (ignore display cursor))
         (test-assert (not (search "no update" text))
                      "running child activity suppresses the stale warning")
         (test-assert (and (search "00:59" text)
                           (search "active-child" text))
                      "running child activity keeps compact timing and identity visible"))
       (terminal-ui-set-agent-activities active-ui nil)
       (multiple-value-bind (text display cursor)
           (terminal-ui--live-content active-ui)
         (declare (ignore display cursor))
         (test-assert (search "00:59 · no update 00:30" text)
                      "the stale warning returns after child activity ends"))
      (terminal-ui-note-status-progress active-ui)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "new progress immediately clears the stale status state")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "no update" text))
                     "new progress removes the stale warning"))))
  nil)

(-> test-terminal-compaction-indicator () null)
(defun test-terminal-compaction-indicator ()
  "Test compaction state, refresh timing, and idempotent lifecycle updates."
  (let* ((clock 0)
         (clock-calls 0)
         (terminal (make-instance 'recording-terminal :columns 60))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda ()
                                (incf clock-calls)
                                clock))))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working")
      (recording-terminal-reset terminal)
      (terminal-ui-set-compacting active-ui t)
      (test-assert
       (terminal-ui-compacting-p active-ui)
       "starting compaction records active state")
      (test-assert
       (= (terminal-ui-compaction-started-at active-ui) 0)
       "starting compaction records its start time")
      (test-assert
       (= (length (recording-terminal-chunks terminal)) 1)
       "starting compaction repaints")
      (recording-terminal-reset terminal)
      (let ((calls-before-repeat clock-calls))
        (terminal-ui-set-compacting active-ui t)
        (test-assert
         (= clock-calls calls-before-repeat)
         "repeating active compaction does not read the clock")
        (test-assert
         (string= (recording-terminal-output terminal) "")
         "repeating active compaction does not repaint"))
      (setf clock 0.24)
      (test-assert (not (terminal-ui-refresh-status active-ui))
                   "time within one compaction frame does not repaint")
      (setf clock 0.25)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "a new compaction frame repaints")
      (recording-terminal-reset terminal)
      (terminal-ui-set-compacting active-ui nil)
      (test-assert
       (not (terminal-ui-compacting-p active-ui))
       "clearing compaction removes active state")
      (test-assert
       (null (terminal-ui-compaction-started-at active-ui))
       "clearing compaction removes timing state")
      (test-assert
       (= (length (recording-terminal-chunks terminal)) 1)
       "clearing compaction repaints")
      (recording-terminal-reset terminal)
      (let ((calls-before-repeat clock-calls))
        (terminal-ui-set-compacting active-ui nil)
        (test-assert
         (= clock-calls calls-before-repeat)
         "repeating cleared compaction does not read the clock")
        (test-assert
         (string= (recording-terminal-output terminal) "")
         "repeating cleared compaction does not repaint")))
  nil))

(-> test-terminal-agent-activities () null)
(defun test-terminal-agent-activities ()
  "Test bounded cumulative traces and expanded blocking child rows."
  (let* ((clock 65)
         (terminal (make-instance 'recording-terminal
                                  :columns 100
                                  :styled-p t))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () clock)))
         (activities
           (list
            (list :id "review-agent-2"
                  :index 2
                  :agent "reviewer"
                  :state ':running
                  :current-tool "lisp.eval"
                  :current-tool-duration-ms 60000
                  :recent-tools '("search.content" "resource.read")
                  :request-count 1
                  :duration-ms 65000
                  :assignment "Review the finished patch."
                  :detached nil)
            (list :id "search-1"
                  :index 1
                  :agent "explorer"
                  :state ':running
                  :current-tool "lisp.eval"
                  :current-tool-duration-ms 60000
                  :recent-tools
                  '("search.files" "search.glob" "resource.read" "lisp.load-system")
                  :request-count 2
                  :duration-ms 65000
                  :assignment "Locate the scheduler."
                  :detached t))))
    (let ((oversized-activity (copy-list (first activities))))
      (setf (getf oversized-activity :recent-tools)
            (loop repeat (1+ *task-progress-recent-tool-limit*)
                  collect "tool"))
      (test-assert
       (not (terminal-agent-activity-p oversized-activity))
       "the terminal rejects child traces above its retained milestone bound"))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working")
      (recording-terminal-reset terminal)
      (terminal-ui-set-agent-activities active-ui activities)
      (test-assert (string= (recording-terminal-output terminal) "")
                   "child notifications do not paint from worker threads")
      (test-assert
       (equal (mapcar (lambda (activity) (getf activity :id))
                      (terminal-ui-agent-activities active-ui))
              '("search-1" "review-agent-2"))
       "child rows retain scheduler creation order")
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader coalesces changed child state into one frame")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore cursor))
        (let* ((lines (uiop:split-string text :separator '(#\Newline)))
               (header-index (position "agents 2" lines :test #'string=))
               (search-index
                 (position-if (lambda (line)
                                (search "search-1" line))
                              lines))
               (review-index
                 (position-if (lambda (line)
                                (search "review-agent-2" line))
                              lines))
               (expanded-index
                 (position-if (lambda (line)
                                (search "↳ " line))
                              lines))
               (status-index
                 (position-if (lambda (line)
                                (search "READ " line))
                              lines))
               (search-line (and search-index (nth search-index lines)))
               (review-line (and review-index (nth review-index lines)))
               (expanded-line (and expanded-index
                                   (nth expanded-index lines)))
               (search-role-position
                 (and search-line (search "explorer" search-line)))
               (review-role-position
                 (and review-line (search "reviewer" review-line)))
               (search-detail-position
                 (and search-line (search " · … ›" search-line)))
               (review-detail-position
                 (and review-line (search " · blocking" review-line))))
          (test-assert
           (and header-index
                search-index
                review-index
                expanded-index
                status-index
                (< status-index header-index)
                (= search-index (+ header-index 2))
                (= review-index (1+ search-index))
                (= expanded-index (1+ review-index))
                (string= (nth (1+ header-index) lines) "")
                (search "working" (nth (1+ expanded-index) lines))
                (string= (nth (+ expanded-index 2) lines) "")
                (uiop:string-prefix-p "  " search-line)
                (uiop:string-prefix-p "  " review-line)
                (uiop:string-prefix-p "      ↳ " expanded-line)
                search-role-position
                review-role-position
                (= (text-cell-width
                    (subseq search-line 0 search-role-position))
                   (text-cell-width
                    (subseq review-line 0 review-role-position)))
                search-detail-position
                review-detail-position
                (= (text-cell-width
                    (subseq search-line 0 search-detail-position))
                   (text-cell-width
                    (subseq review-line 0 review-detail-position)))
                (search
                 "explorer · … › resource.read › lisp.load-system › lisp.eval 01:00"
                 text)
                (search "reviewer · blocking · lisp.eval 01:00" text)
                (search
                 "↳ search.content › resource.read › lisp.eval 01:00 · Review the finished patch."
                 text)
                (not (search "search.files ›" search-line))
                (not (search "search.glob ›" search-line)))
           "child rows show bounded aligned traces below the modeline")
          (test-assert (not (search "async" text))
                       "detached state does not add a redundant row label"))
        (test-assert
         (every (lambda (row)
                  (<= (terminal--spans-width row) 12))
                (terminal-ui--agent-activity-rows-at
                 active-ui clock 12))
         "expanded and compact child rows remain bounded on narrow terminals")
        (test-assert
         (and (search (terminal-style-sequence ':agent-spinner) display)
              (search (terminal-style-sequence ':agent-name) display)
              (search (terminal-style-sequence ':agent-role) display)
              (search (terminal-style-sequence ':agent-tool) display))
         "running child traces use distinct basic-palette semantic colors"))
      (setf clock 65.24)
      (test-assert (not (terminal-ui-refresh-status active-ui))
                   "child spinners do not repaint within one animation frame")
      (setf clock 65.25)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "running child spinners advance on the shared cadence")
      (terminal-ui-set-status active-ui nil)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore display cursor))
        (let* ((lines (uiop:split-string text :separator '(#\Newline)))
               (expanded-index
                 (position-if (lambda (line)
                                (search "↳ " line))
                              lines)))
          (test-assert
           (and (search "search-1" text)
                (search "∙ 00:00" text)
                expanded-index
                (string= (nth (1+ expanded-index) lines) "")
                (non-empty-string-p (nth (+ expanded-index 2) lines)))
           "child traces retain the status animation above the idle prompt")))
      (setf clock 65.5)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "child and status animation continue without provider activity")
      (terminal-ui-set-agent-activities
       active-ui
       (list
        (list :id "blocking-queued"
              :index 1
              :agent "reviewer"
              :state ':queued
              :current-tool nil
              :current-tool-duration-ms nil
              :recent-tools nil
              :request-count 0
              :duration-ms nil
              :assignment "Wait for the implementation."
              :detached nil)))
      (terminal-ui-refresh-status active-ui)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore display cursor))
        (test-assert
         (and (search "reviewer · blocking · queued" text)
              (search "↳ Wait for the implementation." text))
         "queued blocking children retain their assignment on an expanded row"))
      (let ((many-activities
              (loop for index from 1 to 10
                    collect
                    (list :id (format nil "worker-~D" index)
                          :index index
                          :agent "worker"
                          :state ':queued
                          :current-tool nil
                          :current-tool-duration-ms nil
                          :recent-tools nil
                          :request-count 0
                          :duration-ms nil
                          :assignment "Wait for capacity."
                          :detached t))))
        (terminal-ui-set-agent-activities active-ui many-activities)
        (terminal-ui-refresh-status active-ui)
        (multiple-value-bind (text display cursor)
            (terminal-ui--live-content active-ui clock)
          (declare (ignore display cursor))
          (test-assert
           (and (search "agents 10" text)
                (find-if
                 (lambda (line)
                   (uiop:string-prefix-p "  … 2 more agents" line))
                 (uiop:split-string text :separator '(#\Newline)))
                (not (search "worker-9" text)))
           "the child strip caps agents and summarizes overflow")))
      (terminal-ui-set-agent-activities active-ui nil)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "clearing the final child repaints the live region")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui)
        (declare (ignore display cursor))
        (test-assert (not (search "agents " text))
                     "terminal child state disappears when no jobs remain"))))
  (let* ((short-clock 65)
         (short-terminal (make-instance 'recording-terminal
                                        :columns 80
                                        :rows 10))
         (short-ui
           (terminal-ui-create
            :terminal short-terminal
            :clock-function (lambda () short-clock)))
         (blocking-activities
           (loop for index from 1 to 8
                 collect
                 (list :id (format nil "worker-~D" index)
                       :index index
                       :agent "reviewer"
                       :state ':running
                       :current-tool "lisp.eval"
                       :current-tool-duration-ms 60000
                       :recent-tools '("resource.read")
                       :request-count 1
                       :duration-ms 65000
                       :assignment "Review the implementation."
                       :detached nil))))
    (with-terminal-ui (active-ui short-ui)
      (terminal-ui-set-status active-ui "working")
      (terminal-ui-set-agent-activities active-ui blocking-activities)
      (let* ((rows
               (terminal-ui--agent-activity-rows-at
                active-ui short-clock 80))
             (text
               (format nil "~{~A~^~%~}"
                       (mapcar #'terminal--spans-text rows))))
        (test-assert
         (and (= (terminal-ui--agent-row-budget active-ui) 3)
              (= (length rows) 3)
              (search "agents 8" text)
              (search "worker-1" text)
              (search "blocking · lisp.eval 01:00" text)
              (search "… 7 more agents" text)
              (not (search "worker-2" text)))
         "short terminals preserve one useful blocking row inside their budget"))
      (terminal-ui-refresh-status active-ui)
      (test-assert
       (<= (terminal-ui-live-row-count active-ui)
           (live-region-maximum-rows (terminal-ui-live-region active-ui)))
       "blocking traces remain inside the live-region viewport budget")))
  nil)

(-> test-terminal-command-activities () null)
(defun test-terminal-command-activities ()
  "Test primary command rows, timing, completion retention, and row bounds."
  (let* ((clock 65)
         (terminal (make-instance 'recording-terminal
                                  :columns 90
                                  :rows 12
                                  :styled-p t))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () clock)))
         (commands
           (list
            (list :id "exec:2" :type ':tool :index 2
                  :tool "shell.run" :description "Build the release"
                  :state ':queued :duration-ms nil :detached t)
            (list :id "exec:1" :type ':tool :index 1
                  :tool "shell.run" :description "Run repository checks"
                  :state ':running :duration-ms 60000 :detached nil)))
         (agent
           (list :id "review-3" :index 3 :agent "reviewer"
                 :state ':running :current-tool "resource.read"
                 :current-tool-duration-ms 1000 :recent-tools nil
                 :request-count 1 :duration-ms 1000
                 :assignment "Review the patch." :detached t)))
    (test-assert
     (not (terminal-command-activity-p
           (list :id "exec:1" :type ':tool :index 1
                 :tool "shell.run" :description ""
                 :state ':running :duration-ms 0 :detached nil)))
     "command rows require a non-empty display label")
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "receiving response")
      (recording-terminal-reset terminal)
      (terminal-ui-set-agent-activities active-ui (list agent))
      (terminal-ui-set-command-activities active-ui commands)
      (test-assert (string= (recording-terminal-output terminal) "")
                   "command notifications do not paint from worker threads")
      (test-assert
       (equal (mapcar (lambda (activity) (getf activity :id))
                      (terminal-ui-command-activities active-ui))
              '("exec:1" "exec:2"))
       "command rows retain scheduler creation order")
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader paints changed command state immediately")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore cursor))
        (test-assert
         (and (search "exec:1" text)
              (search "Run repository checks" text)
              (search "01:00" text)
              (search "exec:2" text)
              (search "Build the release" text)
              (search "queued" text)
              (search "review-3" text)
              (search "receiving response" text)
              (search (terminal-style-sequence ':command-spinner) display)
              (search (terminal-style-sequence ':command-id) display)
              (search (terminal-style-sequence ':command-tool) display))
         "commands render with child and provider activity"))
      (test-assert
       (<= (terminal-ui-live-row-count active-ui)
           (live-region-maximum-rows (terminal-ui-live-region active-ui)))
       "command and child strips share the live-region viewport budget")
      (setf clock 66)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "running command timers advance once per second")
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content active-ui clock)
        (declare (ignore display cursor))
        (test-assert
         (and (search "Run repository checks" text)
              (search "01:01" text))
         "command elapsed time advances from its observed duration"))
      (terminal-ui-set-command-activities active-ui nil)
      (test-assert
       (and (null (terminal-ui-command-activities active-ui))
            (null (terminal-ui-command-pending-completions active-ui)))
       "painted command rows clear at terminal state")))
  (let* ((clock 0)
         (terminal (make-instance 'recording-terminal :columns 80 :rows 8))
         (ui (terminal-ui-create
              :terminal terminal
              :clock-function (lambda () clock)))
         (command-a
           (list :id "exec:1" :type ':tool :index 1
                 :tool "shell.run" :description "Command A"
                 :state ':running :duration-ms 0 :detached nil))
         (command-b
           (list :id "exec:2" :type ':tool :index 2
                 :tool "shell.run" :description "Command B"
                 :state ':running :duration-ms 0 :detached nil)))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (terminal-ui-set-command-activities
       active-ui (list command-a command-b))
      (terminal-ui-set-command-activities active-ui (list command-b))
      (terminal-ui-set-command-activities active-ui nil)
      (test-assert
       (equal
        (mapcar (lambda (activity) (getf activity :id))
                (terminal-ui-command-pending-completions active-ui))
        '("exec:1" "exec:2"))
       "concurrent fast completions are retained independently")
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader paints the first completed command")
      (let ((output (recording-terminal-output terminal)))
        (test-assert
         (and (search "exec:1" output)
              (search "Command A" output)
              (not (search "exec:2" output)))
         "the first command reaches its first viewport-limited paint"))
      (test-assert
       (equal
        (mapcar (lambda (activity) (getf activity :id))
                (terminal-ui-command-pending-completions active-ui))
        '("exec:2"))
       "an unpainted completion survives for the following frame")
      (recording-terminal-reset terminal)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the reader advances to the remaining completed command")
      (let ((output (recording-terminal-output terminal)))
        (test-assert
         (and (search "exec:2" output)
              (search "Command B" output))
         "the second command reaches its own first paint"))
      (test-assert
       (null (terminal-ui-command-pending-completions active-ui))
       "paint releases each pending command completion")
      (recording-terminal-reset terminal)
      (test-assert (terminal-ui-refresh-status active-ui)
                   "the following frame removes completed command rows")
      (let ((output (recording-terminal-output terminal)))
        (test-assert
         (and (not (search "Command A" output))
              (not (search "Command B" output)))
         "completed command rows do not persist after their first paint"))))
  nil)

(-> test-terminal-stream-update () null)
(defun test-terminal-stream-update ()
  "Test continuous streamed blocks, fluid tail repaint, and block completion."
  (let* ((terminal (make-instance 'recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal :placeholder "hint")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-cursor-visible active-ui nil)
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update
       active-ui
       :rows (list (list (terminal-span :brand "● autolith"))
                   (list (terminal-span :plain "  first line")))
       :tail "  partial")
      (let ((output (recording-terminal-output terminal)))
        (test-assert (= (length (recording-terminal-chunks terminal)) 1)
                     "committed rows and tail use one terminal write")
        (test-assert (search "● autolith" output)
                     "streamed rows append the block header")
        (test-assert (search "  first line" output)
                     "streamed rows append committed lines")
        (test-assert (search "  partial" output)
                     "the fluid tail is painted live")
        (test-assert
         (zerop (terminal-tests--substring-count
                 (format nil "~C[?25h" *terminal-escape-character*)
                 output))
         "streaming leaves cursor motion hidden")
        (test-assert (not (terminal-tests--forbidden-control-p output))
                     "streamed rows never erase the display"))
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update
       active-ui
       :tail (list (list (terminal-span ':plain "  alpha beta gamma"))
                   (list (terminal-span ':plain "  delta epsilon"))))
      (let* ((output (recording-terminal-output terminal))
             (first-row (search "  alpha beta gamma" output))
             (second-row (search "  delta epsilon" output)))
        (test-assert (and first-row second-row (< first-row second-row))
                     "a fluid update paints every speculative wrapped row"))
      (terminal-ui-set-cursor-visible active-ui t)
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update active-ui :tail "  partial response")
      (let ((output (recording-terminal-output terminal)))
        (test-assert (= (length (recording-terminal-chunks terminal)) 1)
                     "a fluid-tail update is one terminal write")
        (test-assert
         (= (terminal-tests--substring-count
             (format nil "~C[?25l" *terminal-escape-character*)
             output)
            1)
         "a fluid-tail update hides cursor motion once")
        (test-assert
         (= (terminal-tests--substring-count
             (format nil "~C[?25h" *terminal-escape-character*)
             output)
            1)
         "a fluid-tail update restores the input cursor once"))
      (terminal-ui-set-cursor-visible active-ui nil)
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update active-ui :rows (list nil) :tail nil)
      (test-assert (not (search "partial" (recording-terminal-output terminal)))
                   "completing a block removes the fluid tail")
      (test-assert (null (terminal-ui-stream-tail active-ui))
                   "a completed block clears the stored tail")
      (terminal-ui-set-cursor-visible active-ui t)
      (test-assert (live-region-cursor-visible-p
                    (terminal-ui-live-region active-ui))
                   "the input cursor can be restored after streaming")))
  (let* ((terminal (make-instance 'recording-terminal
                                  :columns 40
                                  :rows 10))
         (ui (terminal-ui-create :terminal terminal :placeholder "hint")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "receiving response")
      (terminal-ui-set-notice active-ui "Notice" :duration-seconds 100)
      (terminal-ui-set-pending-inputs
       active-ui
       '("steer one" "steer two" "steer three")
       '("follow one" "follow two" "follow three"))
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update active-ui :tail "  visible response")
      (let ((output (recording-terminal-output terminal)))
        (test-assert (search "  visible response" output)
                     "pending inputs cannot crop the complete streamed response")
        (test-assert (search "hint" output)
                     "the short viewport keeps the editable prompt visible"))))
  nil)

(-> test-terminal-command-completion () null)
(defun test-terminal-command-completion ()
  "Test suggestion filtering, selection movement, acceptance, and submission."
  (let* ((terminal (make-instance 'recording-terminal :columns 60))
         (completions
           '((:name "/help" :argument nil :description "show this reference")
             (:name "/resume" :argument "ID" :description "load a conversation")
             (:name "/rollback" :argument "ID" :description "select a generation")
             (:name "/quit" :argument nil :description "leave Autolith")))
         (ui (terminal-ui-create :terminal terminal
                                 :completions completions)))
    (with-terminal-ui (active-ui ui)
      (let ((editor (terminal-ui-editor active-ui)))
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui '(:insert "/r"))
        (let ((painted (recording-terminal-output terminal)))
          (test-assert (search "/resume ID" painted)
                       "typing a command prefix paints matching suggestions")
          (test-assert (search "/rollback ID" painted)
                       "every matching command is suggested")
          (test-assert (not (search "/quit" painted))
                       "commands outside the typed prefix are not suggested"))
          (recording-terminal-reset terminal)
          (terminal-ui-process-event active-ui :escape)
          (test-assert (not (search "/resume ID"
                                    (recording-terminal-output terminal)))
                       "escape hides a passive completion menu")
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "tab cycles to and previews the next command")
        (test-assert (terminal-ui-completion-active-p active-ui)
                     "tab keeps command completion selection active")
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "/resume ")
                     "repeated tab cycles through command completions")
        (terminal-ui-process-event
         active-ui
         :complete-previous
         :queue-completion-p t
         :queue-editing-p t)
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "command completions take precedence over follow-up cycling")
        (terminal-ui-process-event active-ui :complete)
        (terminal-ui-process-event active-ui '(:insert "draft"))
        (test-assert (string= (line-editor-text editor) "/resume draft")
                     "ordinary input retains the selected completion")
        (test-assert (not (terminal-ui-completion-active-p active-ui))
                     "ordinary input dismisses completion selection")
        (terminal-ui-process-event active-ui :interrupt)
        (terminal-ui-process-event active-ui '(:insert "/r"))
        (terminal-ui-process-event active-ui :history-next)
        (test-assert (not (terminal-ui-completion-active-p active-ui))
                     "history keys do not hijack an unbegun completion")
        (terminal-ui-process-event active-ui :down)
        (test-assert (terminal-ui-completion-active-p active-ui)
                     "arrow keys begin completion for a typed command prefix")
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "arrow keys move the completion selection")
        (terminal-ui-process-event active-ui :escape)
        (test-assert (string= (line-editor-text editor) "/r")
                     "escape restores the prefix from before completion")
        (terminal-ui-process-event active-ui :interrupt)
        (terminal-ui-process-event active-ui '(:insert "/help"))
        (terminal-ui-process-event active-ui :submit)
        (terminal-ui-process-event active-ui '(:insert "/quit"))
        (terminal-ui-process-event active-ui :submit)
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui :history-previous)
        (test-assert (string= (line-editor-text editor) "/quit")
                     "history recall restores the newest command")
        (test-assert (not (terminal-ui-completion-active-p active-ui))
                     "history recall does not begin completion")
        (test-assert (not (search "leave Autolith"
                                  (recording-terminal-output terminal)))
                     "history recall does not paint command suggestions")
         (test-assert
          (not (terminal-ui-completion-menu-present-p active-ui))
          "history traversal makes retained completion state non-renderable")
         (test-assert
          (null (selector-items (terminal-ui-completion-selector active-ui)))
          "history traversal clears stale completion selector items")
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui :up)
        (test-assert (string= (line-editor-text editor) "/help")
                     "arrows continue history through recalled commands")
        (test-assert (not (search "show this reference"
                                  (recording-terminal-output terminal)))
                     "continued history recall still hides command suggestions")
        (terminal-ui-process-event active-ui :complete)
        (test-assert (terminal-ui-completion-active-p active-ui)
                     "tab can still begin completion on a recalled command")
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui :escape)
        (test-assert (not (search "show this reference"
                                  (recording-terminal-output terminal)))
                     "escape hides the completion menu")
        (test-assert (string= (line-editor-text editor) "/help")
                     "escape restores the recalled history entry")
        (test-assert (terminal-ui--editor-history-navigating-p editor)
                     "escape restores history traversal after completion")
        (terminal-ui-process-event active-ui :down)
        (test-assert (string= (line-editor-text editor) "/quit")
                     "history navigation continues after completion is cancelled")
        (terminal-ui-process-event active-ui :interrupt)
        (terminal-ui-process-event active-ui '(:insert "/q"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event active-ui :submit)
          (test-assert (eq action :submit)
                       "enter on an argument-free suggestion submits")
          (test-assert (string= payload "/quit")
                       "enter submits the completed command name"))
        (terminal-ui-process-event active-ui '(:insert "plain text"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event active-ui :complete)
          (test-assert (eq action ':submit)
                       "idle tab submits outside command completion")
          (test-assert (string= payload "plain text")
                       "idle tab submits the complete editor contents"))
        (terminal-ui-process-event active-ui '(:insert "queued follow-up"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui :complete :queue-completion-p t)
          (test-assert (eq action :queue)
                       "tab queues a non-empty draft while a turn is active")
          (test-assert (string= payload "queued follow-up")
                       "queued submission returns the complete draft"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui :complete :queue-completion-p t)
          (declare (ignore payload))
          (test-assert (eq action ':edit-queue)
                       "empty active-turn tab requests queued follow-up editing"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui :complete-previous :queue-completion-p t)
          (declare (ignore payload))
          (test-assert
           (not (eq action ':cycle-queue))
           "shift-tab does not cycle without a recalled follow-up"))
        (terminal-ui-set-input active-ui "")
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui :complete :queue-editing-p t)
          (declare (ignore payload))
          (test-assert
           (eq action ':kept)
           "empty tab keeps an already recalled follow-up selected"))
        (terminal-ui-set-input active-ui "edited follow-up")
        (multiple-value-bind (action payload)
            (terminal-ui-process-event
             active-ui
             :complete-previous
             :queue-editing-p t)
          (test-assert (eq action ':cycle-queue)
                       "shift-tab requests recalled follow-up cycling")
          (test-assert (string= payload "edited follow-up")
                       "follow-up cycling snapshots the edited draft"))
        (let ((image
                (merge-pathnames "follow-up-cycle.png"
                                 (uiop:temporary-directory))))
          (terminal-ui-set-input
           active-ui
           (user-message-input-create
            :text "[Image #1] revise"
            :image-pathnames (list image)))
          (multiple-value-bind (action payload)
              (terminal-ui-process-event
               active-ui
               :complete-previous
               :queue-editing-p t)
            (test-assert
             (and (eq action ':cycle-queue)
                  (typep payload 'user-message-input)
                  (equal (user-message-input-image-pathnames payload)
                         (list image)))
             "follow-up cycling preserves image attachments"))))))
  (let* ((terminal (make-instance 'recording-terminal :columns 60))
         (completions
           '((:name "/help" :argument nil :description "show this reference")))
         (ui (terminal-ui-create
              :terminal terminal
              :completion-function (lambda () completions))))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-process-event active-ui '(:insert "/n"))
      (test-assert
       (not (search "/new" (recording-terminal-output terminal)))
       "a dynamic completion provider initially omits unregistered commands")
      (setf completions
            '((:name "/help" :argument nil :description "show this reference")
              (:name "/new" :argument nil :description "start a conversation")))
      (test-assert
       (find "/new"
             (terminal-ui--matching-completions active-ui)
             :key (lambda (entry) (getf entry :name))
             :test #'string=)
       "dynamic command completion observes registry changes without rebuilding UI")))
  nil)

(-> test-terminal-lisp-operation-completion () null)
(defun test-terminal-lisp-operation-completion ()
  "Test registered parenthesized operation suggestions and acceptance."
  (let* ((terminal (make-instance 'recording-terminal :columns 72))
          (completions
            '((:name "/help" :argument nil :description "show this reference")
              (:name "/ste on" :argument nil :description "enable STE")
              (:name "/ste off" :argument nil :description "disable STE")
              (:name "(help)" :argument nil :description "show this reference")
              (:name "(ste \"on\")" :argument nil :description "enable STE")
              (:name "(ste \"off\")" :argument nil :description "disable STE")
              (:name "(resource.read" :argument ":uri URI)"
               :description "read one resource")))
         (ui (terminal-ui-create :terminal terminal :completions completions)))
    (with-terminal-ui (active-ui ui)
      (let ((editor (terminal-ui-editor active-ui)))
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui '(:insert "(r"))
        (let ((painted (recording-terminal-output terminal)))
          (test-assert (search "(resource.read :uri URI)" painted)
                       "typing an opening parenthesis suggests registered tools")
          (test-assert (not (search "(help)" painted))
                       "parenthesized completion filters unrelated operations"))
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "(resource.read ")
                     "tool completion inserts the canonical Lisp function name")
        (terminal-ui--cancel-completion active-ui)
        (terminal-ui-set-input active-ui "(h")
        (multiple-value-bind (action payload)
            (terminal-ui-process-event active-ui :submit)
          (test-assert (eq action ':submit)
                       "enter accepts an argument-free Lisp operation")
          (test-assert (string= payload "(help)")
                       "accepted Lisp completion includes its closing parenthesis"))
        (terminal-ui-set-input active-ui " (h")
        (test-assert (null (terminal-ui--matching-completions active-ui))
                     "leading whitespace preserves prose without Lisp completion")
        (terminal-ui-set-input active-ui "(resource.read :uri")
        (test-assert (null (terminal-ui--matching-completions active-ui))
                     "operation completion stops after the function name")
        (terminal-ui-set-input active-ui "/ste ")
        (test-assert
         (equal (mapcar (lambda (entry) (getf entry :name))
                        (terminal-ui--matching-completions active-ui))
                '("/ste on" "/ste off"))
         "slash argument prefixes offer finite command options")
        (terminal-ui-set-input active-ui "(ste ")
        (test-assert
         (equal (mapcar (lambda (entry) (getf entry :name))
                        (terminal-ui--matching-completions active-ui))
                '("(ste \"on\")" "(ste \"off\")"))
         "Lisp argument prefixes offer the same finite command options"))))
  nil)

(-> test-terminal-modal-selection () null)
(defun test-terminal-modal-selection ()
  "Test modal picker navigation, acceptance, cancellation, and cleanup."
  (let ((selector (make-selector :visible-count 4 :arrangement ':vertical)))
    (selector-set-items
     selector
     '((:name "a" :argument nil :description "first")
       (:name "considerably-longer" :argument nil :description "second")))
    (let* ((rows (terminal-ui--choice-rows selector 50))
           (texts (mapcar #'test-terminal-row-text rows)))
      (test-assert (= (search "first" (first texts))
                      (search "second" (second texts)))
                   "picker descriptions share one content-aware value column")))
  (let ((selector (make-selector :visible-count 4 :arrangement ':vertical)))
    (selector-set-items
     selector
     (list
      (list :name "alpha"
            :argument nil
            :tally "2 turns"
            :description "2026-07-26 14:30 · first"
            :description-spans
            (list (terminal-span ':plain "2026-07-26 ")
                  (terminal-span ':timestamp-time "14:30")
                  (terminal-span ':plain " · first")))
      (list :name "beta"
            :argument nil
            :tally "1:02"
            :description "2026-07-25 09:15 · second"
            :description-spans
            (list (terminal-span ':plain "2026-07-25 ")
                  (terminal-span ':timestamp-time "09:15")
                  (terminal-span ':plain " · second")))))
    (let* ((rows (terminal-ui--choice-rows selector 70))
           (texts (mapcar #'test-terminal-row-text rows))
           (first-time
             (find "14:30" (first rows)
                   :key #'terminal-span-text
                   :test #'string=))
           (second-date
             (find "2026-07-25 " (second rows)
                   :key #'terminal-span-text
                   :test #'string=)))
      (test-assert (= (search "2026-07-26" (first texts))
                      (search "2026-07-25" (second texts)))
                   "picker tallies preserve one aligned description column")
      (test-assert (and first-time
                        (eq (terminal-span-style first-time) ':timestamp-time))
                   "picker descriptions preserve the timestamp time color")
      (test-assert (and second-date
                        (eq (terminal-span-style second-date) ':dim))
                   "unselected picker dates retain the ordinary dim style")))
  (test-assert
   (not (terminal-completion-p
         (list :name "broken"
               :argument nil
               :description "visible"
               :description-spans (list (terminal-span ':plain "different")))))
   "completion descriptions reject mismatched styled text")
  (let* ((items '((:name "alpha" :argument nil :description "first entry"
                   :group "current directory")
                  (:name "beta" :argument nil :description "second entry"
                   :group "current directory")
                  (:name "gamma" :argument nil :description "third entry"
                   :group "other sessions")))
         (terminal (make-instance 'scripted-terminal
                                  :columns 60
                                  :events (list :history-next :submit)))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (test-assert (string= (terminal-ui-select active-ui
                                                :title "pick one"
                                                :items items)
                            "beta")
                   "arrow keys move the modal selection before enter")
      (let ((painted (recording-terminal-output terminal)))
        (test-assert (search "pick one" painted)
                     "the picker paints its title")
        (test-assert (search "alpha" painted)
                     "the picker paints its items")
        (test-assert (and (search "current directory" painted)
                          (search "other sessions" painted))
                     "the picker paints nonselectable candidate groups")
        (test-assert (not (terminal-tests--forbidden-control-p painted))
                     "the picker never erases the display"))
      (test-assert (null (terminal-ui-selector active-ui))
                   "the selector state clears after selection")
      (recording-terminal-reset terminal)
      (setf (scripted-terminal-events terminal) (list :submit))
      (test-assert
       (string=
        (terminal-ui-select
         active-ui
         :title "pick one"
         :items '((:name "Display title"
                   :value "stable-id"
                   :argument nil
                   :description "titled entry")))
        "stable-id")
       "picker values can differ from their displayed names")
      (test-assert
       (and (search "Display title" (recording-terminal-output terminal))
            (not (search "stable-id" (recording-terminal-output terminal))))
       "picker values remain hidden behind their display titles")
      (setf (scripted-terminal-events terminal) (list :submit))
      (test-assert
       (string=
        (terminal-ui-select
         active-ui
         :title "pick one"
         :items '((:name "Duplicate title" :value "first-id"
                   :argument nil :description "first")
                  (:name "Duplicate title" :value "second-id"
                   :argument nil :description "second"))
         :initial-value "second-id")
        "second-id")
       "an initial stable value disambiguates duplicate display titles")
      (setf (scripted-terminal-events terminal) (list :escape))
      (test-assert (null (terminal-ui-select active-ui
                                             :title "pick one"
                                             :items items))
                   "escape cancels the picker")
      (setf (scripted-terminal-events terminal) (list :complete :submit))
      (test-assert (string= (terminal-ui-select active-ui
                                                :title "pick one"
                                                :items items)
                            "beta")
                   "tab cycles modal picker options")
      (setf (scripted-terminal-events terminal)
            (list :history-next '(:insert "x")))
      (test-assert (string= (terminal-ui-select active-ui
                                                :title "pick one"
                                                :items items)
                            "beta")
                   "ordinary picker input retains the selected option")
      (recording-terminal-reset terminal)
      (setf (scripted-terminal-events terminal) (list :submit))
      (let ((visible-count nil)
            (selected-name nil))
        (test-assert
         (string=
          (terminal-ui-select
           active-ui
           :title "pick one"
           :items items
           :visible-count 15
           :initial-name "gamma"
           :on-event
           (lambda (event selector)
             (when (eq event ':submit)
               (setf visible-count (selector-visible-count selector)
                     selected-name
                     (getf (nth (selector-selection selector)
                                (selector-items selector))
                           :name)))
             nil))
          "gamma")
         "modal pickers accept their initial named selection")
        (test-assert (= visible-count 15)
                     "modal pickers honor a custom visible row capacity")
        (test-assert (string= selected-name "gamma")
                     "the initial named selection is installed before input"))
      (recording-terminal-reset terminal)
      (setf (scripted-terminal-events terminal)
            (list '(:insert "SECOND") :submit))
      (test-assert
       (string= (terminal-ui-select active-ui
                                    :title "pick one"
                                    :items items
                                    :search-p t)
                "beta")
       "searchable pickers match visible metadata case-insensitively")
      (let ((painted (recording-terminal-output terminal)))
        (test-assert (and (search "search: SECOND" painted)
                          (search "1 match" painted))
                     "searchable pickers show their query and match count"))
      (setf (scripted-terminal-events terminal)
            (list '(:insert "betx") :backspace :submit))
      (test-assert
       (string= (terminal-ui-select active-ui
                                    :title "pick one"
                                    :items items
                                    :search-p t)
                "beta")
       "backspace restores matches after deleting one search grapheme")
      (setf (scripted-terminal-events terminal)
            (list '(:paste "no matches") :kill-line :history-next :submit))
      (test-assert
       (string= (terminal-ui-select active-ui
                                    :title "pick one"
                                    :items items
                                    :search-p t)
                "beta")
       "Ctrl-U clears pasted picker search and restores navigation")
      (setf (scripted-terminal-events terminal)
            (list '(:insert "absent") :submit :kill-line :submit))
      (test-assert
       (string= (terminal-ui-select active-ui
                                    :title "pick one"
                                    :items items
                                    :search-p t)
                "alpha")
       "submitting an empty search result keeps the picker open")
      (recording-terminal-reset terminal)
      (setf (scripted-terminal-events terminal)
            (list '(:insert "d") :submit))
      (let ((mode ':browse))
        (test-assert
         (string=
          (terminal-ui-select
           active-ui
           :title "browse"
           :items items
           :hint "d deletes"
           :search-p t
           :on-event
           (lambda (event selector)
             (declare (ignore selector))
             (ecase mode
               (:browse
                (when (equal event '(:insert "d"))
                  (setf mode ':confirm)
                  (list ':replace
                        "confirm"
                        '((:name "delete"
                           :argument nil
                           :description "confirm deletion"))
                        "enter confirms")))
               (:confirm
                (when (eq event ':submit)
                  (list ':accept "deleted"))))))
          "deleted")
         "picker event hooks can replace and accept modal choices")
        (let ((painted (recording-terminal-output terminal)))
          (test-assert (and (search "d deletes" painted)
                            (search "enter confirms" painted))
                       "picker replacements repaint their contextual hints"))
        (test-assert (null (terminal-ui-selector-hint active-ui))
                     "picker cleanup clears its contextual hint"))
      (setf (scripted-terminal-events terminal)
            (list '(:insert "c") :submit))
      (let ((mode ':browse)
            (replacement-hint :unobserved))
        (test-assert
         (string=
          (terminal-ui-select
           active-ui
           :title "browse"
           :items items
           :hint "temporary hint"
           :on-event
           (lambda (event selector)
             (declare (ignore selector))
             (ecase mode
               (:browse
                (when (equal event '(:insert "c"))
                  (setf mode ':replaced)
                  (list ':replace "cleared" items nil)))
               (:replaced
                (when (eq event ':submit)
                  (setf replacement-hint
                        (terminal-ui-selector-hint active-ui))
                  (list ':accept "cleared"))))))
          "cleared")
         "picker replacements accept an explicit NIL hint")
        (test-assert (null replacement-hint)
                     "an explicit NIL replacement clears the previous hint"))))
  (let* ((terminal (make-instance 'scripted-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal)))
    (test-assert (null (terminal-ui-select
                        ui
                        :title "pick"
                        :items '((:name "a" :argument nil :description "d"))))
                 "non-interactive terminals never open the picker"))
  nil)

(-> terminal-tests--call-without-host-size (function) t)
(defun terminal-tests--call-without-host-size (function)
  "Call FUNCTION while kernel and tput terminal sizes are unavailable.

Resize tests drive size changes through COLUMNS and LINES, which the real
resolution only consults after the kernel and tput sizes. Masking those host
sources keeps the tests deterministic under an interactive terminal."
  (test-call-with-function-replacements
   (list (list 'terminal-file-descriptor-size
               (lambda (file-descriptor)
                 (declare (ignore file-descriptor))
                 (values nil nil)))
         (list 'terminal--query-dimension
               (lambda (capability)
                 (declare (ignore capability))
                 nil)))
   function))

(-> test-terminal-modal-resize () null)
(defun test-terminal-modal-resize ()
  "Test that a resize raised during modal input repaints before event dispatch."
  (let* ((previous-columns (uiop:getenv "COLUMNS"))
         (previous-lines (uiop:getenv "LINES"))
         (*terminal-resize-pending-p* nil)
         (terminal
           (make-instance
            'scripted-terminal
            :columns 60
            :events (list :submit)
            :read-callback
            (lambda ()
              (sb-posix:setenv "COLUMNS" "18" 1)
              (sb-posix:setenv "LINES" "8" 1)
              (setf *terminal-resize-pending-p* t))))
         (ui (terminal-ui-create :terminal terminal))
         (items '((:name "alpha" :argument nil :description "first entry"))))
    (unwind-protect
         (terminal-tests--call-without-host-size
          (lambda ()
            (with-terminal-ui (active-ui ui)
              (recording-terminal-reset terminal)
              (test-assert
               (string= (terminal-ui-select
                         active-ui
                         :title "pick"
                         :items items
                         :resize-callback
                         #'application-pending-terminal-size)
                        "alpha")
               "submit still accepts the picker event received during resize")
              (test-assert (= (terminal-columns terminal) 18)
                           "a pending picker resize refreshes terminal columns")
              (test-assert (= (terminal-rows terminal) 8)
                           "a pending picker resize refreshes terminal rows")
              (test-assert (null *terminal-resize-pending-p*)
                           "the picker consumes the pending resize flag")
              (test-assert
               (= (terminal-tests--substring-count
                   "pick"
                   (recording-terminal-output terminal))
                  2)
               "the picker repaints at the new width before submit exits"))))
      (if previous-columns
          (sb-posix:setenv "COLUMNS" previous-columns 1)
          (sb-posix:unsetenv "COLUMNS"))
      (if previous-lines
          (sb-posix:setenv "LINES" previous-lines 1)
          (sb-posix:unsetenv "LINES"))))
  nil)

(-> test-terminal-application-read-resize () null)
(defun test-terminal-application-read-resize ()
  "Test that the outer application refreshes size before dispatching a read event."
  (let* ((previous-columns (uiop:getenv "COLUMNS"))
         (previous-lines (uiop:getenv "LINES"))
         (*terminal-resize-pending-p* nil)
         (terminal
           (make-instance
            'scripted-terminal
            :columns 60
            :events (list :submit)
            :read-callback
            (lambda ()
              (sb-posix:setenv "COLUMNS" "19" 1)
              (sb-posix:setenv "LINES" "9" 1)
              (setf *terminal-resize-pending-p* t))))
         (ui (terminal-ui-create :terminal terminal)))
    (unwind-protect
         (terminal-tests--call-without-host-size
          (lambda ()
            (with-terminal-ui (active-ui ui)
              (test-assert
               (eq (application-read-terminal-event active-ui) :submit)
               "the application preserves the event read during resize")
              (test-assert
               (= (terminal-columns terminal) 19)
               "the application refreshes width before event dispatch")
              (test-assert
               (= (terminal-rows terminal) 9)
               "the application refreshes height before event dispatch")
              (test-assert
               (null *terminal-resize-pending-p*)
               "the application consumes a resize raised during read"))))
      (if previous-columns
          (sb-posix:setenv "COLUMNS" previous-columns 1)
          (sb-posix:unsetenv "COLUMNS"))
      (if previous-lines
          (sb-posix:setenv "LINES" previous-lines 1)
          (sb-posix:unsetenv "LINES"))))
  nil)


(-> test-terminal-non-tty-fallback () null)
(defun test-terminal-non-tty-fallback ()
  "Test line-oriented fallback input and output on a non-TTY descriptor."
  (test-assert (not (terminal--interactive-file-descriptor-p -1))
               "a missing descriptor is not interactive")
  (let ((terminal
          (stream-terminal-create
           :input-stream (make-string-input-stream "")
           :output-stream (make-string-output-stream)
           :input-file-descriptor -1)))
    (terminal-start terminal)
    (test-assert (terminal-started-p terminal)
                 "a noninteractive stream starts without inspecting its descriptor")
    (test-assert (not (terminal-interactive-p terminal))
                 "a noninteractive stream selects fallback mode")
    (terminal-stop terminal))
  (let ((null-descriptor (sb-posix:open "/dev/null" sb-posix:o-rdonly)))
    (unwind-protect
         (test-assert
          (not (terminal--interactive-file-descriptor-p null-descriptor))
          "/dev/null is not an interactive terminal")
      (sb-posix:close null-descriptor)))
  (multiple-value-bind (read-descriptor write-descriptor)
      (sb-posix:pipe)
    (unwind-protect
         (let* ((output (make-string-output-stream))
                (terminal
                  (stream-terminal-create
                   :input-stream (make-string-input-stream
                                  (format nil "fallback input~%"))
                   :output-stream output
                   :input-file-descriptor read-descriptor
                   :columns 20))
                (ui (terminal-ui-create :terminal terminal)))
           (terminal-ui-start ui)
           (test-assert (not (terminal-interactive-p terminal))
                        "a pipe selects the non-TTY fallback")
           (multiple-value-bind (action submitted)
               (terminal-ui-process-event ui (terminal-ui-read-event ui))
             (test-assert (eq action :submit)
                          "fallback line input produces a submit action")
             (test-assert (string= submitted "fallback input")
                          "fallback line input preserves its text"))
           (terminal-ui-set-status ui "not printed")
           (terminal-ui-append-finalized ui 1 "fallback output")
           (terminal-ui-stop ui)
           (let ((captured (get-output-stream-string output)))
             (test-assert (search "fallback output" captured)
                          "fallback mode writes finalized transcript output")
             (test-assert (not (find *terminal-escape-character* captured))
                          "fallback mode emits no terminal controls")))
      (sb-posix:close read-descriptor)
      (sb-posix:close write-descriptor)))
  nil)

(-> test-terminal-descriptor-tty-detection () null)
(defun test-terminal-descriptor-tty-detection ()
  "Test TTY detection from the file descriptor rather than stream class."
  (let ((process nil)
        (terminal nil))
    (unwind-protect
         (progn
           (setf process
                 (sb-ext:run-program "/bin/sh"
                                     '("-c" "sleep 10")
                                     :pty t
                                     :wait nil))
           (let ((pty (sb-ext:process-pty process)))
             (setf terminal
                   (stream-terminal-create
                    :input-stream (make-string-input-stream "")
                    :output-stream pty
                    :input-file-descriptor (sb-sys:fd-stream-fd pty)))
             (test-assert
              (not (interactive-stream-p
                    (stream-terminal-input-stream terminal)))
              "a wrapped input stream can be noninteractive while its descriptor is a TTY")
             (terminal-start terminal)
             (test-assert (terminal-interactive-p terminal)
                          "a TTY descriptor selects interactive mode")
             (terminal-stop terminal)
             (terminal-start terminal)
             (test-assert (terminal-interactive-p terminal)
                          "restarting preserves descriptor-based interactive mode")
             (terminal-stop terminal)))
      (when (and terminal (terminal-started-p terminal))
        (terminal-stop terminal))
      (when process
        (ignore-errors (sb-ext:process-kill process 15))
        (ignore-errors (sb-ext:process-wait process)))))
  nil)

(-> test-terminal-prompt-markers () null)
(defun test-terminal-prompt-markers ()
  "Test OSC 133 terminal output and its semantic UI lifecycle."
  (flet ((expected-marker (payload)
           (format nil "~C]133;~A~C~C"
                   *terminal-escape-character*
                   payload
                   *terminal-escape-character*
                   #\\)))
    (let ((prompt-start (expected-marker "A;redraw=0"))
          (input-start (expected-marker "B"))
          (execution-start (expected-marker "C"))
          (success (expected-marker "D;0"))
          (failure (expected-marker "D;7")))
      (let ((terminal
              (make-instance 'protocol-recording-stream-terminal
                             :input-stream (make-string-input-stream "")
                             :output-stream (make-string-output-stream)
                             :input-file-descriptor 0
                             :interactive-p t)))
        (test-assert
         (and (terminal-write-prompt-marker terminal ':prompt-start)
              (equal (reverse
                      (protocol-recording-stream-terminal-chunks terminal))
                     (list prompt-start))
              (= (protocol-recording-stream-terminal-write-count terminal) 1)
              (= (protocol-recording-stream-terminal-flush-count terminal) 1))
         "an interactive prompt marker writes exactly once and flushes once")
        (setf (terminal-interactive-p terminal) nil)
        (test-assert
         (and (not (terminal-write-prompt-marker terminal ':input-start))
              (= (protocol-recording-stream-terminal-write-count terminal) 1)
              (= (protocol-recording-stream-terminal-flush-count terminal) 1))
         "a noninteractive terminal emits and flushes no prompt marker"))
      (let* ((terminal
               (make-instance 'recording-terminal
                              :columns 72
                              :rows 24
                              :styled-p t))
             (ui (terminal-ui-create :terminal terminal)))
        (with-terminal-ui (active-ui ui)
          (terminal-ui-stream-update
           active-ui
           :tail (format nil "stale first row~%stale second row"))
          (let ((painted-row-count (terminal-ui-live-row-count active-ui)))
            (recording-terminal-reset terminal)
            (test-assert
             (and (terminal-ui-open-prompt-block active-ui)
                  (not (terminal-ui-open-prompt-block active-ui)))
             "one idle prompt emits its boundaries only once")
            (let* ((chunks (reverse (recording-terminal-chunks terminal)))
                   (prompt-position (position prompt-start chunks
                                              :test #'string=))
                   (retraction
                     (and prompt-position
                          (plusp prompt-position)
                          (elt chunks (1- prompt-position)))))
              (test-assert
               (and (> painted-row-count 1)
                    retraction
                    (= (count #\Return retraction) painted-row-count)
                    (= (terminal-tests--substring-count
                        (format nil "~C[K" *terminal-escape-character*)
                        retraction)
                       painted-row-count)
                    (notany
                     (lambda (chunk)
                       (search "stale" chunk))
                     (subseq chunks 0 prompt-position)))
               "prompt start follows complete multi-row live-region retraction")))
          (terminal-ui--paint-live active-ui)
          (terminal-ui-set-status active-ui "working")
          (terminal-ui-refresh-status active-ui)
          (terminal-ui-stream-update active-ui :tail "unfinished")
          (terminal-ui-resize active-ui 64 :rows 20)
          (test-assert
           (and (terminal-ui-start-prompt-execution active-ui)
                (not (terminal-ui-start-prompt-execution active-ui)))
           "one submitted prompt starts execution only once")
          (terminal-ui--paint-live active-ui)
          (terminal-ui-set-status active-ui nil)
          (terminal-ui-stream-update active-ui :tail nil)
          (test-assert
           (and (terminal-ui-finish-prompt-block active-ui 7)
                (not (terminal-ui-finish-prompt-block active-ui 7))
                (terminal-ui-open-prompt-block active-ui))
           "one execution completes once before the next prompt opens")
          (let* ((output (recording-terminal-output terminal))
                 (first-prompt (search prompt-start output))
                 (first-input (search input-start output))
                 (execution (search execution-start output))
                 (completion (search failure output))
                 (second-prompt
                   (and first-prompt
                        (search prompt-start output
                                :start2 (+ first-prompt
                                           (length prompt-start)))))
                 (second-input
                   (and first-input
                        (search input-start output
                                :start2 (+ first-input
                                           (length input-start))))))
            (test-assert
             (and first-prompt first-input execution completion
                  second-prompt second-input
                  (< first-prompt first-input execution completion
                     second-prompt second-input)
                  (= (terminal-tests--substring-count prompt-start output) 2)
                  (= (terminal-tests--substring-count input-start output) 2)
                  (= (terminal-tests--substring-count execution-start output) 1)
                  (= (terminal-tests--substring-count failure output) 1))
             "repaints, ticks, streams, and resize preserve one ordered marker block"))))
      (let* ((terminal
               (make-instance 'recording-terminal :columns 40))
             (ui (terminal-ui-create :terminal terminal)))
        (terminal-ui-start ui)
        (unwind-protect
             (progn
               (setf (terminal-interactive-p terminal) nil)
               (recording-terminal-reset terminal)
               (test-assert
                (and (not (terminal-ui-open-prompt-block ui))
                     (not (terminal-ui-start-prompt-execution ui))
                     (not (terminal-ui-finish-prompt-block ui 1))
                     (zerop (length (recording-terminal-output terminal)))
                     (eq (terminal-ui-prompt-marker-state ui) ':closed))
                "a noninteractive UI has no prompt-marker state transitions"))
          (terminal-ui-stop ui)))
      (let* ((terminal
               (make-instance 'recording-terminal :columns 40))
             (ui (terminal-ui-create :terminal terminal))
             (stop-failure (expected-marker "D;1")))
        (terminal-ui-start ui)
        (recording-terminal-reset terminal)
        (terminal-ui-open-prompt-block ui)
        (terminal-ui-start-prompt-execution ui)
        (terminal-ui-stop ui)
        (terminal-ui-stop ui)
        (test-assert
         (and (= (terminal-tests--substring-count
                  stop-failure (recording-terminal-output terminal))
                 1)
              (eq (terminal-ui-prompt-marker-state ui) ':closed))
         "UI shutdown closes one unfinished execution with failure status"))))
  nil)

(-> run-terminal-tests () boolean)
(defun run-terminal-tests ()
  "Run focused terminal seam tests and return true when every assertion succeeds."
  (test-terminal-primary-screen-controls)
  (test-terminal-prompt-markers)
  (test-terminal-finalized-batch)
  (test-terminal-untrusted-text)
  (test-terminal-finalized-scrollback)
  (test-terminal-resize-frame)
  (test-terminal-line-editor)
  (test-terminal-history-replacement)
  (test-terminal-image-attachments)
  (test-terminal-input-decoding)
  (test-terminal-status-worked-time)
  (test-terminal-bounded-editor-repaint)
  (test-terminal-transient-notice)
  (test-terminal-notice-lock-contention)
  (test-terminal-timed-status)
  (test-terminal-compaction-indicator)
  (test-terminal-agent-activities)
  (test-terminal-command-activities)
  (test-terminal-stream-update)
  (test-terminal-command-completion)
  (test-terminal-lisp-operation-completion)
  (test-terminal-modal-selection)
  (test-terminal-modal-resize)
  (test-terminal-application-read-resize)
  (test-terminal-non-tty-fallback)
  (test-terminal-descriptor-tty-detection)
  t)

(-> test-terminal-status-worked-time () null)
(defun test-terminal-status-worked-time ()
  "Test the right-aligned total worked time on the status row."
  (let* ((columns 96)
         (terminal (make-instance 'recording-terminal
                                  :columns columns
                                  :styled-p t))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status
       active-ui
       "working"
       :details (list (terminal-span ':status-model "model"))
       :worked-seconds 3723)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content
           active-ui
           (terminal-ui-status-started-at active-ui))
        (declare (ignore display cursor))
        (let ((status-row
                (second (uiop:split-string text :separator '(#\Newline)))))
          (test-assert (= (text-cell-width status-row) columns)
                       "a worked status row spans the full terminal width")
          (test-assert (search "worked 1:02:03" status-row)
                       "the status row shows accumulated plus elapsed work")
          (test-assert
           (let ((start (search "worked 1:02:03" status-row)))
             (and start
                  (= (+ start (length "worked 1:02:03"))
                     (length status-row))))
           "total worked time is right-aligned on the status row")))
      (terminal-ui-set-status active-ui nil)
      (test-assert (null (terminal-ui-status-worked-seconds active-ui))
                   "clearing the status clears the worked baseline")))
  (let* ((columns 24)
         (terminal (make-instance 'recording-terminal
                                  :columns columns
                                  :styled-p t))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working" :worked-seconds 3723)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content
           active-ui
           (terminal-ui-status-started-at active-ui))
        (declare (ignore display cursor))
        (let ((status-row
                (second (uiop:split-string text :separator '(#\Newline)))))
          (test-assert (not (search "worked" status-row))
                       "a narrow status row drops the worked segment first")))))
  nil)
