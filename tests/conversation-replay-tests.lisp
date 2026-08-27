(in-package #:autolith)

;;;; -- Read-only Conversation Replay Tests --

(-> conversation-replay-tests--session () conversation-replay-session)
(defun conversation-replay-tests--session ()
  "Return deterministic replay navigation state spanning two local dates."
  (let* ((first-time (encode-universal-time 0 0 9 27 8 2026))
         (second-time (encode-universal-time 0 30 10 28 8 2026))
         (conversation
           (make-instance 'conversation
                          :identifier "replay-test"
                          :prompt-cache-key "replay-test"
                          :pathname #p"/tmp/replay-test.sexp"
                          :log-pathname #p"/tmp/replay-test.sexp"
                          :persisted-p nil
                          :created-at first-time
                          :next-sequence 1
                          :input-items nil))
         (records
           (vector
            (make-instance 'conversation-replay-record
                           :record (list ':message :seq 10 :time first-time
                                         :role ':user :content "first")
                           :turn 1)
            (make-instance 'conversation-replay-record
                           :record (list ':assistant :seq 11 :time (1+ first-time)
                                         :content "answer")
                           :turn 1)
            (make-instance 'conversation-replay-record
                           :record (list ':message :seq 20 :time second-time
                                         :role ':user :content "second")
                           :turn 2))))
    (make-instance 'conversation-replay-session
                   :conversation conversation
                   :records records)))

(-> test-conversation-replay-navigation () null)
(defun test-conversation-replay-navigation ()
  "Test record stepping and turn, sequence, date, and time selection."
  (let ((session (conversation-replay-tests--session)))
    (conversation-replay-select-turn session 2)
    (test-assert
     (= (conversation-replay--record-sequence
         (conversation-replay--current-record session))
        20)
     "turn selection lands on the turn's first replay record")
    (conversation-replay-select-sequence session 11)
    (test-assert
     (= (conversation-replay--record-sequence
         (conversation-replay--current-record session))
        11)
     "sequence selection lands on the first record at or after the target")
    (conversation-replay-select-date session "2026-08-28")
    (test-assert
     (= (conversation-replay-record-turn
         (conversation-replay--current-record session))
        2)
     "date selection uses local record dates")
    (conversation-replay-select-time session "10:30")
    (test-assert
     (= (conversation-replay--record-sequence
         (conversation-replay--current-record session))
        20)
     "bare time selection uses the selected record's local date")
    (conversation-replay-move session -2)
    (let ((output (make-string-output-stream)))
      (test-assert
       (conversation-replay-execute-command session "next" output)
       "next continues the replay session")
      (test-assert
       (search "sequence 11" (get-output-stream-string output))
       "next renders the newly selected replay record")
      (test-assert
       (not (conversation-replay-execute-command session "quit" output))
       "quit terminates the replay session")))
  nil)

(-> test-conversation-replay-projection () null)
(defun test-conversation-replay-projection ()
  "Test provider replay projection keeps visible content and drops opaque payloads."
  (let* ((wire-json
           (json-encode
            (json-object
             "type" "message"
             "role" "assistant"
             "content" (vector (json-object "type" "output_text"
                                             "text" "projected answer"))
             "opaque" (make-string 10000 :initial-element #\x))))
         (record
           (list ':provider-item :seq 7 :time 100 :wire-json wire-json))
         (projected (conversation-replay--project-record record)))
    (test-assert
     (and (eq (first projected) ':assistant)
          (string= (getf (rest projected) :content) "projected answer")
          (< (length (prin1-to-string projected)) 200))
     "provider replay stores only bounded visible assistant content"))
    (test-assert
     (null
      (conversation-replay--project-record
       (list ':native-compaction
             :seq 8
             :time 101
             :wire-json (make-string 10000 :initial-element #\x))))
     "opaque native compaction payloads are omitted from replay projection")
  nil)

(-> test-conversation-replay-storage () null)
(defun test-conversation-replay-storage ()
  "Test standalone replay reads a saved conversation without mutating its files."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "read-only-replay")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "inspect me")
           (let* ((pathname (conversation-log-pathname conversation))
                  (write-date (file-write-date pathname))
                  (length (with-open-file (stream pathname)
                            (file-length stream)))
                  (output (make-string-output-stream)))
             (conversation-replay-run configuration
                                      "read-only-replay"
                                      nil
                                      :output output
                                      :interactive-p nil)
             (test-assert
              (and (= write-date (file-write-date pathname))
                   (= length
                      (with-open-file (stream pathname)
                        (file-length stream)))
                   (search "inspect me" (get-output-stream-string output)))
              "replay reads and renders a conversation without writing storage")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-conversation-replay-command-line () null)
(defun test-conversation-replay-command-line ()
  "Test the top-level replay command forwards its identifier and selector."
  (let ((observed nil))
    (test-call-with-function-replacements
     (list
      (list 'conversation-replay-run
            (lambda (configuration identifier selection &rest arguments)
              (declare (ignore arguments))
              (setf observed
                    (list (configuration-immutable-p configuration)
                          identifier
                          selection)))))
     (lambda ()
       (main-dispatch '("replay" "abc123" "turn" "4"))))
    (test-assert
     (equal observed '(t "abc123" ("turn" "4")))
     "the replay command starts an immutable selected inspection"))
  nil)

(-> test-conversation-replay () null)
(defun test-conversation-replay ()
  "Run read-only conversation replay tests."
  (test-conversation-replay-navigation)
  (test-conversation-replay-projection)
  (test-conversation-replay-storage)
  (test-conversation-replay-command-line)
  nil)
