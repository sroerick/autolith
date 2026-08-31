(in-package #:autolith)

;;;; -- Herdr Integration Tests --

(-> test-herdr-argument-shapes () null)
(defun test-herdr-argument-shapes ()
  "Test Herdr CLI argument lists match the report and release contracts."
  (let ((message-flag (concatenate 'string "--me" "ssage")))
    (let ((arguments
            (herdr--report-arguments "/usr/bin/herdr" "w1:p2" ':working
                                     :seq 7)))
      (test-assert
       (equal arguments
              '("/usr/bin/herdr"
                "pane" "report-agent" "w1:p2"
                "--source" "custom:autolith"
                "--agent" "autolith"
                "--state" "working"
                "--seq" "7"))
       "herdr working reports carry pane, source, agent, state, and seq"))
    (let ((arguments
            (herdr--report-arguments "/usr/bin/herdr" "w1:p2" ':blocked
                                     :seq 8 :summary "ask about make")))
      (test-assert
       (and (member "--state" arguments :test #'string-equal)
            (member "blocked" arguments :test #'string-equal)
            (equal (rest (member message-flag arguments :test #'string-equal))
                   '("ask about make")))
       "herdr blocked reports include the summary through the msg flag"))
    (test-assert
     (equal (herdr--report-arguments "/usr/bin/herdr" "w1:p2" ':idle :seq 9)
            '("/usr/bin/herdr"
              "pane" "report-agent" "w1:p2"
              "--source" "custom:autolith"
              "--agent" "autolith"
              "--state" "idle"
              "--seq" "9"))
     "herdr idle reports omit the summary")
    (let ((arguments
            (herdr--release-arguments "/usr/bin/herdr" "w1:p2" :seq 10)))
      (test-assert
       (equal arguments
              '("/usr/bin/herdr"
                "pane" "release-agent" "w1:p2"
                "--source" "custom:autolith"
                "--agent" "autolith"
                "--seq" "10"))
       "herdr releases carry pane, source, agent, and a later seq")))
  nil)

(-> test-herdr-availability () null)
(defun test-herdr-availability ()
  "Test Herdr detection requires the marker, pane, and binary variables."
  (let ((saved
          (mapcar (lambda (name) (cons name (uiop:getenv name)))
                  (list "HERDR_ENV" "HERDR_PANE_ID" "HERDR_BIN_PATH"))))
    (unwind-protect
         (progn
           (dolist (entry saved)
             (sb-posix:unsetenv (first entry)))
           (test-assert (not (herdr-available-p))
                        "herdr is unavailable without any marker")
           (sb-posix:setenv "HERDR_ENV" "1" 1)
           (test-assert (not (herdr-available-p))
                        "herdr is unavailable without pane and binary")
           (sb-posix:setenv "HERDR_PANE_ID" "w1:p2" 1)
           (sb-posix:setenv "HERDR_BIN_PATH" "/usr/bin/herdr" 1)
           (test-assert (herdr-available-p)
                        "herdr is available with marker, pane, and binary")
           (sb-posix:setenv "HERDR_ENV" "0" 1)
           (test-assert (not (herdr-available-p))
                        "herdr is unavailable without the marker value 1"))
      (dolist (entry saved)
        (tests--restore-environment (first entry) (rest entry)))))
  nil)

(-> test-herdr--call-state (list) string)
(defun test-herdr--call-state (call)
  "Return the state reported by one Herdr report-agent CLI CALL."
  (second (member "--state" call :test #'string-equal)))

(-> test-herdr--call-sequence (list) integer)
(defun test-herdr--call-sequence (call)
  "Return the sequence number reported by one Herdr CLI CALL."
  (parse-integer (second (member "--seq" call :test #'string-equal))))

(-> test-herdr-sequence-monotonicity () null)
(defun test-herdr-sequence-monotonicity ()
  "Test report sequences stay strictly increasing across process restarts."
  (with-lock-held (*herdr-lock*)
    (setf *herdr-sequence* 0)
    (let ((first-sequence (herdr--next-sequence))
          (second-sequence (herdr--next-sequence)))
      (test-assert (> first-sequence 0)
                   "herdr sequences seed from the wall clock")
      (test-assert (> second-sequence first-sequence)
                   "herdr sequences increase within one process")
      (setf *herdr-sequence* 0)
      (sleep 0.002)
      (test-assert (> (herdr--next-sequence) first-sequence)
                   "herdr sequences never drop below an earlier process")))
  nil)

(-> test-herdr--reset-state () null)
(defun test-herdr--reset-state ()
  "Clear shared Herdr reporter state so lifecycle tests start fresh."
  (with-lock-held (*herdr-lock*)
    (setf *herdr-sequence*            0
          *herdr-last-state*         nil
          *herdr-state-before-block* nil
          *herdr-pending*             0
          *herdr-released-p*          nil)
    (deque-clear *herdr-queue*))
  nil)

(-> test-herdr-lifecycle-reports () null)
(defun test-herdr-lifecycle-reports ()
  "Test queued lifecycle reports serialize with increasing sequence numbers."
  (let ((saved
          (mapcar (lambda (name) (cons name (uiop:getenv name)))
                  (list "HERDR_ENV" "HERDR_PANE_ID" "HERDR_BIN_PATH")))
        (calls '())
        (calls-lock (make-lock "test-herdr-calls")))
    (test-herdr--reset-state)
    (unwind-protect
         (test-call-with-function-replacements
          (list (list 'herdr--run-cli
                      (lambda (arguments)
                        (with-lock-held (calls-lock)
                          (setf calls (append calls (list arguments))))
                        nil)))
          (lambda ()
            (sb-posix:setenv "HERDR_ENV" "1" 1)
            (sb-posix:setenv "HERDR_PANE_ID" "w9:p1" 1)
            (sb-posix:setenv "HERDR_BIN_PATH" "/bin/true" 1)
            (sb-posix:unsetenv "HERDR_ENV")
            (herdr-report-idle)
            (test-assert (null calls)
                         "herdr reports are suppressed outside a pane")
            (sb-posix:setenv "HERDR_ENV" "1" 1)
            (herdr-report-idle)
            (herdr-report-working)
            (test-assert (herdr--await-quiescence :seconds 30)
                         "herdr idle and working reports drain in time")
            (test-assert
             (equal (mapcar #'test-herdr--call-state calls)
                    '("idle" "working"))
             "herdr reports serialize in submission order")
            (test-assert
             (apply #'<= (mapcar #'test-herdr--call-sequence calls))
             "herdr report sequences never decrease")
            (herdr-report-blocked "command approval: run make")
            (test-assert (herdr--await-quiescence :seconds 30)
                         "herdr blocked report drains in time")
            (herdr-report-resumed)
            (test-assert (herdr--await-quiescence :seconds 30)
                         "herdr resumed report drains in time")
            (test-assert
             (equal (mapcar #'test-herdr--call-state calls)
                    '("idle" "working" "blocked" "working"))
             "herdr resumes to the pre-block working state")
            (herdr-report-idle)
            (herdr-report-blocked "tool approval: allow tool")
            (herdr-report-resumed)
            (test-assert (herdr--await-quiescence :seconds 30)
                         "herdr second blocked cycle drains in time")
            (test-assert
             (string= (second (member "--state" (car (last calls))
                                      :test #'string-equal))
                      "idle")
             "herdr resumes to idle after a block from idling input")
            (herdr-release)
            (test-assert *herdr-released-p*
                         "herdr release records one authority release")
            (let ((release (car (last calls))))
              (test-assert
               (equal (subseq release 0 3)
                      '("/bin/true" "pane" "release-agent"))
               "herdr release runs the release-agent subcommand"))
            (herdr-report-idle)
            (let ((count-before (length calls)))
              (herdr--await-quiescence :seconds 5)
              (test-assert (= (length calls) count-before)
                           "herdr reports stay suppressed after release"))))
      (test-herdr--reset-state)
      (dolist (entry saved)
        (tests--restore-environment (first entry) (rest entry)))))
  nil)
