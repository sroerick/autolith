(in-package #:autolith)

(defun windows-tests--script (pathname forms)
  "Write readable FORMS as a native runtime test script."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                                   :external-format ':utf-8)
    (with-standard-io-syntax
      (let ((*package* (find-package '#:autolith)))
        (dolist (form forms)
          (write form :stream stream)
          (terpri stream)))))
  pathname)

(defun windows-tests--command (script &rest arguments)
  "Return the running runtime's script invocation with literal ARGUMENTS."
  (append (list (namestring sb-ext:*runtime-pathname*) "--noinform"
                "--no-userinit" "--no-sysinit" "--script" (namestring script))
          arguments))

(defun windows-tests--await (predicate)
  "Require PREDICATE within ten seconds, bounding native process fixtures."
  (let ((deadline (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop until (funcall predicate)
          do (when (>= (get-internal-real-time) deadline)
               (error "Native process fixture timed out."))
             (sleep 0.01)))
  t)

(defun test-windows-detached-arguments ()
  "Exercise real Unicode/quoted argv and inherited output through CreateProcessW."
  (with-platform-capability (':restartable-image-saver "native Windows process launch")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((directory (merge-pathnames "Lukáš žluťoučký/" root))
             (script (windows-tests--script
                      (merge-pathnames "arguments.lisp" directory)
                      '((require :asdf)
                        (write (uiop:command-line-arguments))
                        (terpri) (finish-output))))
             (arguments (list "" "two words" "Lukáš-žluťoučký"
                              (format nil "tab~Cvalue" #\Tab)
                              "slash\\\"quote" "trailing space\\"))
             (log (merge-pathnames "output.log" directory))
             (process nil))
        (unwind-protect
             (progn
               (with-open-file (output log :direction ':output :if-exists ':supersede)
                 (setf process
                       (platform-launch-detached-process
                        *platform* (apply #'windows-tests--command script arguments)
                        :directory directory :output output)))
               (windows-tests--await
                (lambda () (not (platform-process-object-alive-p *platform* process))))
               (platform-wait-process *platform* process)
               (with-open-file (input log :external-format ':utf-8)
                 (let ((*read-eval* nil))
                   (test-assert (equal (read input) arguments)
                                "native launch preserves every literal argument")))
               (platform-release-process *platform* process)
               (test-assert (not (platform-process-object-alive-p *platform* process))
                            "waiting and repeated release close resources idempotently"))
          (when process
            (ignore-errors (platform-terminate-process-object *platform* process :force t))
            (platform-wait-process *platform* process)))))))

(defun test-windows-detached-descendants ()
  "Cancel descendants after their detached root has already exited."
  (with-platform-capability (':restartable-image-saver "native Windows tree ownership")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((child (windows-tests--script (merge-pathnames "child.lisp" root)
                                          '((sleep 60))))
             (pid-file (merge-pathnames "child.pid" root))
             (script
               (windows-tests--script
                (merge-pathnames "parent.lisp" root)
                `((let ((process (sb-ext:run-program
                                  ,(namestring sb-ext:*runtime-pathname*)
                                  '("--noinform" "--script" ,(namestring child))
                                  :wait nil :input nil :output nil :error nil)))
                    (with-open-file (stream ,pid-file :direction :output
                                                     :if-exists :supersede)
                      (write (sb-ext:process-pid process) :stream stream))))))
             (handoff (merge-pathnames "handoff.sexp" root))
             (process nil))
        (unwind-protect
             (progn
               (setf process
                     (localgroup-handoff--launch-supervised
                      :arguments (windows-tests--command script)
                      :handoff-pathname handoff :directory root
                      :output *standard-output*))
               (windows-tests--await (lambda () (probe-file pid-file)))
               (windows-tests--await
                (lambda ()
                  (not (platform-process-alive-p
                        *platform* (platform-process-object-pid *platform* process)))))
               (test-assert (platform-process-object-alive-p *platform* process)
                            "the owned tree survives an exited root")
               (with-open-file (input pid-file)
                 (let* ((*read-eval* nil) (pid (read input)))
                   (test-assert (platform-process-alive-p *platform* pid)
                                "the root's child is running before cancellation")
                   (test-assert (localgroup-handoff--stop-replacement process handoff)
                                "handoff cancellation positively reaps the whole tree")
                   (test-assert (not (platform-process-alive-p *platform* pid))
                                "no descendant survives a cancelled handoff"))))
          (when process
            (ignore-errors (platform-terminate-process-object *platform* process :force t))
            (platform-wait-process *platform* process)))))))

(defun test-windows-detached-startup-failure ()
  "Refuse failed job assignment before any replacement code can run."
  (with-platform-capability (':restartable-image-saver "native Windows startup gate")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((marker (merge-pathnames "ran.sexp" root))
             (script (windows-tests--script
                      (merge-pathnames "marker.lisp" root)
                      `((with-open-file (stream ,marker :direction :output)
                          (write :ran :stream stream)))))
             (process-handle nil))
        (test-call-with-function-replacements
         (list (list 'win32--assign-process-to-job-object
                     (lambda (job process)
                       (declare (ignore job))
                       (setf process-handle process)
                       0)))
         (lambda ()
           (test-assert
            (handler-case
                (progn
                  (platform-launch-detached-process
                   *platform* (windows-tests--command script) :directory root)
                  nil)
              (platform-error () t))
            "failed assignment rejects the detached launch")))
        (test-assert process-handle "the test reached native job assignment")
        (test-assert (not (probe-file marker))
                     "the suspended replacement never ran before ownership")))))

(defun test-windows-detached-release ()
  "Release a successful handoff without killing its detached process."
  (with-platform-capability (':restartable-image-saver "native Windows ownership release")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((marker (merge-pathnames "completed.sexp" root))
             (script (windows-tests--script
                      (merge-pathnames "complete.lisp" root)
                      `((sleep 1)
                        (with-open-file (stream ,marker :direction :output)
                          (write :completed :stream stream)))))
             (process (platform-launch-detached-process
                       *platform* (windows-tests--command script) :directory root))
             (pid (platform-process-object-pid *platform* process)))
        (unwind-protect
             (progn
               (platform-release-process *platform* process)
               (platform-release-process *platform* process)
               (windows-tests--await (lambda () (probe-file marker)))
               (test-assert (probe-file marker)
                            "a successful detached session outlives released handles")
               (windows-tests--await
                (lambda () (not (platform-process-alive-p *platform* pid)))))
          (when (platform-process-alive-p *platform* pid)
            (platform-terminate-process *platform* pid :force t)))))))
(defun test-windows-restart-library ()
  "Save and boot the library's exact cyclic heap on native Windows."
  (with-platform-capability (':restartable-image-saver "native Windows exact heap save")
    (asdf:test-system "sbcl-generations")
    (test-assert t "native exact-heap restart and failed-save fixtures passed")))
