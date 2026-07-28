(in-package #:autolith)

(-> test-workspace-plan () null)
(defun test-workspace-plan ()
  "Test plan update, list rendering, and clear."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (test-assert (null (plan-load configuration))
                        "a workspace begins without a plan")
           (let ((plan
                   (plan-update
                    configuration
                    (list (list :text "inspect the queue" :status "pending")
                          (list :text "fix the 429 path" :status "doing")
                          (list :text "ship the release" :status "done"))
                    :explanation "Finish the 0.20.0 release work.")))
             (test-assert (= (length (workspace-plan-steps plan)) 3)
                          "plan update retains ordered steps")
             (test-assert
              (eq (plan-step-status (second (workspace-plan-steps plan)))
                  ':doing)
              "plan statuses normalize doing aliases")
             (let ((loaded (plan-load configuration))
                   (rendered (plan-render plan)))
               (test-assert (and loaded
                                 (string= (workspace-plan-explanation loaded)
                                          "Finish the 0.20.0 release work."))
                            "plan load restores the explanation")
               (test-assert (search "[doing] fix the 429 path" rendered)
                            "plan rendering shows status and text")))
           (plan-clear configuration)
           (test-assert (null (plan-load configuration))
                        "plan clear removes the durable plan"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
