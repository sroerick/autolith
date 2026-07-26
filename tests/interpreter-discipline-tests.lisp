(in-package #:autolith)

;;;; -- Shell Interpreter Discipline Tests --

(-> interpreter-discipline-tests--shell-call (string) json-object)
(defun interpreter-discipline-tests--shell-call (command)
  "Return one durable shell.run function call executing COMMAND."
  (json-object
   "type" "function_call"
   "call_id" (make-identifier)
   "name" "run"
   "namespace" "shell"
   "arguments" (json-encode (json-object "command" command))))

(-> interpreter-discipline-tests--rendered
    (configuration conversation)
    string)
(defun interpreter-discipline-tests--rendered (configuration conversation)
  "Return the rendered request-local context for CONVERSATION."
  (or (context-delivery-rendered
       (context-resolve-request configuration conversation #()))
      ""))

(-> test-interpreter-discipline () null)
(defun test-interpreter-discipline ()
  "Test the corrective advice following a shell interpreter one-liner."
  (let* ((configuration (test-configuration))
         (conversation (conversation-create configuration
                                            :identifier "discipline-test"))
         (*context-contributors* nil)
         (*context-next-request-delivered* (make-hash-table :test #'equal))
         (*context-last-deliveries* (make-hash-table :test #'equal))
         (*context-last-delivery-order* nil))
    (register-context-contributor "interpreter-discipline"
                                  'interpreter-discipline-context
                                  :source ':built-in)
    (conversation-append-user-message conversation "sum the report numbers")
    (test-assert
     (not (search "interpreter one-liner"
                  (interpreter-discipline-tests--rendered configuration
                                                          conversation)))
     "a turn without shell calls draws no interpreter rebuke")
    (conversation-append-provider-item
     conversation
     (interpreter-discipline-tests--shell-call "ls -la"))
    (test-assert
     (not (search "interpreter one-liner"
                  (interpreter-discipline-tests--rendered configuration
                                                          conversation)))
     "an ordinary shell command draws no interpreter rebuke")
    (conversation-append-provider-item
     conversation
     (json-object
      "type" "function_call"
      "call_id" (make-identifier)
      "name" "eval"
      "namespace" "lisp"
      "arguments" (json-encode
                   (json-object "form" "(print \"python3 -c\")"))))
    (test-assert
     (not (search "interpreter one-liner"
                  (interpreter-discipline-tests--rendered configuration
                                                          conversation)))
     "a non-shell tool call mentioning the fragment draws no rebuke")
    (conversation-append-provider-item
     conversation
     (interpreter-discipline-tests--shell-call
      "python3 -c 'print(40 + 2)'"))
    (test-assert
     (search "interpreter one-liner"
             (interpreter-discipline-tests--rendered configuration
                                                     conversation))
     "a python3 -c shell call draws the interpreter rebuke")
    (conversation-append-user-message conversation "carry on")
    (test-assert
     (not (search "interpreter one-liner"
                  (interpreter-discipline-tests--rendered configuration
                                                          conversation)))
     "a new logical turn clears the interpreter rebuke")
    (conversation-append-provider-item
     conversation
     (interpreter-discipline-tests--shell-call
      "python -c 'import sys; print(sys.platform)'"))
    (test-assert
     (search "interpreter one-liner"
             (interpreter-discipline-tests--rendered configuration
                                                     conversation))
     "a python -c shell call draws the interpreter rebuke"))
  nil)
