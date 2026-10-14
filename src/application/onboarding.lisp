(in-package #:autolith)

;;;; -- Lesson Catalog --

(defparameter *onboarding-lessons*
  '((:id "welcome" :version 3 :title "Welcome to Autolith"
     :body ((:brand "Autolith" "https://autolith.rocks")
             " is designed to be the most flexible agent.

I'm " (:strong "Lukáš Hozda" "https://twitter.com/LukasHozda")
             ". I made this tour to help you get started.

You'll try the prompt, then explore what you can do with Autolith.

"
            (:dim "Press Space to continue, or Esc to skip. Replay anytime with (onboard).")))
    (:id "ordinary-prompt" :version 3 :title "Start with a conversation" :exercise :continue
     :body ("Ask a question, describe a task, or paste some code.

"
            (:code "Explain this project") "
"
            (:code "Fix the failing tests and verify the change")
            "

Use familiar terminal editing shortcuts:
"
             (:strong "Ctrl-A / Ctrl-E") "  go to the beginning / end
"
             (:strong "Ctrl-W / Ctrl-U") "  erase a word / erase to the beginning
"
             (:strong "Up / Down") "       recall input history

"
            (:brand "PRACTICE  Type  continue  and press Enter.")))
    (:id "lisp-listener" :version 3 :title "Your prompt is also a Lisp REPL" :exercise :help
     :body ("Parenthesized input is local Common Lisp. Ordinary text goes to the agent.

"
            (:code "(help)") "      basic help
"
            (:code "(settings)") "  settings

"
            "When you start a Lisp form, the first prompt character changes to "
            (:lisp-prompt "*") ". Watch it change below.

"
             (:strong "Use Lisp forms and normal arguments.") "

"
             (:brand "PRACTICE  Type  (help)  to see the real basic help, then continue.")))
    (:id "asynchronous-lisp" :version 3 :title "Run Lisp in the background" :exercise :async
     :body ("Type " (:strong "? followed by a space") " before a Lisp form to evaluate it asynchronously.

"
            (:code "? (+ 1 2)")
            "

Press Enter to run it in the background. You can keep typing while it finishes.

"
            (:brand "(form)    local evaluation
? (form)  background local evaluation")
            "

"
            (:strong "PRACTICE  Try ? (+ 1 2).  This safe demo shows the result 3.")))
    (:id "prompt-function" :version 3 :title "Compose requests with Lisp"
     :body ("Use " (:code "(prompt ...)") " to submit text built in Lisp.

"
            (:code "(prompt \"Review this change\")") "

"
            "Target a named, already-running child with " (:code ":to") ":
"
            (:code "(prompt :to 'test-review
        \"Run focused tests and report failures.\")")
            "

The default target is the primary Autolith. "
            (:strong ":to names a running child; it does not create one.")
            "
Content can be computed, read from a file, or accompanied by images."))
    (:id "steering" :version 3 :title "Stay in control while work is active"
     :body ((:brand "NOW   Enter  -> steer the active turn
NEXT  Tab    -> queue a follow-up")
            "

"
             (:strong "Press Enter to steer the current work.")
             " All pending steers are submitted together at the next safe boundary.

"
             (:strong "Press Tab to queue a follow-up.")
             " Follow-ups are submitted one by one, in order, after the current work.

"
            (:code "Focus on the parser first") "
"
            (:code "After that, review the documentation")))
    (:id "input-vault" :version 3 :title "Park input for later"
     :body ("Store drafts and pending requests in the " (:strong "input vault")
            " to return to them later.

"
            (:code "(vault-store)") "    park input
"
            (:code "(vault)") "          browse and edit
"
            (:code "(vault-restore)") "  return input to the queue
"
            "Use " (:code "(vault-contents)") " to edit the stored Lisp data.

"
            (:brand "draft -> vault -> restore")
            "

Put work aside without losing what you typed. The vault is for input, not secrets."))
    (:id "indexed-search" :version 3 :title "Find your way through a project"
     :body ("Autolith uses " (:strong "fff indexed search") " for fast discovery.

"
            (:code "(search.files :query \"parser\")") "  fuzzy file paths
"
            (:code "(search.content :query \"parse-request\")") "  exact identifiers
"
            (:code "(search.glob :pattern \"**/*.lisp\")") "  matching paths

"
            (:strong "In the prompt: type @ and part of a path, then press Tab.")
            " Choose a fuzzy match to insert its path."))
    (:id "settings" :version 3 :title "Make it yours" :exercise :settings
     :body ("Type " (:code "(settings)") " to browse and change your settings.
Press Esc when you're ready to return to the tour.

"
            "For Lisp-level setup, edit:
"
            (:code "${XDG_CONFIG_HOME:-~/.config}/autolith/init.lisp") "

"
            "Durable preferences and onboarding acknowledgements live in the state directory.
"
            (:dim "The actual config and state directories appear below.")))
    (:id "detached-sessions" :version 3 :title "Leave the terminal.  Keep the session."
     :body ("Use " (:code "(detach)") " to disconnect your terminal while Autolith keeps working.

"
            (:strong "From another terminal:") "
"
            (:code "autolith localgroup status") "
"
            (:code "autolith localgroup attach SESSION-ID") "
"
            (:code "autolith localgroup tell SESSION-ID \"message\"") "
"
            (:code "autolith localgroup pause SESSION-ID") "
"
            (:code "autolith localgroup detach SESSION-ID") "
"
            (:code "autolith localgroup kill SESSION-ID") "

"
             "To watch without sending input, use " (:code "attach --read-only") ".
To take over the session, use " (:code "attach --take-over") ".
"
            "Use the conversation ID reported by " (:code "status") "."))
    (:id "goals" :version 3 :title "Give Autolith an objective"
     :body ("A request describes the next task. A " (:strong "goal")
            " gives autonomous work a longer-lived direction.

"
            (:code "(goal \"Make the project ready for release\")") "
"
             (:code "(goal :pause)") "   pause autonomous work
"
             (:code "(goal :resume)") "  resume it
"
             (:code "(goal :clear)") "   remove the objective

"
            "For explicit acceptance criteria, verification gates, and budgets, use "
            (:code "(mission ...)") "."))
    (:id "recursive-inference" :version 3 :title "Large inputs, focused reasoning"
     :body ((:strong "Recursive language models (RLM)")
            " let Autolith examine large inputs through bounded views and smaller inference calls.

"
            (:brand "large input
      -> selected views
      -> focused analyses
      -> combined result")
            "

Autolith chooses this when a task benefits from it, such as reading a long document or comparing resources.

"
            (:strong "You do not need to manage the recursion yourself.")))
    (:id "child-agents" :version 3 :title "Delegate independent work"
     :body ("Autolith can start " (:strong "child agents") " for implementation, research, design, or review.

"
            (:brand "you -> primary Autolith -> named children
                       <- results and evidence")
            "

Inspect roles with " (:code "(task.agents)") ". Define a .sexp role with "
            (:code ":name, :description, :instructions")
            " and optional tools, models, effort, or spawn policy.

"
            (:code ".autolith/agents/") "  project roles
"
            (:code "<config-root>/agents/") "  user roles

"
            "Steer a running child with " (:code "(prompt :to 'child-name \"...\")") "."))
    (:id "mcp" :version 3 :title "Connect external tools and services"
     :body ((:strong "Model Context Protocol (MCP)")
            " provides tools, resources, and reusable prompts from external servers.

"
            (:code "(mcp.status)") "  inspect connections
"
            (:code "(mcp.refresh)") " refresh discovery

"
            "Configure servers in " (:code "<config-root>/mcp.sexp") ".
"
            "Trusted project configuration can use " (:code ".autolith/mcp.sexp") ".
"
            (:code "init.lisp") " can also register servers; use " (:code "(mcp)")
            " for the configuration command and its help."))
    (:id "skills" :version 3 :title "Turn expertise into a reusable Skill"
     :body ("Skills are named instruction sets selected when relevant to a task.

"
            (:strong "Native format: SKILL.sexp")
            "
Use " (:code ":autolith-skill") " with version, name, description, and instructions."
            " Executable skills add EXECUTABLE.sexp with entrypoints, contracts, and capabilities.

"
            (:strong "Standard format: SKILL.md")
            "
YAML frontmatter with name and description, followed by Markdown instructions, is compatible too.

"
            (:code ".autolith/skills/NAME/") "  project skills
"
            (:code "<config-root>/skills/NAME/") "  user skills
"
            (:code "(skills)") " lists the catalog; " (:code "skill.load") " selects instructions."))
    (:id "fullscreen" :version 3 :title "Choose your workspace view"
     :body ("In " (:strong "fullscreen mode") ", scroll through the transcript while keeping the prompt in view.

"
            (:code "autolith --fullscreen") "

"
            "Save the preference through " (:code "(settings)") ". Inline mode fits ordinary terminal scrollback.

"
            (:brand "resize freely")
             " Scroll long lessons with Page Up/Page Down. Use Up/Down on reading pages."))
    (:id "live-image" :version 3 :title "Adapt the system while it runs"
     :body ("Autolith runs inside a " (:strong "live Common Lisp image") ". You and the agent can inspect, define, test, and revise behavior without a rebuild.

"
            (:brand "inspect -> redefine -> test -> private commit")
            "

Durable self-modifications are journaled and replayed from private image commits.

"
            (:strong "Your needs can shape the runtime.")
            " Changes are bounded by permissions, invariants, and the checks you require.

"
            (:code "autolith --immutable") " disables mutation-capable active-image operations for the session."))
    (:id "ready" :version 3 :title "Your next step"
     :body ((:strong "Give Autolith something real to do.") "

"
            (:code "Explain this codebase and identify the next useful change") "

"
            "Replay the tour with " (:code "(onboard)") ". Future launches show only new or revised lessons.

"
            (:brand "Welcome aboard."))))
  "Stable lesson identities and independently revised instructional content.")

(-> onboarding--body (list) terminal-styled-text)
(defun onboarding--body (fragments)
  "Convert authored text, semantic highlights, and links into terminal spans."
  (mapcar (lambda (fragment)
            (if (stringp fragment)
                (terminal-span ':plain fragment)
                (destructuring-bind (style text &optional url) fragment
                  (if url
                      (termdown:make-widget style text (list ':open-url url))
                      (terminal-span style text)))))
          fragments))

(-> onboarding-pages (configuration) list)
(defun onboarding-pages (configuration)
  "Return the current ordered lessons with CONFIGURATION's actual paths."
  (loop for lesson in *onboarding-lessons*
        for page = (copy-list lesson)
        do (setf (getf page :body) (onboarding--body (getf lesson :body)))
           (when (string= (getf page :id) "settings")
             (setf (getf page :body)
                   (append (getf page :body)
                           (list (terminal-span ':code
                                  (format nil "~%~%Config: ~A~%State:  ~A"
                                          (config :config-root configuration)
                                          (config :state-root configuration)))))))
        collect page))


;;;; -- Application Integration --

(-> application-onboarding--call-fullscreen (application function) t)
(defun application-onboarding--call-fullscreen (application function)
  "Call FUNCTION with exclusive fullscreen ownership, restoring the user's mode."
  (let* ((ui (application-ui application))
         (inline-p (not (terminal-ui-fullscreen-p ui)))
         (active-p (and (not inline-p) (fullscreen-terminal-ui-active-p ui)))
         (suspended-p (terminal-ui-live-output-suspended-p ui)))
    (terminal-ui-call-with-exclusive-input
     ui
     (lambda ()
       (unwind-protect
            (progn
              (with-terminal-ui-locked (ui)
                (setf (terminal-ui-live-output-suspended-p ui) t)
                (when inline-p
                  (change-class ui 'fullscreen-terminal-ui)))
              (terminal-ui-fullscreen-enter ui)
              (funcall function))
         (unwind-protect
              (unless active-p
                (when (terminal-ui-fullscreen-p ui)
                  (terminal-ui-fullscreen-leave ui)))
           (with-terminal-ui-locked (ui)
             (when inline-p
               (change-class ui 'terminal-ui))
             (setf (terminal-ui-live-output-suspended-p ui) suspended-p)
             (unless suspended-p
               (terminal-ui--paint-live ui)))))))))

(-> application-onboarding--settings (application) null)
(defun application-onboarding--settings (application)
  "Open the real settings picker and return to the suspended tour on every exit."
  (let* ((ui (application-ui application))
         (suspended-p (terminal-ui-live-output-suspended-p ui)))
    (unwind-protect
         (progn
           (with-terminal-ui-locked (ui)
             (setf (terminal-ui-live-output-suspended-p ui) nil))
           (let ((*application-command-interactive-p* t))
             (application-settings-command application)))
      (with-terminal-ui-locked (ui)
        (setf (terminal-ui-live-output-suspended-p ui) suspended-p))))
  nil)

(-> application-onboard (application &key (:new-only-p boolean)) keyword)
(defun application-onboard (application &key new-only-p)
  "Replay the tour, or only pending revisions when NEW-ONLY-P is true.

Persist each completed page immediately. Explicit refusal acknowledges the
remaining selected revisions; interruption leaves them pending."
  (let* ((configuration (application-configuration application))
         (ui (application-ui application))
         (pages (onboarding-pages configuration))
         (selected (if new-only-p
                       (onboarding-state-pending configuration pages)
                       pages)))
    (cond
      ((not (terminal-interactive-p (terminal-ui-terminal ui)))
       ':unavailable)
      ((null selected)
       ':completed)
      (t
       (application-onboarding--call-fullscreen
        application
        (lambda ()
          (let ((*terminal-ui-boot-linger-prompt* "Begin onboarding by pressing Space")
                (*terminal-ui-boot-start-label* "onboard"))
            (if (eq (terminal-ui-boot-sequence
                     ui :screen-p (config :boot-screen-p configuration) :linger-p t
                     :resize-callback #'application-pending-terminal-size
                     :duration (config :boot-screen-seconds configuration)
                     :tip-seconds (config :boot-screen-tip-seconds configuration))
                    ':interrupt)
                ':interrupted
                (multiple-value-bind (outcome seen)
                    (terminal-ui-onboard
                     ui selected
                     :introduction-title
                     (if (and new-only-p (< (length selected) (length pages)))
                         "Here's what's new in Autolith"
                         "Getting started")
                     :help-function (lambda () (application-operation-help application))
                      :settings-function (lambda () (application-onboarding--settings application))
                      :resize-callback #'application-pending-terminal-size
                     :page-completed-function
                     (lambda (page)
                       (onboarding-state-record configuration (list page) ':completed)))
                  (when (eq outcome ':refused)
                    (onboarding-state-record
                     configuration
                     (remove-if (lambda (page) (member (getf page :id) seen :test #'equal))
                                selected)
                     ':refused))
                  outcome)))))))))

(-> application-startup-presentation
    (application application-input-controller &key (:onboarding-p boolean)) keyword)
(defun application-startup-presentation (application controller &key (onboarding-p t))
  "Present pending onboarding on interactive startup, otherwise the usual boot."
  (let* ((ui (application-ui application))
         (terminal (terminal-ui-terminal ui))
         (configuration (application-configuration application))
         (pending (and onboarding-p (config :onboarding-p configuration)
                       (onboarding-state-pending configuration
                                                 (onboarding-pages configuration)))))
    (when (and pending (typep terminal 'localgroup-terminal))
      ;; Client-first sessions publish their listener before the relay reports a
      ;; TTY. Wait for the same window as the launch's attachment watchdog.
      (catch 'onboarding-client-wait-ended
        (terminal-ui--await-interactive
         terminal
         (lambda (seconds)
           (when (application-input-controller-exit-requested-p controller)
             (throw 'onboarding-client-wait-ended nil))
           (sleep seconds))
         *localgroup-first-attach-timeout-seconds*)))
    (let ((outcome
            (if (and pending (terminal-interactive-p terminal)
                     (not (application-input-controller-exit-requested-p controller)))
                (application-onboard application :new-only-p t)
                (terminal-ui-boot-sequence
                 ui :screen-p (config :boot-screen-p configuration)
                 :duration (config :boot-screen-seconds configuration)
                 :linger-p (config :boot-screen-linger-p configuration)
                 :tip-seconds (config :boot-screen-tip-seconds configuration)
                 :resize-callback #'application-pending-terminal-size
                 :halted-function
                 (lambda () (application-input-controller-exit-requested-p controller))
                 :direct-input-p-function
                 (lambda () (not (application-input-controller-reader-live-p controller)))))))
      (when (member outcome '(:interrupt :interrupted))
        (application-input-controller--request-exit controller ':interrupt))
      outcome)))

(define-application-command application--builtin-onboard-command
    (:name "/onboard"
     :argument nil
     :description "replay the fullscreen onboarding tour"
     :tip "Use (onboard) to practice the prompt and explore Autolith's capabilities."
     :busy-behavior :hold
     :terminal-behavior :exclusive
     :callable t)
    (application)
  (application-onboard application)
  ':continue)
