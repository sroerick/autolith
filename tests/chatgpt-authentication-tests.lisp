(in-package #:autolith)

;;;; -- ChatGPT OAuth Test Support --

(defvar *chatgpt-test-saved-credentials* nil
  "Credentials observed by the ChatGPT recording source.")

(defclass chatgpt-test-credential-source (autolith-credential-source)
  ()
  (:documentation "A ChatGPT credential source that records test writes."))

(defmethod credential-source-save
    ((source chatgpt-test-credential-source) (credentials oauth-credentials))
  "Record CREDENTIALS without writing SOURCE."
  (declare (ignore source))
  (setf *chatgpt-test-saved-credentials* credentials))

(-> chatgpt-test--manager () chatgpt-credential-manager)
(defun chatgpt-test--manager ()
  "Return an isolated recording ChatGPT credential manager."
  (make-instance 'chatgpt-credential-manager
                 :primary-source
                 (make-instance 'chatgpt-test-credential-source
                                :pathname #P"/tmp/chatgpt-auth.sexp")))

(defclass chatgpt-test-memory-credential-source (autolith-credential-source)
  ((credentials
    :initarg :credentials
    :accessor chatgpt-test-memory-source-credentials
    :type oauth-credentials
    :documentation "The in-memory credentials returned by this test source.")
   (save-count
    :initform 0
    :accessor chatgpt-test-memory-source-save-count
    :type (integer 0)
    :documentation "The number of credentials published to this test source."))
  (:documentation "An in-memory credential source for refresh concurrency tests."))

(defmethod credential-source-load ((source chatgpt-test-memory-credential-source))
  "Return SOURCE's current in-memory credentials."
  (chatgpt-test-memory-source-credentials source))

(defmethod credential-source-save
    ((source chatgpt-test-memory-credential-source)
     (credentials oauth-credentials))
  "Publish CREDENTIALS to SOURCE's in-memory store."
  (incf (chatgpt-test-memory-source-save-count source))
  (setf (chatgpt-test-memory-source-credentials source) credentials))

(defclass chatgpt-test-refresh-manager (chatgpt-credential-manager)
  ((gate-lock
    :initform (make-lock "ChatGPT refresh test gate")
    :reader chatgpt-test-refresh-manager-gate-lock
    :documentation "The lock protecting this test manager's exchange gate.")
   (gate-condition
    :initform (make-condition-variable :name "ChatGPT refresh test gate")
    :reader chatgpt-test-refresh-manager-gate-condition
    :documentation "The condition variable controlling the test exchange.")
   (released-count
    :initform 0
    :accessor chatgpt-test-refresh-manager-released-count
    :type (integer 0)
    :documentation "The number of synthetic exchanges allowed to leave the gate.")
   (exchange-count
    :initform 0
    :accessor chatgpt-test-refresh-manager-exchange-count
    :type (integer 0)
    :documentation "The number of synthetic refresh exchanges that reached the gate.")
   (outcomes
    :initarg :outcomes
    :initform '(:success)
    :accessor chatgpt-test-refresh-manager-outcomes
    :type list
    :documentation "The ordered synthetic outcomes for successive exchanges.")
   (refreshed-credentials
    :initarg :refreshed-credentials
    :reader chatgpt-test-refresh-manager-refreshed-credentials
    :type oauth-credentials
    :documentation "The credentials returned by a successful synthetic exchange."))
  (:documentation "A fully gated refresh manager used for deterministic concurrency tests."))

(defmethod credential-manager-refresh-exchange
    ((manager chatgpt-test-refresh-manager)
     (credentials oauth-credentials)
     (refresh-token string))
  "Wait at MANAGER's numbered gate, then perform its configured synthetic outcome."
  (declare (ignore credentials refresh-token))
  (let (number outcome)
    (with-lock-held ((chatgpt-test-refresh-manager-gate-lock manager))
      (setf number (incf (chatgpt-test-refresh-manager-exchange-count manager)))
      (sb-thread:condition-broadcast
       (chatgpt-test-refresh-manager-gate-condition manager))
      (loop while (> number (chatgpt-test-refresh-manager-released-count manager))
            do (condition-wait
                (chatgpt-test-refresh-manager-gate-condition manager)
                (chatgpt-test-refresh-manager-gate-lock manager)))
      (setf outcome
            (or (nth (1- number) (chatgpt-test-refresh-manager-outcomes manager))
                ':success)))
    (ecase outcome
      (:success
       (values (chatgpt-test-refresh-manager-refreshed-credentials manager) t))
      (:success-without-publication
       (values (chatgpt-test-refresh-manager-refreshed-credentials manager) nil))
      (:failure
       (error 'token-refresh-failed
              :message "Synthetic refresh failure."
              :status nil
              :response nil))
      (:throw
       (throw 'chatgpt-test-refresh-abort ':abandoned)))))


(defclass chatgpt-test-install-gated-refresh-manager
    (chatgpt-test-refresh-manager)
  ((install-count
    :initform 0
    :accessor chatgpt-test-refresh-manager-install-count
    :type (integer 0)
    :documentation "The number of leader generations observed before exchange.")
   (install-blocked-p
    :initform t
    :accessor chatgpt-test-refresh-manager-install-blocked-p
    :type boolean
    :documentation "Whether leader installation waits at the deterministic test gate."))
  (:documentation "A refresh manager that can be interrupted after leader publication."))

(defmethod credential-manager--refresh-generation-installed
    ((manager chatgpt-test-install-gated-refresh-manager)
     (generation credential-refresh-generation))
  "Block after GENERATION is externally visible and before its exchange begins."
  (declare (ignore generation))
  (with-lock-held ((chatgpt-test-refresh-manager-gate-lock manager))
    (incf (chatgpt-test-refresh-manager-install-count manager))
    (sb-thread:condition-broadcast
     (chatgpt-test-refresh-manager-gate-condition manager))
    (loop while (chatgpt-test-refresh-manager-install-blocked-p manager)
          do (condition-wait
              (chatgpt-test-refresh-manager-gate-condition manager)
              (chatgpt-test-refresh-manager-gate-lock manager))))
  nil)

(-> chatgpt-test--refresh-fixture
    (&key (:outcomes list))
    (values chatgpt-test-refresh-manager
            chatgpt-test-memory-credential-source
            oauth-credentials
            oauth-credentials))
(defun chatgpt-test--refresh-fixture (&key (outcomes '(:success)))
  "Return a gated manager, memory source, and stale/fresh credentials."
  (let* ((pathname #P"/tmp/chatgpt-refresh-single-flight.sexp")
         (stale
           (make-instance 'oauth-credentials
                          :access-token "stale-access"
                          :refresh-token "rotating-refresh"
                          :id-token nil
                          :account-id "account-refresh"
                          :expires-at nil
                          :source-path pathname))
         (fresh
           (make-instance 'oauth-credentials
                          :access-token "fresh-access"
                          :refresh-token "rotated-refresh"
                          :id-token nil
                          :account-id "account-refresh"
                          :expires-at (+ (get-universal-time) 3600)
                          :source-path pathname))
         (source
           (make-instance 'chatgpt-test-memory-credential-source
                          :pathname pathname
                          :credentials stale))
         (manager
           (make-instance 'chatgpt-test-refresh-manager
                          :primary-source source
                          :outcomes outcomes
                          :refreshed-credentials fresh)))
    (values manager source stale fresh)))

(-> chatgpt-test--wait-for-exchange-count
    (chatgpt-test-refresh-manager (integer 1))
    null)
(defun chatgpt-test--wait-for-exchange-count (manager count)
  "Wait at most five seconds for MANAGER to begin COUNT synthetic exchanges."
  (sb-sys:with-deadline (:seconds 5)
    (with-lock-held ((chatgpt-test-refresh-manager-gate-lock manager))
      (loop while (< (chatgpt-test-refresh-manager-exchange-count manager) count)
            do (condition-wait
                (chatgpt-test-refresh-manager-gate-condition manager)
                (chatgpt-test-refresh-manager-gate-lock manager)))))
  nil)

(-> chatgpt-test--release-refresh-exchange (chatgpt-test-refresh-manager) null)
(defun chatgpt-test--release-refresh-exchange (manager)
  "Release the next numbered synthetic exchange for MANAGER."
  (with-lock-held ((chatgpt-test-refresh-manager-gate-lock manager))
    (incf (chatgpt-test-refresh-manager-released-count manager))
    (sb-thread:condition-broadcast
     (chatgpt-test-refresh-manager-gate-condition manager)))
  nil)

(-> chatgpt-test--current-refresh-generation
    (chatgpt-test-refresh-manager)
    credential-refresh-generation)
(defun chatgpt-test--current-refresh-generation (manager)
  "Return MANAGER's active refresh generation under its state lock."
  (with-lock-held ((credential-manager-refresh-lock manager))
    (or (credential-manager--refresh-generation manager)
        (error "The test manager has no active refresh generation."))))

(-> chatgpt-test--wait-for-generation-waiter
    (credential-refresh-generation)
    null)
(defun chatgpt-test--wait-for-generation-waiter (generation)
  "Wait at most five seconds until GENERATION has an observable waiter."
  (sb-sys:with-deadline (:seconds 5)
    (with-lock-held ((credential-refresh-generation-lock generation))
      (loop until (plusp (credential-refresh-generation-waiter-count generation))
            do (condition-wait
                (credential-refresh-generation-completion generation)
                (credential-refresh-generation-lock generation)))))
  nil)

(-> chatgpt-test--wait-for-manager-idle (chatgpt-test-refresh-manager) null)
(defun chatgpt-test--wait-for-manager-idle (manager)
  "Wait at most five seconds until MANAGER exposes no active refresh generation."
  (sb-sys:with-deadline (:seconds 5)
    (with-lock-held ((credential-manager-refresh-lock manager))
      (loop while (credential-manager--refresh-generation manager)
            do (condition-wait
                (credential-manager--refresh-completion manager)
                (credential-manager-refresh-lock manager)))))
  nil)

(-> chatgpt-test--join-thread (t) t)
(defun chatgpt-test--join-thread (thread)
  "Join THREAD within five seconds and return its primary value."
  (let ((result (sb-thread:join-thread thread :timeout 5 :default ':timed-out)))
    (test-assert (not (eq result ':timed-out))
                 "the gated refresh test thread terminates within its bound")
    result))

(-> chatgpt-test--single-flight-refresh () null)
(defun chatgpt-test--single-flight-refresh ()
  "Test deterministic refresh success, failure replay, and non-local cleanup."
  (multiple-value-bind (manager source stale fresh)
      (chatgpt-test--refresh-fixture)
    (let ((first-result nil)
          (second-result nil))
      (let ((first-thread
              (make-thread
               (lambda ()
                 (setf first-result (credential-manager-refresh manager stale)))
               :name "ChatGPT refresh leader")))
        (chatgpt-test--wait-for-exchange-count manager 1)
        (let ((generation (chatgpt-test--current-refresh-generation manager)))
          (let ((acquired-p
                  (bordeaux-threads:acquire-lock
                   (credential-manager-refresh-lock manager)
                   nil)))
            (test-assert acquired-p
                         "a blocked refresh exchange does not hold the manager lock")
            (when acquired-p
              (bordeaux-threads:release-lock
               (credential-manager-refresh-lock manager))))
          (let ((second-thread
                  (make-thread
                   (lambda ()
                     (setf second-result
                           (credential-manager-refresh manager stale)))
                   :name "ChatGPT refresh waiter")))
            (chatgpt-test--wait-for-generation-waiter generation)
            (chatgpt-test--release-refresh-exchange manager)
            (chatgpt-test--join-thread first-thread)
            (chatgpt-test--join-thread second-thread)
            (test-assert
             (and (eq first-result fresh)
                  (eq second-result fresh)
                  (= (chatgpt-test-refresh-manager-exchange-count manager) 1)
                  (= (chatgpt-test-memory-source-save-count source) 1))
             "concurrent refresh success performs one exchange and one publication"))))))
  (multiple-value-bind (manager source stale fresh)
      (chatgpt-test--refresh-fixture :outcomes '(:failure :success))
    (declare (ignore fresh))
    (flet ((attempt-refresh ()
             (handler-case
                 (credential-manager-refresh manager stale)
               (token-refresh-failed (condition)
                 condition))))
      (let ((first-result nil)
            (second-result nil))
        (let ((first-thread
                (make-thread
                 (lambda () (setf first-result (attempt-refresh)))
                 :name "ChatGPT failed refresh leader")))
          (chatgpt-test--wait-for-exchange-count manager 1)
          (let* ((generation (chatgpt-test--current-refresh-generation manager))
                 (second-thread
                   (make-thread
                    (lambda () (setf second-result (attempt-refresh)))
                    :name "ChatGPT failed refresh waiter")))
            (chatgpt-test--wait-for-generation-waiter generation)
            (chatgpt-test--release-refresh-exchange manager)
            (chatgpt-test--join-thread first-thread)
            (chatgpt-test--join-thread second-thread)
            (test-assert
             (and (typep first-result 'token-refresh-failed)
                  (eq first-result second-result)
                  (= (chatgpt-test-refresh-manager-exchange-count manager) 1)
                  (zerop (chatgpt-test-memory-source-save-count source)))
             "concurrent refresh failure is replayed from one immutable outcome")
            (chatgpt-test--release-refresh-exchange manager)
            (let ((retry (credential-manager-refresh manager stale)))
              (test-assert
               (and (string= (oauth-credentials-access-token retry) "fresh-access")
                    (= (chatgpt-test-refresh-manager-exchange-count manager) 2)
                    (= (chatgpt-test-memory-source-save-count source) 1))
               "a later refresh may proceed after a shared failure")))))))
  (multiple-value-bind (manager source stale fresh)
      (chatgpt-test--refresh-fixture :outcomes '(:failure :success))
    (declare (ignore source))
    (flet ((attempt-refresh ()
             (handler-case
                 (credential-manager-refresh manager stale)
               (token-refresh-failed (condition)
                 condition))))
      (let ((leader-result nil)
            (waiter-result nil)
            (next-result nil))
        (let ((leader-thread
                (make-thread
                 (lambda () (setf leader-result (attempt-refresh)))
                 :name "ChatGPT epoch N leader")))
          (chatgpt-test--wait-for-exchange-count manager 1)
          (let* ((generation (chatgpt-test--current-refresh-generation manager))
                 (waiter-thread
                   (make-thread
                    (lambda () (setf waiter-result (attempt-refresh)))
                    :name "ChatGPT epoch N waiter")))
            (chatgpt-test--wait-for-generation-waiter generation)
            (let ((next-thread nil))
              (with-lock-held ((credential-refresh-generation-lock generation))
                (chatgpt-test--release-refresh-exchange manager)
                (chatgpt-test--wait-for-manager-idle manager)
                (setf next-thread
                      (make-thread
                       (lambda ()
                         (setf next-result
                               (credential-manager-refresh manager stale)))
                       :name "ChatGPT epoch N+1 leader"))
                (chatgpt-test--wait-for-exchange-count manager 2))
              (chatgpt-test--join-thread leader-thread)
              (chatgpt-test--join-thread waiter-thread)
              (test-assert
               (and (typep leader-result 'token-refresh-failed)
                    (eq leader-result waiter-result))
               "epoch N failure survives epoch N+1 starting before its waiter resumes")
              (chatgpt-test--release-refresh-exchange manager)
              (chatgpt-test--join-thread next-thread)
              (test-assert (eq next-result fresh)
                           "epoch N+1 completes independently of epoch N's outcome")))))))
  (multiple-value-bind (manager source stale fresh)
      (chatgpt-test--refresh-fixture
       :outcomes '(:success-without-publication :failure))
    (flet ((attempt-refresh ()
             (handler-case
                 (credential-manager-refresh manager stale)
               (token-refresh-failed (condition)
                 condition))))
      (let ((leader-result nil)
            (waiter-result nil)
            (next-result nil))
        (let ((leader-thread
                (make-thread
                 (lambda () (setf leader-result (attempt-refresh)))
                 :name "ChatGPT successful epoch N leader")))
          (chatgpt-test--wait-for-exchange-count manager 1)
          (let* ((generation (chatgpt-test--current-refresh-generation manager))
                 (waiter-thread
                   (make-thread
                    (lambda () (setf waiter-result (attempt-refresh)))
                    :name "ChatGPT successful epoch N waiter")))
            (chatgpt-test--wait-for-generation-waiter generation)
            (let ((next-thread nil))
              (with-lock-held ((credential-refresh-generation-lock generation))
                (chatgpt-test--release-refresh-exchange manager)
                (chatgpt-test--wait-for-manager-idle manager)
                (setf next-thread
                      (make-thread
                       (lambda () (setf next-result (attempt-refresh)))
                       :name "ChatGPT failing epoch N+1 leader"))
                (chatgpt-test--wait-for-exchange-count manager 2))
              (chatgpt-test--join-thread leader-thread)
              (chatgpt-test--join-thread waiter-thread)
              (test-assert
               (and (eq leader-result fresh)
                    (eq waiter-result fresh)
                    (zerop (chatgpt-test-memory-source-save-count source)))
               "epoch N success survives epoch N+1 starting before its waiter resumes")
              (chatgpt-test--release-refresh-exchange manager)
              (chatgpt-test--join-thread next-thread)
              (test-assert
               (typep next-result 'token-refresh-failed)
               "epoch N+1 failure remains independent of epoch N's success")))))))
  (multiple-value-bind (manager source stale fresh)
      (chatgpt-test--refresh-fixture :outcomes '(:throw :success))
    (let ((leader-result nil)
          (waiter-result nil))
      (let ((leader-thread
              (make-thread
               (lambda ()
                 (setf leader-result
                       (catch 'chatgpt-test-refresh-abort
                         (credential-manager-refresh manager stale))))
               :name "ChatGPT abandoned refresh leader")))
        (chatgpt-test--wait-for-exchange-count manager 1)
        (let* ((generation (chatgpt-test--current-refresh-generation manager))
               (waiter-thread
                 (make-thread
                  (lambda ()
                    (setf waiter-result
                          (credential-manager-refresh manager stale)))
                  :name "ChatGPT abandoned refresh waiter")))
          (chatgpt-test--wait-for-generation-waiter generation)
          (chatgpt-test--release-refresh-exchange manager)
          (chatgpt-test--wait-for-exchange-count manager 2)
          (chatgpt-test--release-refresh-exchange manager)
          (chatgpt-test--join-thread leader-thread)
          (chatgpt-test--join-thread waiter-thread)
          (test-assert
           (and (eq leader-result ':abandoned)
                (eq waiter-result fresh)
                (= (chatgpt-test-refresh-manager-exchange-count manager) 2)
                (= (chatgpt-test-memory-source-save-count source) 1)
                (not (credential-manager--refresh-in-progress-p manager)))
           "a non-local leader exit broadcasts cleanup and leaves retryable state")))))
  nil)

(-> chatgpt-test--leader-install-interruption () null)
(defun chatgpt-test--leader-install-interruption ()
  "Interrupt a published refresh leader before exchange and prove cleanup wakes callers."
  (multiple-value-bind (base-manager source stale fresh)
      (chatgpt-test--refresh-fixture)
    (let* ((manager
             (make-instance
              'chatgpt-test-install-gated-refresh-manager
              :primary-source source
              :refreshed-credentials fresh))
           (leader-result nil)
           (waiter-result nil)
           (leader-thread
             (make-thread
              (lambda ()
                (setf leader-result
                      (catch 'chatgpt-test-refresh-install-abort
                        (credential-manager-refresh manager stale))))
              :name "ChatGPT pre-exchange refresh leader")))
      (declare (ignore base-manager))
      (sb-sys:with-deadline (:seconds 5)
        (with-lock-held ((chatgpt-test-refresh-manager-gate-lock manager))
          (loop until (= (chatgpt-test-refresh-manager-install-count manager) 1)
                do (condition-wait
                    (chatgpt-test-refresh-manager-gate-condition manager)
                    (chatgpt-test-refresh-manager-gate-lock manager)))))
      (let* ((generation (chatgpt-test--current-refresh-generation manager))
             (waiter-thread
               (make-thread
                (lambda ()
                  (setf waiter-result
                        (credential-manager-refresh manager stale)))
                :name "ChatGPT pre-exchange refresh waiter")))
        (chatgpt-test--wait-for-generation-waiter generation)
        (sb-thread:interrupt-thread
         leader-thread
         (lambda ()
           (throw 'chatgpt-test-refresh-install-abort ':interrupted)))
        (test-assert (eq (chatgpt-test--join-thread leader-thread) ':interrupted)
                     "the refresh leader is interrupted before exchange")
        (sb-sys:with-deadline (:seconds 5)
          (with-lock-held ((chatgpt-test-refresh-manager-gate-lock manager))
            (loop until (= (chatgpt-test-refresh-manager-install-count manager) 2)
                  do (condition-wait
                      (chatgpt-test-refresh-manager-gate-condition manager)
                      (chatgpt-test-refresh-manager-gate-lock manager)))
            (setf (chatgpt-test-refresh-manager-install-blocked-p manager) nil)
            (sb-thread:condition-broadcast
             (chatgpt-test-refresh-manager-gate-condition manager))))
        (chatgpt-test--wait-for-exchange-count manager 1)
        (chatgpt-test--release-refresh-exchange manager)
        (chatgpt-test--join-thread waiter-thread)
        (chatgpt-test--wait-for-manager-idle manager)
        (test-assert
         (and (eq waiter-result fresh)
              (eq (credential-manager-refresh manager stale) fresh)
              (= (chatgpt-test-refresh-manager-exchange-count manager) 1)
              (= (chatgpt-test-memory-source-save-count source) 1)
              (not (credential-manager--refresh-in-progress-p manager)))
         "leader cleanup wakes waiters and leaves later refresh calls usable"))))
  nil)

(-> chatgpt-test--rotated-refresh-token-reconciliation () null)
(defun chatgpt-test--rotated-refresh-token-reconciliation ()
  "Test source reconciliation notices refresh-token rotation without access rotation."
  (multiple-value-bind (manager source stale fresh)
      (chatgpt-test--refresh-fixture)
    (declare (ignore fresh))
    (let ((rotated
            (make-instance 'oauth-credentials
                           :access-token (oauth-credentials-access-token stale)
                           :refresh-token "externally-rotated-refresh"
                           :id-token nil
                           :account-id (oauth-credentials-account-id stale)
                           :expires-at (+ (get-universal-time) 3600)
                           :source-path (oauth-credentials-source-path stale))))
      (setf (chatgpt-test-memory-source-credentials source) rotated)
      (test-assert
       (and (eq (credential-manager-refresh manager stale) rotated)
            (zerop (chatgpt-test-refresh-manager-exchange-count manager)))
       "the stale-source recheck adopts a refresh-token-only rotation")))
  (multiple-value-bind (manager source credentials fresh)
      (chatgpt-test--refresh-fixture)
    (declare (ignore fresh))
    (let ((access-only
            (make-instance 'oauth-credentials
                           :access-token "externally-rotated-access"
                           :refresh-token
                           (oauth-credentials-refresh-token credentials)
                           :id-token nil
                           :account-id (oauth-credentials-account-id credentials)
                           :expires-at (+ (get-universal-time) 3600)
                           :source-path (oauth-credentials-source-path credentials))))
      (setf (chatgpt-test-memory-source-credentials source) access-only)
      (test-assert
       (null (credential-manager--refresh-token-reuse-recovery
              manager
              (oauth-credentials-refresh-token credentials)
              "refresh_token_reused"))
       "refresh_token_reused rejects an access-only source rotation"))
    (let ((rotated
            (make-instance 'oauth-credentials
                           :access-token (oauth-credentials-access-token credentials)
                           :refresh-token "reused-recovery-refresh"
                           :id-token nil
                           :account-id (oauth-credentials-account-id credentials)
                           :expires-at (+ (get-universal-time) 3600)
                           :source-path (oauth-credentials-source-path credentials))))
      (setf (chatgpt-test-memory-source-credentials source) rotated)
      (test-assert
       (eq (credential-manager--refresh-token-reuse-recovery
            manager
            (oauth-credentials-refresh-token credentials)
            "refresh_token_reused")
           rotated)
       "refresh_token_reused adopts a refresh-token-only source rotation")))
  nil)

(-> chatgpt-test--refresh-response-deadline () null)
(defun chatgpt-test--refresh-response-deadline ()
  "Test ChatGPT refresh uses its deadline and normalizes deadline failure."
  (let* ((manager (chatgpt-test--manager))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token "old-access"
                          :refresh-token "old-refresh"
                          :id-token nil
                          :account-id "account-refresh-deadline"
                          :expires-at nil
                          :source-path #P"/tmp/chatgpt-auth.sexp"))
         (deadline nil))
    (test-call-with-function-replacements
     (list
      (list
       'provider-call-with-response-deadline
       (lambda (seconds function)
         (setf deadline seconds)
         (funcall function)))
      (list
       'dexador:post
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (json-encode
          (json-object "access_token" "new-access"
                       "refresh_token" "new-refresh")))))
     (lambda ()
       (multiple-value-bind (refreshed publish-p)
           (credential-manager-refresh-exchange
            manager credentials "old-refresh")
         (test-assert
          (and publish-p
               (string= (oauth-credentials-access-token refreshed) "new-access")
               (= deadline 60))
          "ChatGPT token refresh wraps the complete response in a 60-second deadline"))))
    (test-assert
     (handler-case
         (test-call-with-function-replacements
          (list
           (list
            'provider-call-with-response-deadline
            (lambda (seconds function)
              (declare (ignore seconds function))
              (error 'sb-sys:deadline-timeout))))
          (lambda ()
            (credential-manager-refresh-exchange
             manager credentials "old-refresh")))
       (token-refresh-failed ()
         t))
     "ChatGPT refresh deadlines become typed token refresh failures"))
  nil)

(-> chatgpt-test--parameter (string string) (option string))
(defun chatgpt-test--parameter (target name)
  "Return NAME from TARGET's decoded query parameters."
  (rest (assoc name (oauth--query-parameters target) :test #'string=)))


;;;; -- ChatGPT OAuth Tests --

(-> run-chatgpt-authentication-tests () null)
(defun run-chatgpt-authentication-tests ()
  "Test PKCE, authorization, callback validation, exchange, and command routing."
  (chatgpt-test--refresh-response-deadline)
  (chatgpt-test--single-flight-refresh)
  (chatgpt-test--leader-install-interruption)
  (chatgpt-test--rotated-refresh-token-reconciliation)
  (multiple-value-bind (verifier challenge)
      (chatgpt-oauth-create-pkce)
    (test-assert (= (length verifier) 86)
                 "ChatGPT PKCE emits the current 512-bit verifier")
    (test-assert (= (length challenge) 43)
                 "ChatGPT PKCE emits an S256 challenge")
    (test-assert (and (not (find #\= verifier))
                      (not (find #\= challenge)))
                 "ChatGPT PKCE values are unpadded Base64url"))
  (let* ((redirect-uri "http://localhost:1455/auth/callback")
         (url
           (chatgpt-oauth-authorization-url
            :redirect-uri redirect-uri
            :state "state-test"
            :code-challenge "challenge-test"
            :issuer "https://issuer.test/"
            :client-id "client-test"
            :originator "autolith-test")))
    (test-assert (string= (subseq url 0 (position #\? url))
                          "https://issuer.test/oauth/authorize")
                 "ChatGPT authorization uses the configured issuer")
    (dolist (case
             `(("response_type" . "code")
               ("client_id" . "client-test")
               ("redirect_uri" . ,redirect-uri)
               ("scope" . "openid profile email offline_access api.connectors.read api.connectors.invoke")
               ("code_challenge" . "challenge-test")
               ("code_challenge_method" . "S256")
               ("id_token_add_organizations" . "true")
               ("codex_cli_simplified_flow" . "true")
               ("state" . "state-test")
               ("originator" . "autolith-test")))
      (test-assert
       (string= (chatgpt-test--parameter url (first case)) (rest case))
       (format nil "ChatGPT authorization includes ~A" (first case)))))
  (test-assert
   (string=
    (chatgpt-oauth--callback-code
     "/auth/callback?code=code-test&state=state-test"
     "state-test")
    "code-test")
   "ChatGPT callback validation returns the authorization code")
  (test-assert
   (string=
    (chatgpt-oauth--callback-code
     "/auth/callback?code=code-test&state=state-test.onboarding_entrypoint%3Dlife_sciences"
     "state-test")
    "code-test")
   "ChatGPT callback validation accepts the supported onboarding state suffix")
  (let ((condition nil)
        (state "state-secret")
        (code "code-secret"))
    (handler-case
        (chatgpt-oauth--callback-code
         (format nil "/auth/callback?code=~A&state=wrong" code)
         state)
      (chatgpt-oauth-error (caught)
        (setf condition caught)))
    (test-assert
     (and condition
          (eq (chatgpt-oauth-error-stage condition) ':callback)
          (not (test-object-contains-string-p condition state))
          (not (test-object-contains-string-p condition code)))
     "ChatGPT callback failures reject mismatched state without retaining secrets")
    (test-assert (typep condition 'chatgpt-oauth-state-mismatch)
                 "ChatGPT state mismatches use their dedicated condition"))
  (let* ((secret "secret-that-crosses-the-original-boundary")
         (value
           (concatenate 'string
                        (make-string 230 :initial-element #\x)
                        secret
                        " trailing text"))
         (safe-value (chatgpt-oauth--redacted-value value (list secret))))
    (test-assert
     (and (search "[OAUTH VALUE REDACTED]" safe-value)
          (not (search (subseq secret 0 16) safe-value)))
     "ChatGPT OAuth errors redact complete secrets before bounding output"))
  (test-assert
   (null
    (chatgpt-oauth--callback-code-or-continue
     "/auth/callback?code=ignored&state=wrong"
     "state-test"))
   "ChatGPT listener handling ignores unrelated local callbacks")
  (let ((wait-count 0))
    (test-assert
     (null
      (chatgpt-oauth--read-request-line
       (make-string-input-stream "")
       -1
       201/2
       :clock-function (lambda () 1/2)
       :wait-function
       (lambda (file-descriptor direction timeout)
         (declare (ignore file-descriptor direction timeout))
         (incf wait-count)
         nil)))
     "ChatGPT callback request reading stops when its local read wait expires")
    (test-assert (= wait-count 1)
                 "ChatGPT callback request reading performs one bounded wait"))
  (let ((headers (make-hash-table :test #'equal)))
    (multiple-value-bind (body status returned-headers)
        (test-call-with-function-replacements
         (list
          (list
           'dexador:post
           (lambda (&rest arguments)
             (declare (ignore arguments))
             (values "{}" 200 headers nil nil))))
         (lambda ()
           (chatgpt-oauth--request
            :url "https://issuer.test/oauth/token"
            :content "grant_type=authorization_code")))
      (test-assert
       (and (string= body "{}")
            (= status 200)
            (eq returned-headers headers))
       "ChatGPT token transport accepts Dexador hash-table response headers")))
  (let* ((manager (chatgpt-test--manager))
         (id-token (test-account-jwt "account-test"))
         (request-url nil)
         (request-content nil)
         (credentials
           (chatgpt-oauth-exchange-code
            manager
            "code-secret"
            "verifier-secret"
            "http://localhost:1455/auth/callback"
            :client-id "client-test"
            :token-endpoint "https://issuer.test/oauth/token"
            :request-function
            (lambda (&key url content)
              (setf request-url url
                    request-content content)
              (values
               (json-encode
                (json-object "id_token" id-token
                             "access_token" "access-test"
                             "refresh_token" "refresh-test"))
               200
               nil)))))
    (test-assert (string= request-url "https://issuer.test/oauth/token")
                 "ChatGPT exchange uses the configured token endpoint")
    (dolist (case
             '(("grant_type" . "authorization_code")
               ("code" . "code-secret")
               ("redirect_uri" . "http://localhost:1455/auth/callback")
               ("client_id" . "client-test")
               ("code_verifier" . "verifier-secret")))
      (test-assert
       (string= (chatgpt-test--parameter
                 (format nil "?~A" request-content)
                 (first case))
                (rest case))
       (format nil "ChatGPT exchange includes ~A" (first case))))
    (test-assert
     (and (string= (oauth-credentials-access-token credentials) "access-test")
          (string= (oauth-credentials-refresh-token credentials) "refresh-test")
          (string= (oauth-credentials-account-id credentials) "account-test"))
     "ChatGPT exchange returns renewable account credentials"))
  (let ((condition nil)
        (secret "verifier-do-not-leak"))
    (handler-case
        (chatgpt-oauth--token-document
         (lambda (&key url content)
           (declare (ignore url content))
           (values
            (json-encode
             (json-object
              "error"
              (json-object "code" "invalid_grant"
                           "message" (format nil "bad ~A" secret))))
            400
            nil))
         "https://issuer.test/oauth/token"
         (list (cons "code_verifier" secret))
         ':exchange)
      (chatgpt-oauth-error (caught)
        (setf condition caught)))
    (test-assert
     (and condition
          (eq (chatgpt-oauth-error-stage condition) ':exchange)
          (= (chatgpt-oauth-error-status condition) 400)
          (string= (chatgpt-oauth-error-code condition) "invalid_grant")
          (not (test-object-contains-string-p condition secret)))
     "ChatGPT token failures use typed redacted diagnostics"))
  (let* ((manager (chatgpt-test--manager))
         (id-token (test-account-jwt "account-login"))
         (*chatgpt-test-saved-credentials* nil)
         (output (make-string-output-stream))
         (browser-url nil)
         (secret-guard-observed-p nil))
    (test-call-with-function-replacements
     (list
      (list 'chatgpt-oauth-loopback-open
            (lambda ()
              (values ':listener "http://localhost:1455/auth/callback")))
      (list 'chatgpt-oauth-create-pkce
            (lambda () (values "verifier-test" "challenge-test")))
      (list 'chatgpt-oauth--state
            (lambda () "state-test")))
     (lambda ()
       (chatgpt-oauth-login
        manager
        :stream output
        :browser-function (lambda (url) (setf browser-url url) nil)
        :callback-function
        (lambda (listener state &key timeout)
          (test-assert (eq listener ':listener)
                       "ChatGPT login waits on its loopback listener")
          (test-assert (and (string= state "state-test") (= timeout 900))
                       "ChatGPT login passes state and timeout to the callback")
          (setf secret-guard-observed-p (secret-use-active-p))
          "code-test")
        :request-function
        (lambda (&key url content)
          (declare (ignore url content))
          (values
           (json-encode
            (json-object "id_token" id-token
                         "access_token" "access-login"
                         "refresh_token" "refresh-login"))
           200
           nil)))))
    (let ((text (get-output-stream-string output)))
      (test-assert (and browser-url
                        (search "http%3A%2F%2Flocalhost%3A1455%2Fauth%2Fcallback"
                                browser-url)
                        (search "Could not open a browser" text))
                   "ChatGPT login exposes the browser URL and manual fallback"))
    (test-assert secret-guard-observed-p
                 "ChatGPT login keeps transient OAuth data in secret scope")
    (test-assert
     (and *chatgpt-test-saved-credentials*
          (string= (oauth-credentials-access-token
                    *chatgpt-test-saved-credentials*)
                   "access-login"))
     "ChatGPT login publishes credentials through the credential manager"))
  (let* ((provider
           (provider-authentication-provider (test-configuration) "chatgpt"))
         (output (make-string-output-stream))
         (browser-setting nil)
         (device-setting nil)
         (browser-login-count 0)
         (device-login-count 0)
         (browser-message nil)
         (device-message nil))
    (test-call-with-function-replacements
     (list
      (list 'chatgpt-oauth-login
            (lambda (manager &key stream open-browser-p)
              (declare (ignore manager stream))
              (incf browser-login-count)
              (setf browser-setting open-browser-p)
              nil))
      (list 'device-authentication-login
            (lambda (client manager &key stream open-browser-p)
              (declare (ignore client manager stream))
              (incf device-login-count)
              (setf device-setting open-browser-p)
              t)))
     (lambda ()
       (setf browser-message
             (provider-authenticate-with-method
              provider nil :stream output :open-browser-p nil)
             device-message
             (provider-authenticate-with-method
              provider "device" :stream output :open-browser-p nil))))
    (test-assert
     (and (= browser-login-count 1)
          (= device-login-count 1)
          (null browser-setting)
          (null device-setting)
          (string= browser-message
                   "ChatGPT authentication was saved by Autolith.")
          (string= device-message browser-message))
     "The ChatGPT auth command offers browser and device OAuth")
    (test-assert
     (handler-case
         (progn
           (provider-authenticate-with-method
            provider "invalid" :stream output :open-browser-p nil)
           nil)
       (authentication-error ()
         t))
     "The ChatGPT auth command rejects unknown authentication methods"))
  nil)