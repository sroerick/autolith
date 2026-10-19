(in-package #:autolith)

;;;; -- Public Catalog Fixtures --

(defclass model-catalog-test-context ()
  ()
  (:documentation "An explicit catalog owner independent of telemetry or provider configuration."))

(defmethod model-catalog-context-active-p ((context model-catalog-test-context))
  "Authorize this synchronous test context."
  (declare (ignore context))
  t)

(defmethod model-catalog-context-fetch ((context model-catalog-test-context))
  "Use the real bounded HTTP reader without telemetry's subprocess adapter."
  (declare (ignore context))
  (model-catalog--worker-fetch))

(defun test-model-catalog--http-fixture (mode body function &key (port 0))
  "Call FUNCTION with an ephemeral local HTTP source and port, then join its server."
  (let* ((listener (usocket:socket-listen "127.0.0.1" port :reuse-address t
                                                               :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener))
         (server nil))
    (unwind-protect
         (progn
           (if (eq mode ':unreachable)
               (usocket:socket-close listener)
               (setf server
                     (bt:make-thread
                      (lambda ()
                        (when (usocket:wait-for-input listener :timeout 2 :ready-only t)
                          (let* ((connection (usocket:socket-accept listener))
                                 (stream (usocket:socket-stream connection))
                                 (text (flexi-streams:make-flexi-stream stream :external-format :utf-8))
                                 (crlf (format nil "~C~C" #\Return #\Newline)))
                            (unwind-protect
                                 (progn
                                   (loop for line = (read-line text nil nil)
                                         while (and line (plusp (length (string-trim '(#\Return) line)))))
                                   (if (eq mode ':stalled)
                                       (sleep 0.4)
                                       (let ((bytes (babel:string-to-octets body :encoding :utf-8)))
                                         (write-sequence
                                          (babel:string-to-octets
                                           (format nil "HTTP/1.1 200 OK~AContent-Type: application/json~AContent-Length: ~D~AConnection: close~A~A"
                                                   crlf crlf (length bytes) crlf crlf crlf)
                                           :encoding :utf-8) stream)
                                         (write-sequence bytes stream)
                                         (finish-output stream))))
                              (usocket:socket-close connection)))))
                      :name "standalone model catalog HTTP fixture")))
           (funcall function (format nil "http://127.0.0.1:~D/api.json" port) port))
      (when server (bt:join-thread server))
      (ignore-errors (usocket:socket-close listener)))))

(defun test-model-catalog-standalone-http ()
  "Resolve real fixture HTTP membership and retain warm or cold state on network failure."
  (dolist (warm-p '(nil t))
    (dolist (mode '(:unreachable :stalled))
      (with-test-configuration (configuration)
        (let* ((root (config :cache-root configuration))
               (catalog (model-catalog-create :cache-root root))
               (context (make-instance 'model-catalog-test-context))
               (port 0)
               (saved nil)
               (fetched-at nil))
          (when warm-p
            (test-model-catalog--http-fixture
             ':ready (test-model-catalog--fixture '("fixture-new-model"))
             (lambda (source source-port)
               (let ((*model-catalog-source* source))
                 (setf port source-port)
                 (model-catalog--refresh catalog context nil)
                 (test-assert (model-catalog-known-p catalog "fixture-new-model")
                              "A new model from an actual models.dev-shaped HTTP response is known.")
                 (setf saved (uiop:read-file-string (model-catalog--path catalog))
                       fetched-at (model-catalog-fetched-at catalog)
                       catalog (model-catalog-create :cache-root root))
                 (model-catalog--load catalog)
                 (test-assert (model-catalog-known-p catalog "fixture-new-model")
                              "A separate standalone owner reloads last-good disk membership.")))))
          (test-model-catalog--http-fixture
           mode nil
           (lambda (source source-port)
             (declare (ignore source-port))
             (let ((*model-catalog-source* source)
                   (*model-catalog-request-seconds* 0.15)
                   (start (get-internal-real-time)))
               (model-catalog--refresh catalog context nil)
               (test-assert (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 2)
                            "Unreachable and stalled APIs finish within a whole-request bound.")
               (test-assert (eq (model-catalog-known-p catalog "fixture-new-model") warm-p)
                            "Warm failures use cache; cold failures do not invent membership.")
               (test-assert (not (model-catalog-known-p catalog "fixture-unlisted-model"))
                            "An unlisted model stays unknown.")
               (test-assert (eql (model-catalog-fetched-at catalog) fetched-at)
                            "Failure does not renew freshness.")
               (test-assert (if warm-p
                                (equal saved (uiop:read-file-string (model-catalog--path catalog)))
                                (not (probe-file (model-catalog--path catalog))))
                            "Failures preserve last-good cache and leave cold starts without one.")))
           :port port)))))
  nil)

(defun test-model-catalog--fixture (identifiers)
  "Return a small public models.dev fixture without access to any provider endpoint."
  (let ((models (make-hash-table :test #'equal)))
    (dolist (identifier identifiers)
      (setf (gethash identifier models) (json-object "id" identifier)))
    (json-encode (json-object "public-provider" (json-object "models" models)))))

(defun test-model-catalog-parser ()
  "Accept exact native and canonical IDs; reject malformed or changed schemas."
  (let* ((text "{\"p\":{\"models\":{\"native/v1\":{\"id\":\"native/v1\",\"canonical_model_id\":\"lab/base\",\"extra\":[]}}},\"q\":{\"models\":{\"native/v1\":{\"id\":\"native/v1\"},\"UPPER\":{\"id\":\"UPPER\"}}}}")
         (catalog (model-catalog-parse text)))
    (test-assert (= (hash-table-count catalog) 3) "Union deduplicates native and canonical IDs.")
    (dolist (identifier '("native/v1" "lab/base" "UPPER"))
      (test-assert (gethash identifier catalog) "Exact native and canonical IDs are public."))
    (dolist (identifier '("native/v1-private" "upper" "tenant/native/v1" "native/v1 "))
      (test-assert (null (gethash identifier catalog)) "No prefix or case normalization.")))
  (dolist (text '("" "{" "[]" "{}" "{\"p\":[]}" "{\"p\":{}}"
                  "{\"p\":{\"models\":[]}}" "{\"p\":{\"models\":{}}}"
                  "{\"p\":{\"models\":{\"x\":[]}}}"
                  "{\"p\":{\"models\":{\"x\":{\"id\":\"y\"}}}}"
                  "{\"p\":{\"models\":{\"x\":{\"id\":\"x\",\"canonical_model_id\":null}}}}"
                  "{\"p\":{\"models\":{\"x\":{\"id\":\"x\",\"canonical_model_id\":\"bad name\"}}}}"
                  "{\"p\":{\"models\":{\" x\":{\"id\":\" x\"}}}}"
                  "{\"p\":{\"models\":{\"x\":{\"id\":\"x\"}}},\"bad\":{\"models\":null}}"))
    (test-assert (handler-case (progn (model-catalog-parse text) nil) (error () t))
                 "Invalid input never yields a partial catalog."))
  (dolist (identifier (list "" (make-string 257 :initial-element #\a) "non ascii é"
                            (format nil "control~C" #\Newline)))
    (test-assert
     (handler-case (progn (model-catalog-parse (test-model-catalog--fixture
                                                   (list identifier))) nil)
       (error () t)) "IDs have a printable ASCII size bound."))
  (let ((*model-catalog-maximum-bytes* 32))
    (test-assert (handler-case
                    (progn (model-catalog-parse (make-string 33 :initial-element #\Space)) nil)
                  (model-catalog-invalid () t)) "Raw input has a hard byte limit.")))

(defun test-model-catalog-http-stream ()
  "Read an actual application/json HTTP response as bounded UTF-8 bytes."
  (dolist (oversized '(nil t))
    (let* ((body "{\"p\":{\"models\":{\"public-model\":{\"id\":\"public-model\"}},\"metadata\":\"é\"}}")
           (bytes (babel:string-to-octets body :encoding :utf-8))
           (listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                           :element-type '(unsigned-byte 8)))
           (server nil) (server-error nil))
      (unwind-protect
           (progn
             (setf server
                   (bt:make-thread
                    (lambda ()
                      (handler-case
                          (when (usocket:wait-for-input listener :timeout 5 :ready-only t)
                            (let* ((connection (usocket:socket-accept listener))
                                   (stream (usocket:socket-stream connection))
                                   (text (flexi-streams:make-flexi-stream stream :external-format :utf-8))
                                   (crlf (format nil "~C~C" #\Return #\Newline)))
                              (unwind-protect
                                   (progn
                                     (loop for line = (read-line text nil nil)
                                           while (and line (plusp (length (string-trim '(#\Return) line)))))
                                     (write-sequence
                                      (babel:string-to-octets
                                       (format nil "HTTP/1.1 200 OK~AContent-Type: application/json; charset=utf-8~AContent-Length: ~D~AConnection: close~A~A"
                                               crlf crlf (length bytes) crlf crlf crlf)
                                       :encoding :utf-8) stream)
                                     (write-sequence bytes stream)
                                     (finish-output stream))
                                (usocket:socket-close connection))))
                        (error (condition) (setf server-error condition))))
                    :name "public catalog HTTP fixture"))
             (let ((*model-catalog-source*
                     (format nil "http://127.0.0.1:~D/api.json" (usocket:get-local-port listener)))
                   (*model-catalog-maximum-bytes*
                     (if oversized (1- (length bytes)) (length bytes))))
               (if oversized
                   (test-assert (handler-case (progn (model-catalog--worker-fetch) nil)
                                  (model-catalog-invalid () t))
                                "The real HTTP stream enforces the byte limit.")
                   (test-assert (equal (model-catalog--worker-fetch) body)
                                "JSON content type does not change the binary stream contract.")))
             (bt:join-thread server)
             (test-assert (null server-error) "The local HTTP fixture completes without an error."))
        (when server (ignore-errors (bt:join-thread server)))
        (usocket:socket-close listener)))))
