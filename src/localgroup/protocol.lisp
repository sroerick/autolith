(in-package #:autolith)

;;;; -- Localgroup Wire Protocol --

(defparameter *localgroup-protocol-version* 1
  "The local loopback control protocol version.")

(defparameter *localgroup-packet-character-limit* (* 8 1024 1024)
  "The maximum characters accepted in one localgroup packet.")

(defparameter *localgroup-connect-timeout-seconds* 2
  "The maximum seconds allowed for one localgroup connection attempt.")

;; These runtime functions load after the responsive input implementation.
(-> application-localgroup-paused-p (t) boolean)
(-> application-localgroup-resume (t) boolean)

(define-condition localgroup-error (autolith-error)
  ((operation
    :initarg :operation
    :reader localgroup-error-operation
    :type keyword
    :documentation "The localgroup operation that failed.")
   (session-id
    :initarg :session-id
    :initform nil
    :reader localgroup-error-session-id
    :type (option string)
    :documentation "The requested localgroup session identifier, when known.")
   (cause
    :initarg :cause
    :initform nil
    :reader localgroup-error-cause
    :type t
    :documentation "The underlying failure, when one exists."))
  (:documentation "A local session discovery, transport, or control failure."))

(-> localgroup--proper-list-p (t) boolean)
(defun localgroup--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (not
   (null
    (handler-case
        (let ((length (list-length value)))
          (and length t))
      (type-error () nil)))))

(-> localgroup--hex-string ((simple-array (unsigned-byte 8) (*))) string)
(defun localgroup--hex-string (bytes)
  "Return lowercase hexadecimal text for BYTES."
  (with-output-to-string (stream)
    (loop for byte across bytes
          do (format stream "~2,'0x" byte))))

(-> localgroup-random-token () string)
(defun localgroup-random-token ()
  "Return a fresh unguessable capability token."
  (localgroup--hex-string (random-data 32)))

(-> localgroup-random-session-id () string)
(defun localgroup-random-session-id ()
  "Return a concise process-local session identifier."
  (subseq (localgroup--hex-string (random-data 8)) 0 12))

(-> localgroup--packet-string (list) string)
(defun localgroup--packet-string (packet)
  "Return PACKET as one readable line without reader evaluation syntax."
  (with-standard-io-syntax
    (let ((*print-circle* nil)
          (*print-readably* t)
          (*print-pretty* nil))
      (with-output-to-string (stream)
        (write packet :stream stream)
        (terpri stream)))))

(-> localgroup-write-packet (stream list) null)
(defun localgroup-write-packet (stream packet)
  "Write and flush one readable localgroup PACKET to STREAM."
  (write-string (localgroup--packet-string packet) stream)
  (finish-output stream)
  nil)

(-> localgroup--read-line-bounded (stream) (option string))
(defun localgroup--read-line-bounded (stream)
  "Read one bounded line from STREAM, returning NIL only at clean end of file."
  (let ((characters (make-array 128
                                :element-type 'character
                                :adjustable t
                                :fill-pointer 0)))
    (loop for character = (read-char stream nil nil)
          do (cond
               ((null character)
                (return
                  (and (plusp (length characters))
                       (coerce characters 'string))))
               ((char= character #\Newline)
                (return (coerce characters 'string)))
               ((>= (length characters) *localgroup-packet-character-limit*)
                (error 'localgroup-error
                       :message "A localgroup packet exceeded the configured limit."
                       :operation ':read-packet))
               (t
                (vector-push-extend character characters))))))

(-> localgroup-read-packet (stream) (option list))
(defun localgroup-read-packet (stream)
  "Read one safe, proper-list localgroup packet from STREAM."
  (let ((line (localgroup--read-line-bounded stream)))
    (unless line
      (return-from localgroup-read-packet nil))
    (handler-case
        (with-standard-io-syntax
          (let ((*read-eval* nil)
                (*readtable* (copy-readtable nil)))
            (multiple-value-bind (packet position)
                (read-from-string line nil nil)
              (unless (and packet
                           (localgroup--proper-list-p packet)
                           (every (lambda (character)
                                    (find character '(#\Space #\Tab #\Return)))
                                  (subseq line position)))
                (error 'localgroup-error
                       :message "A localgroup packet is malformed."
                       :operation ':read-packet))
              packet)))
      (localgroup-error (condition)
        (error condition))
      (error (condition)
        (error 'localgroup-error
               :message "A localgroup packet could not be read safely."
               :operation ':read-packet
               :cause condition)))))

(-> localgroup--socket-stream (sb-bsd-sockets:socket) stream)
(defun localgroup--socket-stream (socket)
  "Return a buffered UTF-8 character stream for SOCKET."
  (sb-bsd-sockets:socket-make-stream
   socket
   :input t
   :output t
   :element-type 'character
   :external-format :utf-8
   :buffering :full))

(-> localgroup-connect (integer) (values sb-bsd-sockets:socket stream))
(defun localgroup-connect (port)
  "Connect to the loopback localgroup endpoint at PORT."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type ':stream
                               :protocol ':tcp))
        (stream nil))
    (handler-case
        (progn
          (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)
          (sb-bsd-sockets:socket-connect
           socket
           (sb-bsd-sockets:make-inet-address "127.0.0.1")
           port)
          (setf stream (localgroup--socket-stream socket))
          (values socket stream))
      (error (condition)
        (if stream
            (ignore-errors (close stream))
            (ignore-errors (sb-bsd-sockets:socket-close socket)))
        (error 'localgroup-error
               :message (format nil "Could not connect to localgroup port ~D." port)
               :operation ':connect
               :cause condition)))))

(-> localgroup-call
    (integer string keyword &optional list)
    list)
(defun localgroup-call (port token operation &optional arguments)
  "Perform one authenticated localgroup OPERATION and return its response."
  (multiple-value-bind (socket stream)
      (localgroup-connect port)
    (declare (ignore socket))
    (unwind-protect
         (progn
           (localgroup-write-packet
            stream
            (list :localgroup-request
                  :version *localgroup-protocol-version*
                  :token token
                  :operation operation
                  :arguments arguments))
           (or (localgroup-read-packet stream)
               (error 'localgroup-error
                      :message "The localgroup endpoint closed without a response."
                      :operation operation)))
      (ignore-errors (close stream)))))
