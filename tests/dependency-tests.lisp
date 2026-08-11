(in-package #:autolith)

;;;; -- Direct Lambda Symbolics Dependency Pins --

(defparameter *lambda-symbolics-github-prefix*
  "https://github.com/luciusmagn/"
  "The GitHub prefix identifying direct Lambda Symbolics library sources.")

(-> dependency-tests--file-lines (pathname) list)
(defun dependency-tests--file-lines (pathname)
  "Return every text line in PATHNAME, preserving empty lines."
  (with-open-file (stream pathname
                          :direction :input
                          :external-format :utf-8)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(-> dependency-tests--quoted-values (string) list)
(defun dependency-tests--quoted-values (line)
  "Return the strings between balanced double quotes in LINE."
  (loop with start = 0
        for opening = (position #\" line :start start)
        while opening
        for closing = (position #\" line :start (1+ opening))
        unless closing
          do (error "Unterminated quoted value in dependency metadata: ~S" line)
        collect (subseq line (1+ opening) closing)
        do (setf start (1+ closing))))

(-> dependency-tests--commit-ref-p (t) boolean)
(defun dependency-tests--commit-ref-p (value)
  "Return true when VALUE is a full lowercase hexadecimal Git commit."
  (and (stringp value)
       (= (length value) 40)
       (every (lambda (character)
                (or (digit-char-p character)
                    (find character "abcdef" :test #'char=)))
              value)
       t))

(-> dependency-tests--repository-from-url (string) (option string))
(defun dependency-tests--repository-from-url (url)
  "Return the Lambda Symbolics repository named by URL, or NIL for another owner."
  (when (uiop:string-prefix-p *lambda-symbolics-github-prefix* url)
    (let ((repository
            (subseq url (length *lambda-symbolics-github-prefix*))))
      (if (uiop:string-suffix-p repository ".git")
          (subseq repository 0 (- (length repository) 4))
          repository))))

(-> dependency-tests--pin (string string string) cons)
(defun dependency-tests--pin (repository reference source)
  "Return one validated REPOSITORY and REFERENCE pair read from SOURCE."
  (unless (and (non-empty-string-p repository)
               (dependency-tests--commit-ref-p reference))
    (error "Invalid direct dependency pin in ~A: ~S at ~S."
           source repository reference))
  (cons repository reference))

(-> dependency-tests--qlfile-pins (pathname) list)
(defun dependency-tests--qlfile-pins (pathname)
  "Return direct Lambda Symbolics repository pins from QLFILE PATHNAME."
  (loop for line in (dependency-tests--file-lines pathname)
        for words = (uiop:split-string line)
        for url = (and (>= (length words) 3) (third words))
        for repository = (and url (dependency-tests--repository-from-url url))
        when repository
          collect
          (progn
            (unless (and (= (length words) 5)
                         (string= (first words) "git")
                         (string= (fourth words) ":ref"))
              (error "Malformed direct Git dependency in qlfile: ~S" line))
            (dependency-tests--pin repository (fifth words) "qlfile"))))

(-> dependency-tests--qlfile-lock-pins (pathname) list)
(defun dependency-tests--qlfile-lock-pins (pathname)
  "Return direct Lambda Symbolics repository pins from QLFILE.LOCK PATHNAME."
  (loop for line in (dependency-tests--file-lines pathname)
        for values = (and (search ":remote-url" line)
                          (dependency-tests--quoted-values line))
        for url = (first values)
        for repository = (and url (dependency-tests--repository-from-url url))
        when repository
          collect
          (progn
            (unless (= (length values) 2)
              (error "Malformed direct Git dependency in qlfile.lock: ~S" line))
            (dependency-tests--pin repository (second values) "qlfile.lock"))))

(-> dependency-tests--nix-pins (pathname) list)
(defun dependency-tests--nix-pins (pathname)
  "Return direct Lambda Symbolics repository pins from Nix package PATHNAME."
  (let ((inside-source-p nil)
        (owner nil)
        (repository nil)
        (reference nil)
        (pins nil))
    (dolist (line (dependency-tests--file-lines pathname))
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond
          ((search "fetchFromGitHub {" trimmed)
           (setf inside-source-p t
                 owner nil
                 repository nil
                 reference nil))
          ((and inside-source-p (uiop:string-prefix-p "owner = " trimmed))
           (setf owner (first (dependency-tests--quoted-values trimmed))))
          ((and inside-source-p (uiop:string-prefix-p "repo = " trimmed))
           (setf repository (first (dependency-tests--quoted-values trimmed))))
          ((and inside-source-p (uiop:string-prefix-p "rev = " trimmed))
           (setf reference (first (dependency-tests--quoted-values trimmed))))
          ((and inside-source-p (string= trimmed "};"))
           (when (string= owner "luciusmagn")
             (push (dependency-tests--pin repository reference "nix/package.nix")
                   pins))
           (setf inside-source-p nil)))))
    (nreverse pins)))

(-> dependency-tests--sorted-pins (list string) list)
(defun dependency-tests--sorted-pins (pins source)
  "Return PINS sorted by repository, rejecting duplicates from SOURCE."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (pin pins)
      (when (gethash (first pin) seen)
        (error "Duplicate direct dependency ~A in ~A." (first pin) source))
      (setf (gethash (first pin) seen) t))
    (sort (copy-list pins) #'string< :key #'first)))

(-> test-lambda-symbolics-dependency-pins () null)
(defun test-lambda-symbolics-dependency-pins ()
  "Test direct Lambda Symbolics Git refs agree across Qlot and Nix metadata."
  (let* ((root (asdf:system-source-directory :autolith))
         (qlfile
           (dependency-tests--sorted-pins
            (dependency-tests--qlfile-pins (merge-pathnames "qlfile" root))
            "qlfile"))
         (lock
           (dependency-tests--sorted-pins
            (dependency-tests--qlfile-lock-pins
             (merge-pathnames "qlfile.lock" root))
            "qlfile.lock"))
         (nix
           (dependency-tests--sorted-pins
            (dependency-tests--nix-pins
             (merge-pathnames "nix/package.nix" root))
            "nix/package.nix")))
    (test-assert (plusp (length qlfile))
                 "the project declares direct Lambda Symbolics dependencies")
    (test-assert (equal (mapcar #'first qlfile) (mapcar #'first lock))
                 "qlfile and qlfile.lock name the same direct dependencies")
    (test-assert (equal (mapcar #'first qlfile) (mapcar #'first nix))
                 "qlfile and Nix name the same direct dependencies")
    (dolist (pin qlfile)
      (let* ((repository (first pin))
             (reference (rest pin)))
        (test-assert
         (equal reference (rest (assoc repository lock :test #'string=)))
         (format nil "qlfile.lock pins ~A at the qlfile commit" repository))
        (test-assert
         (equal reference (rest (assoc repository nix :test #'string=)))
         (format nil "Nix pins ~A at the qlfile commit" repository)))))
  nil)
