(in-package #:autolith)

;;;; -- Lisp Source Balance Tests --

(-> lisp-source-balance-tests--balanced-p (pathname string) boolean)
(defun lisp-source-balance-tests--balanced-p (path content)
  "Return true when recognized PATH CONTENT has no delimiter issues."
  (null (lisp-source-balance-issues path content)))

(-> test-lisp-source-balance () null)
(defun test-lisp-source-balance ()
  "Test language-aware Lisp-family delimiter analysis and advisory warnings."
  (dolist (case
           '(("sample.ASD" :common-lisp)
             ("sample.cl" :common-lisp)
             ("sample.lisp" :common-lisp)
             ("sample.lsp" :common-lisp)
             ("sample.ros" :common-lisp)
             ("sample.sch" :scheme)
             ("sample.scheme" :scheme)
             ("sample.scm" :scheme)
             ("sample.sld" :scheme)
             ("sample.sls" :scheme)
             ("sample.ss" :scheme)
             ("sample.bb" :clojure)
             ("sample.clj" :clojure)
             ("sample.cljc" :clojure)
             ("sample.cljs" :clojure)
             ("sample.edn" :clojure)
             ("sample.txt" nil)))
    (destructuring-bind (name expected) case
      (test-assert
        (eq (lisp-source-balance-language (pathname name)) expected)
        (format nil "Lisp source language classification recognizes ~A" name))))
  (let ((backslash #\\))
    (dolist
        (case
         `(("Common Lisp strings, characters, symbols, and nested comments"
            ,#P"balanced.lisp"
            ,(format nil
                     "#| ( #| ) |# ) |#~%(list ~S #~C( #~C) '|(|) ; ( ignored~%"
                     ")"
                     backslash
                     backslash))
           ("Scheme strings, characters, escaped identifiers, brackets, datum comments, and block comments"
            ,#P"balanced.scm"
            ,(format nil
                     "#| ( #| ] |# ) |#~%(define value [list #~C( ~S |[|])~%#;(discarded (form))~%(define escaped foo~Cx28;)"
                     backslash
                     ")"
                     backslash))
           ("Clojure strings, regexes, characters, vectors, maps, and sets"
            ,#P"balanced.clj"
            ,(format nil
                     "{:character ~C( :named ~Cnewline :text ~S :items [1 2] :regex #~S :set #{3 4}} ; ]})"
                     backslash
                     backslash
                     "]"
                     "[(]"))))
      (destructuring-bind (label path content) case
        (test-assert
         (lisp-source-balance-tests--balanced-p path content)
         label))))
    (test-assert
     (lisp-source-balance-tests--balanced-p
      #P"escaped.lisp"
      (format nil "(list foo~C( foo~C))" #\\ #\\))
     "Common Lisp single escapes hide delimiter characters in symbols")
    (dolist (path '(#P"script.ros" #P"script.scm" #P"script.bb"))
      (test-assert
       (lisp-source-balance-tests--balanced-p
        path
        (format nil "#!/usr/bin/env lisp (~%(list)"))
       "First-line Lisp-family script headers ignore delimiter characters"))
  (test-assert
   (null
    (lisp-source-balance-issues
     #P"hex-character.scm"
     (format nil "#~Cx3b; (" #\\)))
   "A semicolon after a Scheme hex character literal starts a line comment")
  (let* ((issues
           (lisp-source-balance-issues
            #P"hex-identifier.scm"
            (format nil "foo~Cx28; (" #\\)))
         (issue (first issues))
         (delimiter (and issue (lisp-source-balance-issue-delimiter issue))))
    (test-assert
     (and (= (length issues) 1)
          (eq (lisp-source-balance-issue-kind issue) ':unclosed-open)
          (= (lisp-source-delimiter-line delimiter) 1)
          (= (lisp-source-delimiter-column delimiter) 10))
     "Scheme inline hex escapes preserve following structural delimiters"))
  (let* ((issues
           (lisp-source-balance-issues
            #P"missing.lisp"
            (format nil "(defun sample ()~%  (list 1)~%")))
         (issue (first issues))
         (delimiter (and issue (lisp-source-balance-issue-delimiter issue))))
    (test-assert
     (and (= (length issues) 1)
          (eq (lisp-source-balance-issue-kind issue) ':unclosed-open)
          (char= (lisp-source-delimiter-character delimiter) #\()
          (= (lisp-source-delimiter-line delimiter) 1)
          (= (lisp-source-delimiter-column delimiter) 1))
     "Common Lisp analysis locates an unmatched opening parenthesis"))
  (let* ((issues
           (lisp-source-balance-issues #P"extra.scm" "(define x 1))"))
         (issue (first issues))
         (delimiter (and issue (lisp-source-balance-issue-delimiter issue))))
    (test-assert
     (and (= (length issues) 1)
          (eq (lisp-source-balance-issue-kind issue) ':unexpected-close)
          (char= (lisp-source-delimiter-character delimiter) #\))
          (= (lisp-source-delimiter-line delimiter) 1)
          (= (lisp-source-delimiter-column delimiter) 13))
     "Scheme analysis locates an extra closing parenthesis"))
  (let* ((issues
           (lisp-source-balance-issues #P"mismatch.clj" "{:x [1 2)}"))
         (issue (first issues))
         (delimiter (and issue (lisp-source-balance-issue-delimiter issue)))
         (opener (and issue (lisp-source-balance-issue-opener issue))))
    (test-assert
     (and (= (length issues) 1)
          (eq (lisp-source-balance-issue-kind issue) ':mismatched-close)
          (char= (lisp-source-delimiter-character delimiter) #\))
          (char= (lisp-source-delimiter-character opener) #\[)
          (char= (lisp-source-balance-issue-expected-character issue) #\]))
     "Clojure analysis detects delimiter type mismatches"))
  (test-assert
   (= (length (lisp-source-balance-issues #P"discard.scm" "#;(foo")) 1)
   "Scheme datum comments still require a balanced discarded datum")
  (test-assert
   (= (length (lisp-source-balance-issues #P"discard.clj" "#_(")) 1)
   "Clojure discard forms still require a balanced discarded form")
  (test-assert
   (null (lisp-source-balance-issues #P"string.lisp" "\"("))
   "An opening parenthesis inside an unterminated string is not structural")
  (test-assert
   (null (lisp-source-balance-issues #P"plain.txt" ")))"))
   "Unknown file extensions are not analyzed")
  (multiple-value-bind (issues count)
      (lisp-source-balance--scan
       ':clojure
       (make-string 4096 :initial-element #\))
       :issue-limit 2)
    (test-assert
     (and (= (length issues) 2)
          (= count 4096))
     "Bounded warning scans retain only their requested diagnostic limit"))
  (let ((warning
          (lisp-source-balance-warning #P"bounded.clj" ")))))))")))
    (test-assert
     (and (search "7 unmatched or mismatched delimiters" warning)
          (search "1 additional issue omitted" warning))
     "Model-facing warnings bound individual diagnostics and report omissions"))
  (test-assert
   (string=
    (lisp-source-edit-result-content "Applied." #P"balanced.lisp" "(list 1)")
    "Applied.")
   "Balanced Lisp-family edit results remain unchanged")
  (test-assert
   (string=
    (lisp-source-edit-result-content "Applied." #P"plain.txt" "))")
    "Applied.")
   "Non-Lisp edit results remain unchanged")
  (let ((content
          (lisp-source-edit-result-content
           "Applied."
           #P"warning.clj"
           "{:value [1 2}")))
    (test-assert
     (and (search "Applied." content)
          (search "WARNING: The edit succeeded" content)
          (search "expected `]`" content))
     "Unbalanced Lisp-family edit results carry a successful advisory warning"))
  nil)


;;;; -- Explicit Parenthesis Check Tool Tests --

(-> test-lisp-paren-check-tool () null)
(defun test-lisp-paren-check-tool ()
  "Test bounded file, recursive directory, failure, and authority behavior."
  (let* ((registry (make-default-tool-registry))
         (base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "lisp-paren-check"))
                (context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation)))
           (labels ((call (&rest arguments)
                      "Execute lisp.paren-check with ARGUMENTS."
                      (tool-registry-execute-call
                       registry
                       (json-object
                        "namespace" "lisp"
                        "name" "paren-check"
                        "arguments"
                        (json-encode (apply #'json-object arguments)))
                       context))

                    (write-text (path content)
                      "Replace PATH with UTF-8 CONTENT."
                      (ensure-directories-exist path)
                      (with-open-file (stream path
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create
                                              :external-format :utf-8)
                        (write-string content stream))))
             (let* ((tree (merge-pathnames "source/" root))
                    (nested (merge-pathnames "nested/" tree))
                    (balanced (merge-pathnames "balanced.lisp" tree))
                    (scheme (merge-pathnames "valid.scm" nested))
                    (broken (merge-pathnames "bad.clj" nested))
                    (ignored (merge-pathnames "ignored.txt" tree)))
               (write-text balanced "(defun balanced () 42)")
               (write-text scheme "(define values [1 2 3])")
               (write-text broken "{:x [1 2)}")
               (write-text ignored ")))")
               (let* ((result (call "path" (namestring tree)))
                      (content (tool-result-content result)))
                 (test-assert
                  (and (not (tool-result-success-p result))
                       (search "Checked 3 of 3 recognized Lisp-family files" content)
                       (search "Found 1 unmatched or mismatched delimiter" content)
                       (search "nested/bad.clj" content)
                       (search "expected `]`" content)
                       (not (search "ignored.txt" content)))
                  "lisp.paren-check recursively checks recognized files with exact diagnostics"))
               (let* ((result (call "path" (namestring balanced)))
                      (content (tool-result-content result)))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "Checked 1 of 1 recognized Lisp-family file" content)
                       (search "No unmatched or mismatched delimiters found" content))
                  "lisp.paren-check accepts one balanced source file"))
               (write-text broken "{:x [1 2]}")
               (let ((result (call "path" (namestring tree))))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "Checked 3 of 3 recognized Lisp-family files"
                               (tool-result-content result)))
                  "lisp.paren-check succeeds after every recognized file is balanced"))
               (test-assert
                (not (tool-result-success-p
                      (call "path" (namestring ignored))))
                "lisp.paren-check rejects an explicitly unknown file type")
               (test-assert
                (not (tool-result-success-p
                      (call "path"
                            (namestring (merge-pathnames "missing.lisp" root)))))
                "lisp.paren-check rejects a missing path")
               (test-assert
                (not (tool-result-success-p (call)))
                "lisp.paren-check requires its path argument")
               (let ((plain-directory (merge-pathnames "plain/" root)))
                 (write-text (merge-pathnames "only.txt" plain-directory) "(")
                 (let ((result (call "path" (namestring plain-directory))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "No recognized Common Lisp, Scheme, or Clojure files"
                                 (tool-result-content result)))
                    "lisp.paren-check refuses to claim success for an empty source set")))
                (let* ((outside (merge-pathnames "outside/" root))
                       (outside-file
                         (merge-pathnames "outside.lisp" outside)))
                  (write-text outside-file "(list 1)")
                  (let ((*workspace-tool-readable-roots* (list tree)))
                    (test-assert
                     (not (tool-result-success-p
                           (call "path" (namestring outside))))
                     "lisp.paren-check honors restricted workspace read roots"))
                  (let ((escape (merge-pathnames "escape" tree)))
                    (unwind-protect
                         (progn
                           (sb-posix:symlink (namestring outside)
                                             (namestring escape))
                           (let ((result (call "path" (namestring tree))))
                             (test-assert
                              (and (not (tool-result-success-p result))
                                   (search "outside the requested root"
                                           (tool-result-content result)))
                              "recursive checks refuse symbolic-link escapes")))
                      (when (probe-file escape)
                        (sb-posix:unlink (namestring escape)))))
                  (let ((link (merge-pathnames "balanced-link.lisp" tree)))
                    (unwind-protect
                         (progn
                           (sb-posix:symlink (namestring balanced)
                                             (namestring link))
                           (let ((*workspace-tool-readable-roots* (list tree)))
                             (test-assert
                              (tool-result-success-p
                               (call "path" (namestring link)))
                              "direct checks accept stable source symlinks inside the readable root")))
                      (when (probe-file link)
                        (sb-posix:unlink (namestring link)))))
                  (let* ((race (merge-pathnames "race.lisp" tree))
                         (backup (merge-pathnames "race-original.lisp" tree))
                         (outside-race
                           (merge-pathnames "race-target.lisp" outside)))
                    (write-text race "()")
                    (with-open-file (stream outside-race
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create
                                            :element-type '(unsigned-byte 8))
                      (write-byte 255 stream))
                    (unwind-protect
                         (let ((*workspace-tool-readable-roots* (list tree))
                                (*lisp-paren-check-after-first-read-function*
                                 (lambda (opened)
                                   (declare (ignore opened))
                                   (rename-file race backup)
                                   (sb-posix:symlink
                                    (namestring outside-race)
                                    (namestring race)))))
                           (let* ((result (call "path" (namestring race)))
                                  (content (tool-result-content result)))
                             (test-assert
                              (and (not (tool-result-success-p result))
                                   (search "outside the readable workspace and source roots"
                                           content)
                                   (not (search "not valid UTF-8" content)))
                              "descriptor checks reject a path replacement before reading outside authority")))
                      (when (probe-file backup)
                        (ignore-errors (sb-posix:unlink (namestring race)))
                        (rename-file backup race)))))
                (let* ((outside (merge-pathnames "outside/" root))
                       (directory-race
                         (merge-pathnames "directory-race/" root))
                       (directory-link
                         (merge-pathnames "directory-race" root))
                       (directory-backup
                         (merge-pathnames "directory-race-original/" root)))
                    (write-text
                     (merge-pathnames "inside.lisp" directory-race)
                     "()")
                    (unwind-protect
                         (let ((*workspace-tool-readable-roots*
                                 (list directory-race))
                               (*lisp-paren-check-after-directory-open-function*
                                 (lambda (opened)
                                   (declare (ignore opened))
                                   (rename-file directory-race directory-backup)
                                   (sb-posix:symlink
                                    (namestring outside)
                                    (namestring directory-link)))))
                           (let* ((result
                                    (call "path" (namestring directory-race)))
                                  (content (tool-result-content result)))
                             (test-assert
                              (and (not (tool-result-success-p result))
                                   (search "changed during traversal" content)
                                   (not (search "outside.lisp" content)))
                              "directory descriptor checks reject replacement before outside traversal")))
                      (when (probe-file directory-backup)
                        (ignore-errors
                          (sb-posix:unlink (namestring directory-link)))
                        (rename-file directory-backup directory-race))))
                  (let ((mutable (merge-pathnames "mutable.lisp" tree)))
                    (write-text mutable "()")
                    (let ((*lisp-paren-check-after-first-read-function*
                            (lambda (opened)
                              (declare (ignore opened))
                              (with-open-file (stream mutable
                                                      :direction :output
                                                      :if-exists :overwrite
                                                      :external-format :utf-8)
                                (write-string ")(" stream)
                                (finish-output stream)))))
                      (let* ((result (call "path" (namestring mutable)))
                             (content (tool-result-content result)))
                        (test-assert
                         (and (not (tool-result-success-p result))
                              (search "changed while it was being read" content))
                         "repeated descriptor snapshots reject same-size in-place changes"))))
               (let ((limited (merge-pathnames "file-limit/" root)))
                 (write-text (merge-pathnames "one.lisp" limited) "()")
                 (write-text (merge-pathnames "two.lisp" limited) "()")
                 (let* ((*lisp-paren-check-file-limit* 1)
                        (result (call "path" (namestring limited))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "more than its 1-file limit"
                                 (tool-result-content result)))
                    "lisp.paren-check fails instead of silently truncating discovery")))
               (let ((large (merge-pathnames "large.lisp" root)))
                 (write-text large "(list 1)")
                 (let* ((*lisp-paren-check-file-character-limit* 4)
                        (result (call "path" (namestring large)))
                        (content (tool-result-content result)))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "Checked 0 of 1" content)
                         (search "per-file limit of 4 characters" content))
                    "lisp.paren-check reports files above its per-file input bound")))
               (let ((aggregate (merge-pathnames "aggregate/" root)))
                 (write-text (merge-pathnames "a.lisp" aggregate) "()")
                 (write-text (merge-pathnames "b.lisp" aggregate) "()")
                 (let* ((*lisp-paren-check-total-character-limit* 2)
                        (result (call "path" (namestring aggregate)))
                        (content (tool-result-content result)))
                   (test-assert
                    (and (not (tool-result-success-p result))
                         (search "Checked 1 of 2" content)
                         (search "aggregate source limit of 2 characters" content))
                    "lisp.paren-check reports incomplete aggregate-budget checks")))
               (let ((malformed (merge-pathnames "malformed.lisp" root)))
                 (with-open-file (stream malformed
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create
                                         :element-type '(unsigned-byte 8))
                   (write-byte 255 stream))
                 (let ((result (call "path" (namestring malformed))))
                   (test-assert
                    (and (not (tool-result-success-p result))
                           (search "not valid UTF-8"
                                   (tool-result-content result)))
                    "lisp.paren-check reports malformed source without crashing")))
                (dolist (limit '(0 1 10 45 46 47 48 49 50))
                  (let* ((*lisp-paren-check-result-character-limit* limit)
                         (content
                           (lisp-paren-check--bounded-result
                            (make-string 200 :initial-element #\x))))
                    (test-assert
                     (<= (length content) limit)
                     "lisp.paren-check never exceeds a tiny result bound")))
               (let ((many (merge-pathnames "many-issues.lisp" root)))
                 (write-text many (make-string 200 :initial-element #\)))
                 (let* ((*lisp-paren-check-issue-limit-per-file* 100)
                        (*lisp-paren-check-result-character-limit* 220)
                        (result (call "path" (namestring many)))
                        (content (tool-result-content result)))
                  (test-assert
                   (and (not (tool-result-success-p result))
                        (<= (length content) 220)
                        (search "diagnostic output truncated" content))
                   "lisp.paren-check bounds its complete model-visible result"))))))
      (tool-registry-close-runtime-state registry)
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)

;;;; -- Editing Tool Warning Tests --

(-> lisp-source-balance-tests--field (string string) string)
(defun lisp-source-balance-tests--field (content label)
  "Return the single-line value following LABEL in model-visible CONTENT."
  (let* ((start (search label content))
         (value-start (and start (+ start (length label))))
         (end (and value-start
                   (or (position #\Newline content :start value-start)
                       (length content)))))
    (unless (and value-start end)
      (error "Missing result field ~S in ~S." label content))
    (subseq content value-start end)))

(-> test-lisp-source-edit-warnings () null)
(defun test-lisp-source-edit-warnings ()
  "Test advisory delimiter warnings through every model-facing text editor."
  (let* ((registry (make-default-tool-registry))
         (base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration
                                       :identifier "lisp-source-warnings"))
                (context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation conversation)))
           (labels ((call (namespace name &rest arguments)
                      "Execute one editing tool through the default registry."
                      (tool-registry-execute-call
                       registry
                       (json-object
                        "namespace" namespace
                        "name" name
                        "arguments"
                        (json-encode (apply #'json-object arguments)))
                       context)))
             (dolist (specification
                      '(("warning.lisp" "(" "Common Lisp")
                        ("warning.scm" "(" "Scheme")
                        ("warning.clj" "[" "Clojure")))
               (destructuring-bind (name content language) specification
                 (let* ((path (merge-pathnames name root))
                        (result
                          (call "fs" "write"
                                "path" (namestring path)
                                "content" content)))
                   (test-assert
                    (and (tool-result-success-p result)
                         (search "WARNING: The edit succeeded"
                                 (tool-result-content result))
                         (search language (tool-result-content result))
                         (string= (uiop:read-file-string path) content))
                    (format nil
                            "fs.write returns a successful ~A delimiter warning"
                            language)))))
             (let* ((path (merge-pathnames "plain.txt" root))
                    (result
                      (call "fs" "write"
                            "path" (namestring path)
                            "content" ")")))
               (test-assert
                (and (tool-result-success-p result)
                     (not (search "WARNING:" (tool-result-content result))))
                "fs.write does not analyze unknown file types"))
             (let* ((path (merge-pathnames "edited.lisp" root))
                    (write-result
                      (call "fs" "write"
                            "path" (namestring path)
                            "content" "(list 1)"))
                    (warning-result
                      (call "fs" "edit"
                            "path" (namestring path)
                            "old-text" "(list 1)"
                            "new-text" "(list 1"))
                    (repair-result
                      (call "fs" "edit"
                            "path" (namestring path)
                            "old-text" "(list 1"
                            "new-text" "(list 1)")))
               (test-assert
                (and (tool-result-success-p write-result)
                     (not (search "WARNING:"
                                  (tool-result-content write-result))))
                "fs.write leaves balanced Lisp results unchanged")
               (test-assert
                (and (tool-result-success-p warning-result)
                     (search "opening `(` has no matching `)`"
                             (tool-result-content warning-result)))
                "fs.edit warns after applying an unmatched opening parenthesis")
               (test-assert
                (and (tool-result-success-p repair-result)
                     (not (search "WARNING:"
                                  (tool-result-content repair-result)))
                     (string= (uiop:read-file-string path) "(list 1)"))
                "fs.edit removes its warning after the file is balanced"))
             (let* ((path (merge-pathnames "relaxed.scm" root))
                    (original
                      (format nil
                              "(define (sample)~%  (alpha)~%  (omega))~%"))
                    (old-text
                      (format nil
                              " (define (sample)~%   (alpha)~%   (omega))~%"))
                    (new-text
                      (format nil
                              " (define (sample)~%   (alpha)~%   (omega)~%")))
               (call "fs" "write"
                     "path" (namestring path)
                     "content" original)
               (let ((result
                       (call "fs" "edit"
                             "path" (namestring path)
                             "old-text" old-text
                             "new-text" new-text)))
                 (test-assert
                  (and (tool-result-success-p result)
                       (search "shifted new-text left by 1 space"
                               (tool-result-content result))
                       (search "Scheme delimiter checking"
                               (tool-result-content result)))
                  "fs.edit relaxed replacements retain delimiter warnings")))
             (let* ((path (merge-pathnames "resource.clj" root))
                    (uri "workspace:resource.clj"))
               (call "fs" "write"
                     "path" (namestring path)
                     "content" (format nil "(:ok)~%"))
               (let* ((read-result
                        (call "resource" "read" "uri" uri))
                      (read-content (tool-result-content read-result))
                      (canonical-uri
                        (lisp-source-balance-tests--field
                         read-content "URI: "))
                      (revision
                        (lisp-source-balance-tests--field
                         read-content "Revision: "))
                      (edit-result
                        (call
                         "resource" "edit"
                         "uri" canonical-uri
                         "base-revision" revision
                         "operations"
                         (vector
                          (json-object "op" "replace-lines"
                                       "start-line" 1
                                       "end-line" 1
                                       "content" "(:ok")))))
                 (test-assert
                  (and (tool-result-success-p edit-result)
                       (search "WARNING: The edit succeeded"
                               (tool-result-content edit-result))
                       (search "Clojure delimiter checking"
                               (tool-result-content edit-result))
                       (string= (uiop:read-file-string path)
                                (format nil "(:ok~%")))
                  "resource.edit warns in its successful model-visible result")))
              (let* ((write-result
                       (call "lisp" "scratchpad-write"
                             "path" "warning.clj"
                             "content" "["))
                     (repair-result
                       (call "lisp" "scratchpad-edit"
                             "path" "warning.clj"
                             "old-text" "["
                             "new-text" "[]"))
                     (warning-result
                       (call "lisp" "scratchpad-edit"
                             "path" "warning.clj"
                             "old-text" "[]"
                             "new-text" "[")))
                (test-assert
                 (and (tool-result-success-p write-result)
                      (search "Clojure delimiter checking"
                              (tool-result-content write-result)))
                 "lisp.scratchpad-write returns delimiter warnings to the model")
                (test-assert
                 (and (tool-result-success-p repair-result)
                      (not (search "WARNING:"
                                   (tool-result-content repair-result))))
                 "lisp.scratchpad-edit reports a balanced repaired result")
                (test-assert
                 (and (tool-result-success-p warning-result)
                      (search "Clojure delimiter checking"
                              (tool-result-content warning-result)))
                 "lisp.scratchpad-edit returns delimiter warnings to the model"))))
      (tool-registry-close-runtime-state registry)
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)
