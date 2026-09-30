(in-package #:autolith)

;;;; -- Active-Image Symbol Search --

(defparameter *lisp-apropos-kinds*
  '(:function :macro :generic-function :class :condition :variable :type)
  "The definition kinds lisp.apropos reports and accepts as filters.")

(defparameter *lisp-apropos-default-limit* 40
  "How many matches lisp.apropos lists when the call sets no limit.")

(defparameter *lisp-apropos-maximum-limit* 200
  "The most matches one lisp.apropos call may list.")

(defparameter *lisp-apropos-documentation-characters* 110
  "The longest documentation excerpt shown beside one match.")

(defparameter *lisp-apropos-suggestion-limit* 3
  "How many near-miss names a failed exact lookup proposes.")

(defparameter *lisp-apropos-token-minimum* 3
  "The shortest hyphen-separated name token that counts toward a near miss.")

(-> self-symbol-kinds (symbol) list)
(defun self-symbol-kinds (symbol)
  "Return the definition kinds SYMBOL carries in the active image.

Keywords and the constants T and NIL never count as variables or types, so
plain interned names without a definition yield NIL."
  (let ((kinds '()))
    (cond
      ((macro-function symbol)
       (push ':macro kinds))
      ((special-operator-p symbol)
       (push ':function kinds))
      ((and (fboundp symbol)
            (typep (fdefinition symbol) 'generic-function))
       (push ':generic-function kinds))
      ((fboundp symbol)
       (push ':function kinds)))
    (when (and (boundp symbol)
               (not (keywordp symbol))
               (not (member symbol '(t nil))))
      (push ':variable kinds))
    (let ((class (find-class symbol nil)))
      (cond
        (class
         (push (if (subtypep class 'condition) ':condition ':class) kinds))
        ((and (not (member symbol '(t nil)))
              (not (keywordp symbol))
              (sb-ext:valid-type-specifier-p symbol))
         (push ':type kinds))))
    (nreverse kinds)))

(-> lisp-apropos--present-symbols (package) list)
(defun lisp-apropos--present-symbols (package)
  "Return the symbols present in PACKAGE, imported ones included, inherited ones excluded."
  (let ((symbols '()))
    (do-symbols (symbol package)
      (multiple-value-bind (found status)
          (find-symbol (symbol-name symbol) package)
        (when (and (eq found symbol)
                   (not (eq status ':inherited)))
          (push symbol symbols))))
    (remove-duplicates symbols :test #'eq)))

(-> lisp-apropos--terms (string) list)
(defun lisp-apropos--terms (query)
  "Return the lowercase whitespace-separated search terms of QUERY."
  (let ((terms '())
        (start nil))
    (loop for index from 0 to (length query)
          for character = (and (< index (length query)) (char query index))
          do (cond
               ((and character
                     (not (member character '(#\Space #\Tab #\Newline #\Return))))
                (unless start
                  (setf start index)))
               (start
                (push (string-downcase (subseq query start index)) terms)
                (setf start nil))))
    (nreverse terms)))

(-> lisp-apropos-search
    (string &key (:package package) (:kind (option keyword)))
    list)
(defun lisp-apropos-search (query &key (package (find-package '#:autolith)) kind)
  "Return (SYMBOL . KINDS) pairs for defined symbols of PACKAGE matching QUERY.

Every whitespace-separated term of QUERY must occur in the symbol name,
compared without regard to case. KIND keeps only symbols carrying that
definition kind. Symbols without any definition are never returned, so
names interned by earlier misspelled lookups stay invisible. Matches are
sorted by name."
  (let ((terms (lisp-apropos--terms query))
        (matches '()))
    (dolist (symbol (lisp-apropos--present-symbols package))
      (let ((name (string-downcase (symbol-name symbol))))
        (when (every (lambda (term) (search term name)) terms)
          (let ((kinds (self-symbol-kinds symbol)))
            (when (and kinds
                       (or (null kind) (member kind kinds)))
              (push (cons symbol kinds) matches))))))
    (sort matches #'string< :key (lambda (match) (symbol-name (first match))))))

(-> lisp-apropos--definition-pathname (symbol list) (option pathname))
(defun lisp-apropos--definition-pathname (symbol kinds)
  "Return the first SBCL-recorded source file defining SYMBOL under one of KINDS."
  (require :sb-introspect)
  (dolist (kind kinds nil)
    (dolist (source (uiop:symbol-call '#:sb-introspect
                                      '#:find-definition-sources-by-name
                                      symbol kind))
      (let ((pathname (uiop:symbol-call '#:sb-introspect
                                        '#:definition-source-pathname
                                        source)))
        (when pathname
          (return-from lisp-apropos--definition-pathname pathname))))))

(-> lisp-apropos--tracked-path (symbol list configuration) (option string))
(defun lisp-apropos--tracked-path (symbol kinds configuration)
  "Return SYMBOL's defining file relative to the tracked source root, if it lies there."
  (let ((pathname (lisp-apropos--definition-pathname symbol kinds))
        (source-root (config :source-root configuration)))
    (when (and pathname (uiop:subpathp pathname source-root))
      (enough-namestring pathname source-root))))

(-> lisp-apropos--documentation-line (symbol list) (option string))
(defun lisp-apropos--documentation-line (symbol kinds)
  "Return the bounded first documentation line of SYMBOL's primary definition."
  (let ((documentation
          (or (and (intersection kinds '(:function :macro :generic-function))
                   (documentation symbol 'function))
              (and (member ':variable kinds)
                   (documentation symbol 'variable))
              (and (intersection kinds '(:class :condition :type))
                   (documentation symbol 'type)))))
    (when (non-empty-string-p documentation)
      (let* ((line (subseq documentation
                           0 (or (position #\Newline documentation)
                                 (length documentation))))
             (trimmed (string-trim '(#\Space #\Tab) line)))
        (if (> (length trimmed) *lisp-apropos-documentation-characters*)
            (concatenate 'string
                         (subseq trimmed 0 *lisp-apropos-documentation-characters*)
                         "...")
            trimmed)))))

(-> lisp-apropos--symbol-label (symbol package) string)
(defun lisp-apropos--symbol-label (symbol package)
  "Return SYMBOL's lowercase name, qualified when its home is not PACKAGE."
  (let ((home (symbol-package symbol)))
    (if (or (null home) (eq home package))
        (string-downcase (symbol-name symbol))
        (format nil "~(~A:~A~)" (package-name home) (symbol-name symbol)))))

(-> lisp-apropos-render
    (list &key (:query string) (:package package) (:limit (integer 1))
          (:configuration configuration))
    string)
(defun lisp-apropos-render (matches &key query package limit configuration)
  "Render MATCHES for QUERY over PACKAGE, listing at most LIMIT entries."
  (with-output-to-string (stream)
    (cond
      ((null matches)
       (format stream "No defined name in ~A matches ~S. Try fewer or shorter terms, another package, or search.content over the tracked source."
               (package-name package) query))
      (t
       (format stream "~D defined name~:P in ~A match~:[~;es~] ~S~:[ (showing the first ~D)~;~]:~%"
               (length matches) (package-name package) (= (length matches) 1)
               query (<= (length matches) limit) limit)
       (loop for (symbol . kinds) in matches
             repeat limit
             do (format stream "~%~A  ~{~(~A~)~^, ~}~@[  ~A~]~%"
                        (lisp-apropos--symbol-label symbol package)
                        kinds
                        (lisp-apropos--tracked-path symbol kinds configuration))
                (let ((lambda-list (and (intersection kinds '(:function :macro :generic-function))
                                        (self-symbol-lambda-list symbol)))
                      (documentation (lisp-apropos--documentation-line symbol kinds)))
                  (when (or lambda-list documentation)
                    (format stream "  ~@[~(~S~)~]~:[~;  ~]~@[~A~]~%"
                            lambda-list
                            (and lambda-list documentation)
                            documentation))))))))

(-> lisp-apropos--kind-argument (t) (option keyword))
(defun lisp-apropos--kind-argument (value)
  "Return the definition kind keyword named by tool argument VALUE, or NIL for all kinds."
  (cond
    ((null value)
     nil)
    ((and (stringp value)
          (find (string-upcase value) *lisp-apropos-kinds* :key #'symbol-name
                                                           :test #'string=)))
    (t
     (error 'tool-error
            :message (format nil "Unknown definition kind ~S. Choose one of ~{~(~A~)~^, ~}."
                             value *lisp-apropos-kinds*)
            :tool-name "lisp.apropos"))))

(-> lisp-apropos--limit-argument (hash-table) (integer 1))
(defun lisp-apropos--limit-argument (arguments)
  "Return the requested match limit from ARGUMENTS, clamped to the supported range."
  (min *lisp-apropos-maximum-limit*
       (max 1 (or (workspace-tool-integer-argument arguments "limit")
                  *lisp-apropos-default-limit*))))

(defmethod tool-execute ((tool lisp-apropos-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List defined active-image names matching the required query."
  (declare (ignore tool))
  (when (resource-context-child-agent-p context)
    (error 'tool-error
           :message "Task child agents cannot inspect the active image."
           :tool-name "lisp.apropos"))
  (let ((query (tool-argument arguments "query" :required t)))
    (unless (non-empty-string-p query)
      (error 'tool-error
             :message "lisp.apropos requires a non-empty query string."
             :tool-name "lisp.apropos"))
    (let* ((package (self-resolve-package (tool-argument arguments "package")))
           (kind (lisp-apropos--kind-argument (tool-argument arguments "kind")))
           (limit (lisp-apropos--limit-argument arguments))
           (matches (lisp-apropos-search query :package package :kind kind)))
      (tool-success
       (lisp-apropos-render matches
                            :query query
                            :package package
                            :limit limit
                            :configuration (tool-context-configuration context))))))


;;;; -- Near-Miss Suggestions --

(-> lisp-apropos--name-tokens (string) list)
(defun lisp-apropos--name-tokens (name)
  "Return the lowercase hyphen-separated tokens of NAME long enough to be telling."
  (let ((tokens '())
        (start 0))
    (loop for index from 0 to (length name)
          when (or (= index (length name))
                   (member (char name index) '(#\- #\: #\. #\/ #\Space)))
            do (when (>= (- index start) *lisp-apropos-token-minimum*)
                 (push (string-downcase (subseq name start index)) tokens))
               (setf start (1+ index)))
    (remove-duplicates (nreverse tokens) :test #'string=)))

(-> lisp-apropos--suggestion-score (string list string) integer)
(defun lisp-apropos--suggestion-score (candidate tokens guess)
  "Return how strongly lowercase CANDIDATE resembles GUESS through its TOKENS."
  (+ (count-if (lambda (token) (search token candidate)) tokens)
     (if (or (search guess candidate) (search candidate guess))
         1
         0)))

(-> self-symbol-suggestions (string &key (:package package) (:limit (integer 1))) list)
(defun self-symbol-suggestions
    (name &key (package (find-package '#:autolith)) (limit *lisp-apropos-suggestion-limit*))
  "Return up to LIMIT defined symbols of PACKAGE whose names resemble the guessed NAME.

Candidates share at least one hyphen-separated token with NAME or contain
it; stronger overlaps rank first and shorter names break ties."
  (let* ((guess (string-downcase (string-trim "'#:" name)))
         (tokens (lisp-apropos--name-tokens guess))
         (scored '()))
    (dolist (symbol (lisp-apropos--present-symbols package))
      (let* ((candidate (string-downcase (symbol-name symbol)))
             (score (lisp-apropos--suggestion-score candidate tokens guess)))
        (when (and (plusp score)
                   (string/= candidate guess)
                   (self-symbol-kinds symbol))
          (push (cons score symbol) scored))))
    (setf scored (sort scored
                       (lambda (left right)
                         (or (> (first left) (first right))
                             (and (= (first left) (first right))
                                  (< (length (symbol-name (rest left)))
                                     (length (symbol-name (rest right)))))))))
    (loop for (nil . symbol) in scored
          repeat limit
          collect symbol)))

(-> self-symbol-suggestion-text (string &key (:package package)) string)
(defun self-symbol-suggestion-text (name &key (package (find-package '#:autolith)))
  "Return a sentence naming the closest defined names to NAME, or an empty string."
  (let ((suggestions (self-symbol-suggestions name :package package)))
    (if suggestions
        (format nil " Closest defined names: ~{~A~^, ~}."
                (mapcar (lambda (symbol) (lisp-apropos--symbol-label symbol package))
                        suggestions))
        "")))
