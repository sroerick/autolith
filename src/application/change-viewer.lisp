(in-package #:autolith)

;;;; -- Shared Change Viewer --

(defparameter *application-tool-call-lines* 8
  "The maximum tool input lines shown in the terminal transcript.")

(defparameter *change-viewer-line-limit* 8
  "The default maximum removed or added lines shown by the shared viewer.")


;;; Text and syntax preparation

(-> application--display-lines (string) list)
(defun application--display-lines (text)
  "Return sanitized logical lines from TEXT without trailing blank rows."
  (let ((trimmed (string-right-trim '(#\Newline #\Return) text)))
    (when (plusp (length trimmed))
      (mapcar (lambda (line)
                (sanitize-text (string-right-trim '(#\Return) line)
                               :single-line-p t))
              (uiop:split-string trimmed :separator '(#\Newline))))))

(-> application--syntax-lines
    (string &key (:language (option language)) (:path (option string)))
    (option vector))
(defun application--syntax-lines (text &key language path)
  "Return syntax-highlighted display lines for TEXT, or NIL."
  (let ((lines (application--display-lines text)))
    (when lines
      (let* ((source (format nil "~{~A~^~%~}" lines))
             (highlighted
               (syntax--highlight-lines source
                                        :language language
                                        :pathname path)))
        (and highlighted
             (= (length highlighted) (length lines))
             highlighted)))))


;;; Semantic rows

(-> application--change-line-number-cell ((option integer) integer) string)
(defun application--change-line-number-cell (line-number width)
  "Return LINE-NUMBER right aligned to WIDTH, or an empty cell."
  (if line-number
      (format nil "~V@A" width line-number)
      (make-string width :initial-element #\Space)))

(-> application--change-line-row
    (keyword string &key (:width integer)
                         (:line-number (option integer))
                         (:content-spans (option list)))
    list)
(defun application--change-line-row
    (kind text &key (width 1) line-number content-spans)
  "Return one source, context, removed, or added row with a semantic ruler."
  (let* ((style
           (ecase kind
             (:source ':success)
             (:context ':dim)
             (:removed ':failure)
             (:added ':success)))
         (marker
           (ecase kind
             (:source nil)
             (:context " ")
             (:removed "-")
             (:added "+")))
         (gutter
           (cond
             ((eq kind ':source)
              "│ ")
             (line-number
              (format nil "~A ~A │ "
                      marker
                      (application--change-line-number-cell line-number width)))
             (t
              (format nil "~A │ " marker))))
         (content-style (if (eq kind ':context) ':dim ':code)))
    (cons
     (terminal-span style gutter)
     (or content-spans
         (list (terminal-span content-style text))))))

(-> application--source-preview-rows
    (string &key (:path (option string)) (:language (option language))
                 (:limit integer))
    list)
(defun application--source-preview-rows
    (text &key path language (limit *application-tool-call-lines*))
  "Return bounded syntax-highlighted source TEXT under one green ruler."
  (let ((lines (application--display-lines text)))
    (when lines
      (let* ((line-vector (coerce lines 'vector))
             (highlighted
               (application--syntax-lines text :language language :path path))
             (visible-count (min limit (length line-vector)))
             (omitted (- (length line-vector) visible-count)))
        (append
         (loop for index below visible-count
               collect (application--change-line-row
                        ':source
                        (aref line-vector index)
                        :content-spans (and highlighted
                                            (aref highlighted index))))
         (when (plusp omitted)
           (let ((message (format nil "… +~D more line~:P" omitted)))
             (list
              (application--change-line-row
               ':source
               message
               :content-spans (list (terminal-span ':dim message)))))))))))


;;; Before-and-after viewer

(-> application--sanitize-change-line (string) string)
(defun application--sanitize-change-line (line)
  "Return one terminal-safe display line for the shared change viewer."
  (sanitize-text line :single-line-p t))

(-> change-viewer-render
    (&key (:removed-content (option string))
          (:added-content (option string))
          (:removed-start-line (option integer))
          (:added-start-line (option integer))
          (:source-path (option string))
          (:syntax-language (option language))
          (:syntax-highlight-p boolean)
          (:line-limit (integer 1)))
    list)
(defun change-viewer-render
    (&key removed-content added-content
          removed-start-line added-start-line
          source-path syntax-language
          (syntax-highlight-p
            (not (null (or source-path syntax-language))))
          (line-limit *change-viewer-line-limit*))
  "Render one bounded line-numbered change with optional syntax highlighting.

REMOVED-CONTENT and ADDED-CONTENT are complete before-and-after documents for
one change. A NIL side denotes an absent document, so creation and removal use
the same renderer. SOURCE-PATH is language-classification metadata only and is
never read. SYNTAX-LANGUAGE overrides path inference, while
SYNTAX-HIGHLIGHT-P can explicitly disable highlighting. Exact line coordinates
remain optional rather than being fabricated."
  (render-diff
   :removed-content removed-content
   :added-content added-content
   :removed-start-line removed-start-line
   :added-start-line added-start-line
   :source-path source-path
   :syntax-language syntax-language
   :syntax-highlight-p syntax-highlight-p
   :line-limit line-limit
   :sanitize-line-function #'application--sanitize-change-line
   :span-function #'syntax--terminal-span))
