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

(-> change-viewer--split-lines (string) vector)
(defun change-viewer--split-lines (content)
  "Return CONTENT's logical lines without their LF or CRLF delimiters."
  (if (zerop (length content))
      #()
      (let ((lines nil)
            (start 0)
            (length (length content)))
        (loop
          for newline = (position #\Newline content :start start)
          for end = (or newline length)
          for logical-end = (if (and (> end start)
                                     (char= (char content (1- end)) #\Return))
                                (1- end)
                                end)
          do (push (subseq content start logical-end) lines)
          if newline
            do (setf start (1+ newline))
          else
            do (return)
          when (= start length)
            do (return))
        (coerce (nreverse lines) 'vector))))

(-> change-viewer--content-lines (string) list)
(defun change-viewer--content-lines (text)
  "Return sanitized logical TEXT lines while preserving trailing blank rows."
  (loop for line across (change-viewer--split-lines text)
        collect (sanitize-text line :single-line-p t)))

(-> application--syntax-lines
    (string &key (:language (option language)) (:path (option string))
                 (:file-content-p boolean))
    (option vector))
(defun application--syntax-lines
    (text &key language path (file-content-p nil))
  "Return syntax-highlighted display lines for TEXT, or NIL.

LANGUAGE handles pathless source such as eval forms. PATH selects a language
from a file destination. FILE-CONTENT-P preserves meaningful trailing blank
file rows. Sanitization occurs before ColorLisp sees the text."
  (let ((lines (if file-content-p
                   (change-viewer--content-lines text)
                   (application--display-lines text))))
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

(-> application--change-line-number-width
    (vector vector &key (:old-start-line (option integer))
                        (:new-start-line (option integer)))
    integer)
(defun application--change-line-number-width
    (old-lines new-lines &key old-start-line new-start-line)
  "Return the display width needed for OLD-LINES and NEW-LINES numbers."
  (let ((largest
          (max (or (and old-start-line
                        (+ old-start-line (max 0 (1- (length old-lines)))))
                   0)
               (or (and new-start-line
                        (+ new-start-line (max 0 (1- (length new-lines)))))
                   0))))
    (max 1 (length (princ-to-string largest)))))

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

(-> application--change-common-prefix-length (vector vector) integer)
(defun application--change-common-prefix-length (old-lines new-lines)
  "Return the number of equal leading lines in OLD-LINES and NEW-LINES."
  (loop for index below (min (length old-lines) (length new-lines))
        while (string= (aref old-lines index) (aref new-lines index))
        count t))

(-> application--change-common-suffix-length (vector vector integer) integer)
(defun application--change-common-suffix-length
    (old-lines new-lines prefix-length)
  "Return equal trailing lines after PREFIX-LENGTH without overlap."
  (let ((maximum (min (- (length old-lines) prefix-length)
                      (- (length new-lines) prefix-length))))
    (loop for offset from 1 to maximum
          while (string= (aref old-lines (- (length old-lines) offset))
                         (aref new-lines (- (length new-lines) offset)))
          count t)))

(-> application--change-block-rows
    (vector keyword &key (:start-line (option integer)) (:width integer)
                         (:highlighted-lines (option vector)) (:limit integer))
    list)
(defun application--change-block-rows
    (lines kind &key start-line (width 1) highlighted-lines
                     (limit *application-tool-call-lines*))
  "Return bounded numbered changed LINES of KIND."
  (let* ((visible-count (min limit (length lines)))
         (omitted (- (length lines) visible-count))
         (noun (ecase kind
                 (:removed "removed")
                 (:added "added"))))
    (append
     (loop for index below visible-count
           for line-number = (and start-line (+ start-line index))
           collect (application--change-line-row
                    kind
                    (aref lines index)
                    :width width
                    :line-number line-number
                    :content-spans (and highlighted-lines
                                        (aref highlighted-lines index))))
     (when (plusp omitted)
       (let* ((line-number (and start-line (+ start-line visible-count)))
              (message (format nil "… +~D more ~A line~:P" omitted noun)))
         (list
          (application--change-line-row
           kind
           message
           :width width
           :line-number line-number
           :content-spans (list (terminal-span ':dim message)))))))))

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
  (let* ((old-source (or removed-content ""))
         (new-source (or added-content ""))
         (old-lines
           (coerce (change-viewer--content-lines old-source) 'vector))
         (new-lines
           (coerce (change-viewer--content-lines new-source) 'vector))
         (old-highlighted
           (and syntax-highlight-p
                (application--syntax-lines
                 old-source
                 :language syntax-language
                 :path source-path
                 :file-content-p t)))
         (new-highlighted
           (and syntax-highlight-p
                (application--syntax-lines
                 new-source
                 :language syntax-language
                 :path source-path
                 :file-content-p t)))
         (prefix-length
           (application--change-common-prefix-length old-lines new-lines))
         (suffix-length
           (application--change-common-suffix-length old-lines
                                                     new-lines
                                                     prefix-length))
         (width
           (application--change-line-number-width
            old-lines
            new-lines
            :old-start-line removed-start-line
            :new-start-line added-start-line)))
    (if (and (= prefix-length (length old-lines))
             (= prefix-length (length new-lines)))
        (list (list (terminal-span ':dim "no textual change")))
        (let ((removed (subseq old-lines
                               prefix-length
                               (- (length old-lines) suffix-length)))
              (added (subseq new-lines
                             prefix-length
                             (- (length new-lines) suffix-length))))
          (append
           (when (plusp prefix-length)
             (list
              (application--change-line-row
               ':context
               (aref old-lines (1- prefix-length))
               :width width
               :line-number (or (and added-start-line
                                     (+ added-start-line prefix-length -1))
                                (and removed-start-line
                                     (+ removed-start-line prefix-length -1)))
               :content-spans
               (or (and new-highlighted
                        (aref new-highlighted (1- prefix-length)))
                   (and old-highlighted
                        (aref old-highlighted (1- prefix-length)))))))
           (application--change-block-rows
            removed
            ':removed
            :start-line (and removed-start-line
                             (+ removed-start-line prefix-length))
            :width width
            :highlighted-lines
            (and old-highlighted
                 (subseq old-highlighted
                         prefix-length
                         (- (length old-lines) suffix-length)))
            :limit line-limit)
           (application--change-block-rows
            added
            ':added
            :start-line (and added-start-line
                             (+ added-start-line prefix-length))
            :width width
            :highlighted-lines
            (and new-highlighted
                 (subseq new-highlighted
                         prefix-length
                         (- (length new-lines) suffix-length)))
            :limit line-limit)
           (when (plusp suffix-length)
             (list
              (application--change-line-row
               ':context
               (aref old-lines (- (length old-lines) suffix-length))
               :width width
               :line-number
               (or (and added-start-line
                        (+ added-start-line
                           (- (length new-lines) suffix-length)))
                   (and removed-start-line
                        (+ removed-start-line
                           (- (length old-lines) suffix-length))))
               :content-spans
               (or (and new-highlighted
                        (aref new-highlighted
                              (- (length new-lines) suffix-length)))
                   (and old-highlighted
                        (aref old-highlighted
                              (- (length old-lines) suffix-length))))))))))))
