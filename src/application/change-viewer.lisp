(in-package #:autolith)

;;;; -- Shared Change Viewer --

(defparameter *application-tool-call-lines* 8
  "The maximum tool input lines shown in the terminal transcript.")


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

(-> application--file-content-lines (string) list)
(defun application--file-content-lines (text)
  "Return sanitized logical file TEXT lines, preserving trailing blank rows."
  (loop for line across (workspace-file--split-lines text)
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
                   (application--file-content-lines text)
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

(-> application--change-view-rows
    ((option string) (option string)
     &key (:old-start-line (option integer))
          (:new-start-line (option integer))
          (:path (option string)) (:language (option language))
          (:limit integer))
    list)
(defun application--change-view-rows
    (old-text new-text &key old-start-line new-start-line path language
                            (limit *application-tool-call-lines*))
  "Return one bounded line-numbered view of the change from OLD-TEXT to NEW-TEXT.

A NIL side is an absent document, so creations and removals use this same
viewer. PATH or LANGUAGE enables syntax highlighting when applicable. Exact
line coordinates remain optional rather than being fabricated."
  (let* ((old-source (or old-text ""))
         (new-source (or new-text ""))
         (old-lines
           (coerce (application--file-content-lines old-source) 'vector))
         (new-lines
           (coerce (application--file-content-lines new-source) 'vector))
         (old-highlighted
           (and (or path language)
                (application--syntax-lines
                 old-source
                 :language language
                 :path path
                 :file-content-p t)))
         (new-highlighted
           (and (or path language)
                (application--syntax-lines
                 new-source
                 :language language
                 :path path
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
            :old-start-line old-start-line
            :new-start-line new-start-line)))
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
               :line-number (or (and new-start-line
                                     (+ new-start-line prefix-length -1))
                                (and old-start-line
                                     (+ old-start-line prefix-length -1)))
               :content-spans
               (or (and new-highlighted
                        (aref new-highlighted (1- prefix-length)))
                   (and old-highlighted
                        (aref old-highlighted (1- prefix-length)))))))
           (application--change-block-rows
            removed
            ':removed
            :start-line (and old-start-line
                             (+ old-start-line prefix-length))
            :width width
            :highlighted-lines
            (and old-highlighted
                 (subseq old-highlighted
                         prefix-length
                         (- (length old-lines) suffix-length)))
            :limit limit)
           (application--change-block-rows
            added
            ':added
            :start-line (and new-start-line
                             (+ new-start-line prefix-length))
            :width width
            :highlighted-lines
            (and new-highlighted
                 (subseq new-highlighted
                         prefix-length
                         (- (length new-lines) suffix-length)))
            :limit limit)
           (when (plusp suffix-length)
             (list
              (application--change-line-row
               ':context
               (aref old-lines (- (length old-lines) suffix-length))
               :width width
               :line-number
               (or (and new-start-line
                        (+ new-start-line
                           (- (length new-lines) suffix-length)))
                   (and old-start-line
                        (+ old-start-line
                           (- (length old-lines) suffix-length))))
               :content-spans
               (or (and new-highlighted
                        (aref new-highlighted
                              (- (length new-lines) suffix-length)))
                   (and old-highlighted
                        (aref old-highlighted
                              (- (length old-lines) suffix-length))))))))))))
