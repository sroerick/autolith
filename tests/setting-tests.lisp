(in-package #:autolith)

;;;; -- Setting Protocol Tests --

(-> setting-tests--rejects-p (setting t) boolean)
(defun setting-tests--rejects-p (setting text)
  "Return true when parsing TEXT for SETTING signals a configuration error."
  (handler-case
      (progn
        (setting-parse setting text nil)
        nil)
    (configuration-error ()
      t)))

(-> test-setting-kinds () null)
(defun test-setting-kinds ()
  "Test coercion, validation, options, and rendering of each setting kind."
  (let ((boolean (make-instance 'boolean-setting
                                :name :boolean-case
                                :label "Boolean case"
                                :documentation "A boolean."))
        (choice (make-instance 'choice-setting
                               :name :choice-case
                               :label "Choice case"
                               :documentation "A keyword choice."
                               :type 'keyword
                               :options '(:ask :auto)))
        (computed (make-instance 'choice-setting
                                 :name :computed-case
                                 :label "Computed case"
                                 :documentation "A string choice from a function."
                                 :type 'string
                                 :options (lambda (configuration)
                                            (list "low" configuration))))
        (bounded (make-instance 'integer-setting
                                :name :bounded-case
                                :label "Bounded case"
                                :documentation "A bounded integer."
                                :minimum 1
                                :maximum 95))
        (path (make-instance 'pathname-setting
                             :name :path-case
                             :label "Path case"
                             :documentation "A pathname.")))
    (dolist (case '(("on" t) ("TRUE" t) ("yes" t) ("1" t)
                    ("off" nil) ("false" nil) ("" nil) ("0" nil)))
      (destructuring-bind (text expected) case
        (test-assert (eq (setting-parse boolean text nil) expected)
                     (format nil "~S parses as ~A" text (if expected "on" "off")))))
    (test-assert (setting-tests--rejects-p boolean "maybe")
                 "a boolean rejects text that is neither on nor off")
    (test-assert (and (eq (setting-parse boolean t nil) t)
                      (null (setting-parse boolean nil nil)))
                 "a boolean stores Lisp booleans as given")
    (test-assert (equal (setting-options boolean nil) '(t nil))
                 "a boolean offers on and off")
    (test-assert (and (string= (setting-render-value boolean t) "on")
                      (string= (setting-render-value boolean nil) "off"))
                 "a boolean renders as on or off")
    (test-assert (eq (setting-parse choice "Auto" nil) ':auto)
                 "a keyword choice matches option names case-insensitively")
    (test-assert (setting-tests--rejects-p choice "sandbox")
                 "a choice rejects values outside its options")
    (test-assert (string= (setting-render-value choice ':ask) "ask")
                 "a keyword choice renders in lower case")
    (test-assert (equal (setting-options computed "high") '("low" "high"))
                 "function options receive the configuration")
    (test-assert (string= (setting-parse computed "high" "high") "high")
                 "a string choice accepts a computed option")
    (test-assert (setting-tests--rejects-p computed "medium")
                 "a string choice rejects a value the function does not offer")
    (test-assert (= (setting-parse bounded " 80 " nil) 80)
                 "an integer parses trimmed decimal text")
    (dolist (text '("0" "96" "abc" "1.5"))
      (test-assert (setting-tests--rejects-p bounded text)
                   (format nil "a bounded integer rejects ~S" text)))
    (test-assert (equal (setting-parse path "state/prefs.sexp" nil)
                        #p"state/prefs.sexp")
                 "a pathname parses namestrings")
    (test-assert (string= (setting-render-value path #p"a/b") "a/b")
                 "a pathname renders as its namestring")
    (test-assert (string= (setting-render-value path nil) "unset")
                 "an absent value renders as unset"))
  nil)

(-> test-setting-registry () null)
(defun test-setting-registry ()
  "Test definition order, replacement, defaults, and unknown-name failures."
  (let ((*settings* (make-ordered-map :test #'eq)))
    (define-setting :registry-first (boolean-setting)
      :label "First"
      :documentation "The first registered setting."
      :default t)
    (define-setting :registry-second (integer-setting)
      :label "Second"
      :documentation "The second registered setting."
      :default (lambda (configuration) (* 2 configuration)))
    (test-assert (equal (mapcar #'setting-name (settings-list)) '(:registry-first :registry-second))
                 "settings list in definition order")
    (define-setting :registry-first (boolean-setting)
      :label "First again"
      :documentation "A replacement keeps its position."
      :default nil)
    (test-assert (and (equal (mapcar #'setting-name (settings-list))
                             '(:registry-first :registry-second))
                      (string= (setting-label (find-setting :registry-first)) "First again"))
                 "redefining a setting replaces it in place")
    (test-assert (= (setting-default-value (find-setting :registry-second) 21) 42)
                 "function defaults receive the configuration")
    (test-assert (null (setting-default-value (find-setting :registry-first) nil))
                 "plain defaults are returned as given")
    (test-assert (handler-case (progn (find-setting :registry-missing) nil)
                   (configuration-error () t))
                 "an unknown setting name signals a configuration error"))
  nil)
