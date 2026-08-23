(in-package #:autolith)

;;;; -- Recursive Inference Context Objects --

(-> rlm-object-root (configuration) pathname)
(defun rlm-object-root (configuration)
  "Return the content-addressed inference object directory."
  (merge-pathnames "inferences/objects/"
                   (configuration-data-root configuration)))

(-> rlm-context--store (configuration) cl-llm-provider-api:rlm-context-store)
(defun rlm-context--store (configuration)
  "Return the provider API context store for CONFIGURATION."
  (cl-llm-provider-api:rlm-context-store-create
   (rlm-object-root configuration)))

(-> rlm-context-intern
    (configuration string &key (:label (option string)))
    rlm-context-object)
(defun rlm-context-intern (configuration content &key label)
  "Intern CONTENT in CONFIGURATION's provider API context store."
  (cl-llm-provider-api:rlm-context-intern
   (rlm-context--store configuration)
   content
   :label label))

(-> rlm-context-intern-pathname
    (configuration pathname &key (:label (option string)))
    rlm-context-object)
(defun rlm-context-intern-pathname (configuration pathname &key label)
  "Intern PATHNAME in CONFIGURATION's provider API context store."
  (cl-llm-provider-api:rlm-context-intern-pathname
   (rlm-context--store configuration)
   pathname
   :label label))

(-> rlm-context-object-find
    (configuration string)
    (values (option rlm-context-object) (option string)))
(defun rlm-context-object-find (configuration digest)
  "Return CONFIGURATION's verified context object for DIGEST and its content."
  (cl-llm-provider-api:rlm-context-object-find
   (rlm-context--store configuration)
   digest))

(-> rlm-context-designator-object (configuration t) rlm-context-object)
(defun rlm-context-designator-object (configuration designator)
  "Resolve DESIGNATOR in CONFIGURATION's provider API context store."
  (cl-llm-provider-api:rlm-context-designator-object
   (rlm-context--store configuration)
   designator))
