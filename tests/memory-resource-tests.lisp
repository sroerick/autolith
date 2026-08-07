(in-package #:autolith)

;;;; -- Memory Resource Test Support --

(-> memory-resource-tests--call
    (tool-registry tool-context string string &rest t)
    tool-result)
(defun memory-resource-tests--call
    (registry context namespace name &rest arguments)
  "Execute NAMESPACE.NAME with ARGUMENTS through REGISTRY and CONTEXT."
  (tool-registry-execute-call
   registry
   (json-object "namespace" namespace
                "name" name
                "arguments" (json-encode (apply #'json-object arguments)))
   context))

(-> memory-resource-tests--field (string string) string)
(defun memory-resource-tests--field (content label)
  "Return the single-line value following LABEL in memory resource CONTENT."
  (let* ((start (search label content))
         (value-start (and start (+ start (length label))))
         (end (and value-start
                   (or (position #\Newline content :start value-start)
                       (length content)))))
    (unless (and value-start end)
      (error "Missing memory resource field ~S in ~S." label content))
    (subseq content value-start end)))

(-> memory-resource-tests--observation
    (conversation non-empty-string)
    memory-observation)
(defun memory-resource-tests--observation (conversation alias)
  "Return CONVERSATION's typed memory observation for ALIAS."
  (let ((state
          (resource-observation-state-find
           (conversation-resource-observations conversation)
           alias
           'memory-observation-state)))
    (unless state
      (error "Missing memory observation state ~A." alias))
    (resource-observation-state-observation state)))


;;;; -- Memory Resource Tests --

(-> test-memory-resources () null)
(defun test-memory-resources ()
  "Test scoped and exact read-only memory resources and compatibility."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (first-workspace (merge-pathnames "memory-first/" root))
         (second-workspace (merge-pathnames "memory-second/" root))
         (configuration nil)
         (second-configuration nil)
         (empty-configuration (test-configuration))
         (empty-root (test-configuration-root empty-configuration))
         (registry (make-default-tool-registry)))
    (ensure-directories-exist (merge-pathnames "marker" first-workspace))
    (ensure-directories-exist (merge-pathnames "marker" second-workspace))
    (setf configuration
          (configuration--clone base-configuration
                                :working-directory first-workspace)
          second-configuration
          (configuration--clone base-configuration
                                :working-directory second-workspace))
    (unwind-protect
         (let* ((first-conversation
                  (conversation-create configuration
                                       :identifier "memory-resource-first"))
                (second-conversation
                  (conversation-create configuration
                                       :identifier "memory-resource-second"))
                (empty-conversation
                  (conversation-create empty-configuration
                                       :identifier "memory-resource-empty"))
                (first-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation first-conversation))
                (second-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation second-conversation))
                (other-workspace-context
                  (make-instance 'tool-context
                                 :configuration second-configuration
                                 :worker nil
                                 :conversation second-conversation))
                (empty-context
                  (make-instance 'tool-context
                                 :configuration empty-configuration
                                 :worker nil
                                 :conversation empty-conversation))
                (child-context
                  (make-instance 'tool-context
                                 :configuration configuration
                                 :worker nil
                                 :conversation first-conversation
                                 :agent (allocate-instance
                                         (find-class 'task-child-agent))))
                (workspace-memory
                  (memory-remember
                   configuration
                   :title "First workspace guidance"
                   :content "Use the first workspace release procedure in full."
                   :tags '("first" "release")
                   :source-conversation "memory-resource-first"))
                (global-memory
                  (memory-remember
                   configuration
                   :title "Global response preference"
                   :content "Keep persistent answers direct and evidence based."
                   :scope ':global
                   :tags '("global" "style")
                   :source-conversation "memory-resource-first"))
                (other-memory
                  (memory-remember
                   second-configuration
                   :title "Second workspace secret"
                   :content "Only the second workspace collection may list this."
                   :tags '("second")
                   :source-conversation "memory-resource-second")))
           (labels ((call (context namespace name &rest arguments)
                      "Execute one memory resource fixture call."
                      (apply #'memory-resource-tests--call
                             registry context namespace name arguments))

                    (read-resource (context uri &rest arguments)
                      "Read URI under CONTEXT with optional ARGUMENTS."
                      (apply #'call context "resource" "read"
                             "uri" uri arguments))

                    (revision (result)
                      "Return RESULT's model-visible resource revision alias."
                      (memory-resource-tests--field
                       (tool-result-content result) "Revision: ")))
             (let ((resolver
                     (gethash "memory"
                              (resource-registry-resolvers
                               (tool-registry-resource-registry registry)))))
               (test-assert (typep resolver 'memory-resolver)
                            "default tools register the memory resource resolver"))
             (let* ((relevant (read-resource first-context "memory:relevant"))
                    (workspace (read-resource first-context "memory:workspace"))
                    (global (read-resource first-context "memory:global"))
                    (relevant-content (tool-result-content relevant))
                    (workspace-content (tool-result-content workspace))
                    (global-content (tool-result-content global)))
               (test-assert (and (tool-result-success-p relevant)
                                 (tool-result-success-p workspace)
                                 (tool-result-success-p global))
                            "every reserved memory collection is readable")
               (test-assert
                (and (search (memory-identifier workspace-memory) relevant-content)
                     (search (memory-identifier global-memory) relevant-content)
                     (not (search (memory-identifier other-memory) relevant-content)))
                "memory:relevant contains global and current-workspace memories only")
               (test-assert
                (and (search (memory-identifier workspace-memory) workspace-content)
                     (not (search (memory-identifier global-memory) workspace-content))
                     (not (search (memory-identifier other-memory) workspace-content)))
                "memory:workspace is confined to the current workspace")
               (test-assert
                (and (search (memory-identifier global-memory) global-content)
                     (not (search (memory-identifier workspace-memory) global-content))
                     (not (search (memory-identifier other-memory) global-content)))
                "memory:global contains global memories only")
               (test-assert
                (and (search "created:" relevant-content)
                     (search "updated:" relevant-content)
                     (search "tags:" relevant-content)
                     (search "excerpt:" relevant-content))
                "collection entries include useful metadata and excerpts"))
             (let* ((other-workspace
                      (read-resource other-workspace-context "memory:workspace"))
                    (content (tool-result-content other-workspace)))
               (test-assert
                (and (search (memory-identifier other-memory) content)
                     (not (search (memory-identifier workspace-memory) content)))
                "memory:workspace follows the calling configuration"))
             (let* ((uri (format nil "memory:~A"
                                 (memory-identifier other-memory)))
                    (exact (read-resource first-context uri))
                    (content (tool-result-content exact))
                    (alias (revision exact))
                    (observation
                      (memory-resource-tests--observation first-conversation alias)))
               (test-assert (tool-result-success-p exact)
                            "exact memory reads preserve scope-independent memory.read semantics")
               (test-assert
                (and (search (memory-title other-memory) content)
                     (search (memory-content other-memory) content)
                     (search (memory-workspace other-memory) content)
                     (search "source conversation:" content))
                "exact memory reads return full content and metadata")
               (test-assert (and (typep observation 'memory-observation)
                                 (eq (memory-observation-kind observation) ':item))
                            "exact reads retain typed item observations")
               (let ((edit
                       (call first-context "resource" "edit"
                             "uri" uri
                             "base-revision" alias
                             "operations" (vector (json-object
                                                    "op" "replace-lines"
                                                    "start" 1
                                                    "end" 1
                                                    "content" "mutated")))))
                 (test-assert
                  (and (not (tool-result-success-p edit))
                       (search "does not support :EDIT"
                               (tool-result-content edit))
                       (string= (memory-content
                                 (memory-find configuration
                                              (memory-identifier other-memory)))
                                (memory-content other-memory)))
                  "resource.edit rejects memory mutation without changing content")))
             (let* ((revision-memory
                      (memory-remember
                       configuration
                       :title "Revision fixture"
                       :content "First exact snapshot."
                       :tags '("revision")))
                    (uri (format nil "memory:~A"
                                 (memory-identifier revision-memory)))
                    (before-read (read-resource first-context uri))
                    (before-alias (revision before-read))
                    (before-observation
                      (memory-resource-tests--observation
                       first-conversation before-alias)))
               (memory-remember
                configuration
                :identifier (memory-identifier revision-memory)
                :title "Revision fixture"
                :content "Second exact snapshot."
                :tags '("revision"))
               (let* ((after-read (read-resource first-context uri))
                      (after-alias (revision after-read))
                      (after-observation
                        (memory-resource-tests--observation
                         first-conversation after-alias)))
                 (test-assert
                  (and (not (string= before-alias after-alias))
                       (not (string=
                             (resource-observation-revision before-observation)
                             (resource-observation-revision after-observation))))
                  "exact memory content changes produce a new snapshot identity")))
             (let* ((empty (read-resource empty-context "memory:relevant"))
                    (empty-again (read-resource empty-context "memory:relevant"))
                    (alias (revision empty))
                    (again-alias (revision empty-again))
                    (observation
                      (memory-resource-tests--observation empty-conversation alias)))
               (test-assert (and (tool-result-success-p empty)
                                 (search "count: 0" (tool-result-content empty))
                                 (search "No matching memories."
                                         (tool-result-content empty)))
                            "empty memory collections return a complete readable snapshot")
               (test-assert (string= alias again-alias)
                            "an unchanged empty collection reuses its exact snapshot alias")
               (test-assert
                (equal (memory-observation-snapshot observation)
                       '(:kind :collection :identifier "relevant" :records nil))
                "empty collection revision identity includes exact empty state"))
             (let* ((first-read (read-resource first-context "memory:global"))
                    (second-read (read-resource second-context "memory:global"))
                    (first-alias (revision first-read))
                    (second-alias (revision second-read))
                    (first-observation
                      (memory-resource-tests--observation
                       first-conversation first-alias))
                    (second-observation
                      (memory-resource-tests--observation
                       second-conversation second-alias)))
               (test-assert (not (string= first-alias second-alias))
                            "resource revision aliases are isolated by conversation")
               (test-assert
                (string= (resource-observation-revision first-observation)
                         (resource-observation-revision second-observation))
                "equal collection snapshots have equal content-addressed identity")
               (test-assert
                (null (gethash first-alias
                               (conversation-resource-observations
                                second-conversation)))
                "one conversation cannot resolve another conversation's alias"))
             (let ((line-window
                     (read-resource first-context "memory:relevant"
                                    "start-line" 1)))
               (test-assert
                (and (not (tool-result-success-p line-window))
                     (search "do not accept line windows"
                             (tool-result-content line-window)))
                "memory resources reject workspace line-window arguments"))
             (let* ((forgotten
                      (memory-remember
                       configuration
                       :title "Temporary memory"
                       :content "Forget this exact test fixture."
                       :tags nil))
                    (forgotten-uri
                      (format nil "memory:~A" (memory-identifier forgotten))))
               (memory-forget configuration (memory-identifier forgotten))
               (let ((forgotten-read
                       (read-resource first-context forgotten-uri))
                     (missing-read
                       (read-resource first-context "memory:missing-stable-id")))
                 (test-assert
                  (and (not (tool-result-success-p forgotten-read))
                       (search "does not exist" (tool-result-content forgotten-read)))
                  "forgotten exact memories no longer resolve")
                 (test-assert
                  (and (not (tool-result-success-p missing-read))
                       (search "does not exist" (tool-result-content missing-read)))
                  "missing exact memory identifiers fail clearly")))
             (let ((collection-denied
                     (read-resource child-context "memory:relevant"))
                   (item-denied
                     (read-resource
                      child-context
                      (format nil "memory:~A" (memory-identifier global-memory)))))
               (test-assert
                (and (not (tool-result-success-p collection-denied))
                     (search "unavailable under this authority context"
                             (tool-result-content collection-denied)))
                "memory collections remain denied to task child agents")
               (test-assert
                (and (not (tool-result-success-p item-denied))
                     (search "unavailable under this authority context"
                             (tool-result-content item-denied)))
                "exact memory resources remain denied to task child agents"))
             (let ((sample (merge-pathnames "sample.lisp" first-workspace)))
               (with-open-file (stream sample
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
                 (write-line "(in-package #:autolith)" stream))
               (let* ((workspace-read
                        (read-resource first-context "workspace:sample.lisp"))
                      (agenda-read
                        (read-resource first-context "agenda:current"))
                      (memory-read
                        (read-resource first-context "memory:relevant"))
                      (states
                        (conversation-resource-observations first-conversation)))
                 (test-assert
                  (and (typep (gethash (revision workspace-read) states)
                              'workspace-file-observation-state)
                       (typep (gethash (revision agenda-read) states)
                              'agenda-observation-state)
                       (typep (gethash (revision memory-read) states)
                              'memory-observation-state))
                  "workspace, agenda, and memory observations coexist")))
             (let* ((expiry-conversation
                      (conversation-create configuration
                                           :identifier "memory-resource-expiry"))
                    (expiry-context
                      (make-instance 'tool-context
                                     :configuration configuration
                                     :worker nil
                                     :conversation expiry-conversation))
                    (workspace-read
                      (read-resource expiry-context "workspace:sample.lisp"))
                    (agenda-read
                      (read-resource expiry-context "agenda:current"))
                    (old-read (read-resource expiry-context "memory:global"))
                    (workspace-alias (revision workspace-read))
                    (agenda-alias (revision agenda-read))
                    (old-alias (revision old-read))
                    (states
                      (conversation-resource-observations expiry-conversation)))
               (let ((*memory-resource-maximum-observations* 1))
                 (memory-remember
                  configuration
                  :title "New global snapshot"
                  :content "Change collection identity for retention testing."
                  :scope ':global
                  :tags '("retention"))
                 (read-resource expiry-context "memory:global")
                 (test-assert
                  (null (resource-observation-state-find
                         states old-alias 'memory-observation-state))
                  "memory observation retention expires the oldest memory alias")
                 (test-assert
                  (and (typep (gethash workspace-alias states)
                              'workspace-file-observation-state)
                       (typep (gethash agenda-alias states)
                              'agenda-observation-state))
                  "memory observation expiry preserves workspace and agenda states")))
             (let* ((legacy-list (call first-context "memory" "list"))
                    (legacy-search
                      (call first-context "memory" "search"
                            "query" "response preference"))
                    (legacy-read
                      (call first-context "memory" "read"
                            "id" (memory-identifier global-memory))))
               (test-assert
                (and (tool-result-success-p legacy-list)
                     (search (memory-identifier workspace-memory)
                             (tool-result-content legacy-list)))
                "memory.list remains compatible")
               (test-assert
                (and (tool-result-success-p legacy-search)
                     (search (memory-identifier global-memory)
                             (tool-result-content legacy-search)))
                "memory.search remains compatible")
               (test-assert
                (and (tool-result-success-p legacy-read)
                     (string= (tool-result-content legacy-read)
                              (memory-tool--render-memory global-memory)))
                "memory.read remains compatible"))
             (let* ((resource-read
                      (tool-registry-find registry "resource" "read"))
                    (resource-edit
                      (tool-registry-find registry "resource" "edit"))
                    (uri-property
                      (json-get
                       (json-get (tool-parameters resource-read) "properties")
                       "uri")))
               (test-assert
                (and (search "memory:relevant" (tool-description resource-read))
                     (search "memory:<stable-id>"
                             (json-get uri-property "description")))
                "resource.read schema advertises workspace, agenda, and memory URIs")
               (test-assert
                (search "memory: resources are read-only"
                        (tool-description resource-edit))
                "resource.edit truthfully excludes memory mutation"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree empty-root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)
