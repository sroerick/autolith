(in-package #:autolith)

;;;; -- Native LSP Configuration Tests --

(defun lsp-configuration-tests--write (configuration contents)
  "Write CONTENTS to CONFIGURATION's native LSP file."
  (let ((pathname (lsp-configuration-path configuration)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                            :if-does-not-exist ':create)
      (write-string contents stream))
    pathname))

(defun lsp-configuration-tests--signals-p (configuration contents)
  "Return true when CONTENTS is rejected as LSP configuration."
  (lsp-configuration-tests--write configuration contents)
  (handler-case
      (progn (lsp-load-configurations configuration) nil)
    (lsp-configuration-error () t)))

(defun test-lsp-configuration ()
  "Test strict native LSP configuration decoding."
  (with-test-configuration (configuration)
    (test-assert (null (lsp-load-configurations configuration))
                 "missing lsp.sexp returns NIL")
    (test-assert (equal (file-namestring (lsp-configuration-path configuration)) "lsp.sexp")
                 "LSP configuration has the expected filename")
    (test-assert (lsp-configuration-tests--signals-p configuration
                  "(:version 2 :servers ())")
                 "unsupported version is rejected")
    (test-assert (lsp-configuration-tests--signals-p configuration
                  "(:version 1 :servers ((:name \"x\" :command \"x\" :language-id \"x\" :bogus t)))")
                 "unknown keys are rejected")
    (lsp-configuration-tests--write
     configuration
     "(:version 1 :servers ((:name \"typescript\" :command \"typescript-language-server\" :arguments (\"--stdio\") :extensions (\".ts\" \".tsx\") :language-id \"typescript\" :root-markers (\"tsconfig.json\") :initialization-options \"{\\\"a\\\":1}\" :settings \"{}\" :timeout-seconds 30 :disabled-p nil)))")
    (let ((server (first (lsp-load-configurations configuration))))
      (test-assert (and (typep server 'lsp-server-configuration)
                        (string= (lsp-server-configuration-name server) "typescript")
                        (equal (lsp-server-configuration-arguments server) '("--stdio"))
                        (equal (lsp-server-configuration-extensions server) '(".ts" ".tsx"))
                        (= (json-get (lsp-server-configuration-initialization-options server) "a") 1))
                   "valid LSP configuration is decoded"))
    (test-assert (lsp-configuration-tests--signals-p configuration
                  "(:version 1 :servers ((:name \"x\" :command \"x\" :language-id \"x\" :timeout-seconds 0)))")
                 "non-positive timeout is rejected")
    (test-assert (lsp-configuration-tests--signals-p configuration
                  "(:version 1 :servers ((:name \"x\" :command \"x\" :language-id \"x\" :settings \"[]\")))")
                 "non-object JSON settings are rejected")))
