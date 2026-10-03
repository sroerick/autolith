(in-package #:autolith)


;;;; -- Static API-Key Provider Definitions --

(defmacro define-static-api-key-provider
    (name &key display-name environment-variable provider-name
          source-class source-path login-hint)
  "Define the common classes and factory for one static API-key provider.

NAME supplies the provider prefix used by the public class and function names.
SOURCE-CLASS and SOURCE-PATH customize the persistent credential source; when
omitted, the shared API-key store is used."
  (let* ((prefix (string-upcase (string name)))
         (name-string (string-downcase (string name)))
         (display-name (or display-name prefix))
         (provider-name (or provider-name name-string))
         (account-label (intern (format nil "*~A-ACCOUNT-LABEL*" prefix)))
         (environment-var (intern (format nil "*~A-ENVIRONMENT-VARIABLE*" prefix)))
         (environment-class (intern (format nil "~A-ENVIRONMENT-CREDENTIAL-SOURCE" prefix)))
         (manager-class (intern (format nil "~A-CREDENTIAL-MANAGER" prefix)))
         (create-function (intern (format nil "~A-CREDENTIAL-MANAGER-CREATE" prefix))))
    `(progn
       (defparameter ,account-label ,name-string
         ,(format nil "The synthetic account identifier pinned for static ~A API keys." display-name))
       (defparameter ,environment-var ,environment-variable
         ,(format nil "The environment variable holding the ~A account API key." display-name))

       (defclass ,environment-class (environment-api-key-credential-source)
         ()
         (:default-initargs
          :environment-variable ,environment-var
          :account-id ,account-label)
         (:documentation
          ,(format nil "A read-only adapter loading the ~A API key from the environment." display-name)))

       (defclass ,manager-class (static-api-key-credential-manager)
         ()
         (:documentation
          ,(format nil "The static API key credential manager behind the ~A provider." display-name)))

       (defmethod credential-manager-provider-label ((manager ,manager-class))
         ,(format nil "Name ~A in user-visible credential failures." display-name)
         (declare (ignore manager))
         ,display-name)

       ,@(when login-hint
           `((defmethod credential-manager-login-hint ((manager ,manager-class))
               ,(format nil "Point ~A credential failures at the ~A login command." display-name display-name)
               (declare (ignore manager))
               ,login-hint)))

       (-> ,create-function (configuration) ,manager-class)
       (defun ,create-function (configuration)
         ,(format nil "Create the ~A credential manager for CONFIGURATION's private paths." display-name)
         (make-instance ',manager-class
                        :primary-source
                        (make-instance ',(or source-class 'api-key-credential-source)
                                       :pathname ,(or source-path
                                                     `(configuration-api-keys-path configuration))
                                       ,@(unless source-class
                                           `(:provider-name ,provider-name)))
                        :bootstrap-source
                        (make-instance ',environment-class))))))
