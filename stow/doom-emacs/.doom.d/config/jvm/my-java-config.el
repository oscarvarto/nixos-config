;;; my-java-config.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package! eglot-java
  :hook ((java-mode java-ts-mode). eglot-mode)
  :hook ((java-mode java-ts-mode). eglot-java-mode)
  :config
  (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
  (defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
   "Custom options that will be merged with any default settings."
   (let* ((format-settings-file (expand-file-name "config/jvm/eclipse-java-google-style.xml" doom-user-dir))
          (bundles-dir (expand-file-name "config/jvm/bundles" doom-user-dir))
          (jar-files (when (file-directory-p bundles-dir)
                       (directory-files (expand-file-name "config/jvm/bundles" doom-user-dir) t "\\.jar$"))))
     `(,@(when jar-files
          `(:bundles ,(apply #'vector jar-files)))
       :settings
        (:java
          (:autobuild (:enabled t))
          :configuration
          (:runtimes [(:name "JavaSE-17"
                       :path "/Library/Java/JavaVirtualMachines/corretto-17.0.15.6.1.jdk/Contents/Home"
                       :default nil)
                      (:name "JavaSE-21"
                        :path "/Library/Java/JavaVirtualMachines/corretto-21.0.1.7.1.jdk/Contents/Home"
                        :default nil)
                      (:name "JavaSE-22"
                       :path "/Library/Java/JavaVirtualMachines/oracle-graalvm-24.0.1.jdk/Contents/Home"
                       :default t)])
          :contentProvider (:preferred "fernflower")
          ,@(when (file-exists-p format-settings-file)
             `(:format (:settings (:url ,format-settings-file
                                        :profile "GoogleStyle"))))
          :home "/Library/Java/JavaVirtualMachines/oracle-graalvm-24.0.1.jdk/Contents/Home"
          :implementationsCodeLens (:enabled t)
          :jdt (:ls (:lombokSupport (:enabled t))))
       :extendedClientCapabilities (:classFileContentsSupport t)))))

(use-package! eglot-java-lombok
  :config
  (eglot-java-lombok/init))

(map! :after eglot-java
      :map eglot-java-mode-map
      :localleader
      (:prefix ("j" . "java")
       :desc "Java new file" "n" #'eglot-java-file-new
       :desc "Java run main" "x" #'eglot-java-run-main
       :desc "Java run tests" "t" #'eglot-java-run-test
       :desc "Java project new" "P" #'eglot-java-project-new
       :desc "Java project build" "T" #'eglot-java-project-build-task
       :desc "Java project refresh" "R" #'eglot-java-project-build-refresh))

;;(load! "enhanced-dape-testng-config")
;;(load! "simple-testng-dape")
;;(load! "dape-jdtls")
;;(load! "dape-extras")
(load! "testng-dape-template")
(require 'dape)
(after! dape
  (add-to-list 'dape-configs testng-dape-config-template))

;;(use-package dape-jdtls
;; :load-path (expand-file-name "config/jvm" doom-user-dir)
;;:after dape
               ;;:custom
;;(dape-jdtls-java-debug-plugin-jar "/PATH/TO/java-debug/com.microsoft.java.debug.plugin/target/com.microsoft.java.debug.plugin-VERSION.jar")
;;(dape-jdtls-java-test-plugin-jar "/PATH/TO/vscode-java-test/java-extension/com.microsoft.java.test.plugin/target/com.microsoft.java.test.plugin-VERSION.jar")
               ;;:config
;;(add-to-list 'dape-configs
;;             `(jdtls
;;               modes (java-mode java-ts-mode)
;;               fn dape-jdtls-complete-config
;;               :args ""
;;               :stopOnEntry t)))
               ;;(require 'eglot))
;;(add-to-list 'eglot-server-programs
;;             `((java-mode java-ts-mode) .
;;               ("jdtls"
;;                :initializationOptions
;;                (:bundles [,dape-jdtls-java-debug-plugin-jar
;;                           ,dape-jdtls-java-test-plugin-jar])))))

(provide 'my-java-config)
