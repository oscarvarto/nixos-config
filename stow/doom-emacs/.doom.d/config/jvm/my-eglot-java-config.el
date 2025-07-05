;;; my-eglot-java-config.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package! eglot-java
  :hook (java-mode . eglot-java-mode)
  :config
  (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
  (defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
   "Custom options that will be merged with any default settings."
   (let ((bundles-dir (expand-file-name "config/jvm/bundles" doom-user-dir))
         (format-settings-file (expand-file-name "config/jvm/eclipse-java-google-style.xml" doom-user-dir))
         (jar-files (when (file-directory-p (expand-file-name "config/jvm/bundles" doom-user-dir))
                      (directory-files (expand-file-name "config/jvm/bundles" doom-user-dir) t "\\.jar$"))))
     `(:settings
        (:java
          (:home "/Library/Java/JavaVirtualMachines/corretto-24.0.1.9.1.jdk/Contents/Home")
          ;; (:configuration
          ;;   (:runtimes [(:name 'JAVASE_17
          ;;                :path "/Library/Java/JavaVirtualMachines/corretto-17.0.15.6.1.jdk/Contents/Home"
          ;;                :default nil)
          ;;               (:name 'JAVASE_21
          ;;                 :path "/Library/Java/JavaVirtualMachines/corretto-21.0.1.7.1.jdk/Contents/Home"
          ;;                 :default nil)
          ;;               (:name 'JAVASE_22
          ;;                :path "/Library/Java/JavaVirtualMachines/corretto-24.0.1.9.1.jdk/Contents/Home"
          ;;                :default t)]))
          ;; ,(when (file-exists-p format-settings-file)
          ;;    `(:format (:settings (:url ,format-settings-file
          ;;                               :profile "GoogleStyle"))))
          ;; NOTE: https://github.com/redhat-developer/vscode-java/issues/406#issuecomment-356303715
          ;; > We enabled it by default so that workspace-wide errors can be reported (eg. removing a public method in one class would cause compilation errors in other files consuming that method).
          ;; for large workspaces, it may make sense to be able to disable autobuild if it negatively impacts performance.
          ;;(:autobuild (:enabled t))
          ;; https://github.com/dgileadi/vscode-java-decompiler
          (:contentProvider (:preferred "fernflower")))
       ;; WIP: support non standard LSP `java/classFileContents', `Location' items that have a `jdt://...' uri
       ;; https://github.com/eclipse/eclipse.jdt.ls/issues/1384
       ;; nvim impl demo: https://github.com/mfussenegger/dotfiles/commit/3cddf73cd43120da2655e2df6d79bdfd06697f0e
       ;; lsp-java impl demo: https://github.com/emacs-lsp/lsp-java/blob/master/lsp-java.el
       (:extendedClientCapabilities (:classFileContentsSupport t))))))
       ;; bundles: decompilers, etc.
       ;; https://github.com/dgileadi/dg.jdt.ls.decompiler
       ;;,@(when jar-files
       ;;    `(:bundles ,(apply #'vector jar-files)))))))

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

(provide 'my-eglot-java-config)
