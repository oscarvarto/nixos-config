;;; testng-dape-template.el --- TestNG Debug Adapter Protocol Configuration Template -*- lexical-binding: t; no-byte-compile: t; -*-

;; NOTE: On macOS, you may need to allow network connections for Java processes
;; in System Preferences > Security & Privacy > Firewall if you encounter connection issues.

(defcustom testng-dape-port 5005
  "Default port for TestNG debugging sessions."
  :type 'integer
  :group 'dape)

(defcustom testng-dape-suspend t
  "Whether to suspend TestNG execution on startup."
  :type 'boolean
  :group 'dape)

(defun testng-dape-get-project-root ()
  "Get the project root directory for TestNG configuration."
  (or (locate-dominating-file default-directory "pom.xml")
      (locate-dominating-file default-directory "build.gradle")
      (locate-dominating-file default-directory "build.gradle.kts")
      (dape-cwd)))

(defun testng-dape-get-project-name ()
  "Get the project name for TestNG configuration."
  (file-name-nondirectory
   (directory-file-name (testng-dape-get-project-root))))

(defun testng-dape-resolve-classpath (config)
  "Resolve classpath for TestNG using eglot if available."
  (if-let* ((server (and (featurep 'eglot) (eglot-current-server)))
            (project-name (testng-dape-get-project-name)))
      (condition-case err
          (pcase-let ((`[,_module-paths ,class-paths]
                       (eglot-execute-command server
                                              "vscode.java.resolveClasspath"
                                              (vector "org.testng.TestNG" project-name))))
            (vconcat class-paths))
        (error
         (message "Failed to resolve classpath via eglot: %s" (error-message-string err))
         nil))
    nil))

(defun testng-dape-build-classpath (config)
  "Build classpath for TestNG configuration."
  (let ((resolved-cp (testng-dape-resolve-classpath config))
        (project-root (testng-dape-get-project-root)))
    (if resolved-cp
        (string-join resolved-cp ":")
      ;; Fallback to Maven/Gradle standard paths
      (string-join
       (list
        (expand-file-name "target/test-classes" project-root)
        (expand-file-name "target/classes" project-root)
        (expand-file-name "build/classes/java/test" project-root)
        (expand-file-name "build/classes/java/main" project-root)
        ;; Add TestNG jar - you may need to adjust this path
        (expand-file-name "~/.m2/repository/org/testng/testng/7.8.0/testng-7.8.0.jar"))
       ":"))))

(defun testng-dape-get-test-class ()
  "Get the test class name from current buffer."
  (when (buffer-file-name)
    (let* ((file-name (file-name-nondirectory (buffer-file-name)))
           (class-name (file-name-sans-extension file-name)))
      (when (string-suffix-p "Test" class-name)
        class-name))))

(defun testng-dape-get-testng-xml ()
  "Find testng.xml file in project."
  (let ((project-root (testng-dape-get-project-root)))
    (or (and (file-exists-p (expand-file-name "testng.xml" project-root))
             (expand-file-name "testng.xml" project-root))
        (and (file-exists-p (expand-file-name "src/test/resources/testng.xml" project-root))
             (expand-file-name "src/test/resources/testng.xml" project-root)))))

;; TestNG configuration template
(defvar testng-dape-config-template
  '(testng-template
    modes (java-mode java-ts-mode)
    ensure (lambda (config)
             (let ((file (or (buffer-file-name) (dape-buffer-default))))
               (unless (and (stringp file) (file-exists-p file))
                 (user-error "Unable to locate file %s" file))
               (with-current-buffer (find-file-noselect file)
                 (unless (and (featurep 'eglot) (eglot-current-server))
                   (user-error "No eglot instance active in buffer %s" (current-buffer)))
                 (unless (seq-contains-p (eglot--server-capable :executeCommandProvider :commands)
                                        "vscode.java.startDebugSession")
                   (user-error "Jdtls instance does not bundle java-debug-server, please install")))))
    fn (lambda (config)
         (let* ((file (or (buffer-file-name) (dape-buffer-default)))
                (project-root (testng-dape-get-project-root))
                (testng-xml (testng-dape-get-testng-xml))
                (test-class (testng-dape-get-test-class)))
           (with-current-buffer (find-file-noselect file)
             (if-let* ((server (eglot-current-server)))
                 (pcase-let ((`[,module-paths ,class-paths]
                              (eglot-execute-command server
                                                     "vscode.java.resolveClasspath"
                                                     (vector "org.testng.TestNG"
                                                             (file-name-nondirectory
                                                              (directory-file-name project-root)))))
                             (port (eglot-execute-command server
                                                          "vscode.java.startDebugSession" nil)))
                   (message "TestNG Debug: Started debug session on port %s" port)
                   (message "TestNG Debug: Project root: %s" project-root)
                   (let ((final-config (thread-first config
                                                      (plist-put :port port)
                                                      (plist-put :host "localhost")
                                                      (plist-put :modulePaths module-paths)
                                                      (plist-put :classPaths class-paths)
                                                      (plist-put :mainClass "org.testng.TestNG")
                                                      (plist-put :vmArgs "-Xmx1024m -XX:+ShowCodeDetailsInExceptionMessages")
                                                      (plist-put :args
                                                                 (let ((base-args (cond
                                                                                  ;; If we have a testng.xml file, use it
                                                                                  (testng-xml testng-xml)
                                                                                  ;; If we're in a test class, run that specific class
                                                                                  (test-class (format "-testclass %s" test-class))
                                                                                  ;; Default fallback
                                                                                  (t "testng.xml"))))
                                                                   ;; Add debug-friendly arguments
                                                                   (if (stringp base-args)
                                                                       (format "%s -verbose:2" base-args)
                                                                     (format "%s -verbose:2" (or base-args ""))))))))
                     (message "TestNG Debug: Final config - host: %s, port: %s" 
                              (plist-get final-config 'host)
                              (plist-get final-config 'port))
                     final-config)
               (user-error "No eglot server available"))))))
    host "localhost"
    :type "java"
    :request "attach"
    :stopOnEntry t
    :console "integratedConsole"
    :internalConsoleOptions "neverOpen"))

;; Usage example:
;; (add-to-list 'dape-configs testng-dape-config-template)

(provide 'testng-dape-template)
