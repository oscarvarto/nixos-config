;;; enhanced-dape-testng-config.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package dape
  :config

  ;; Helper function to find project root reliably
  (defun dape-testng-project-root
   ()
   "Find the project root directory reliably."
   (or (when (fboundp 'projectile-project-root)
         (projectile-project-root))
       (when (fboundp 'project-root)
         (when -let ((project (project-current)))
           (project-root project)))
       (vc-root-dir)
       (locate-dominating-file default-directory "pom.xml")
       (locate-dominating-file default-directory "build.gradle")
       (locate-dominating-file default-directory ".git")
       default-directory))

  ;; Helper function to build classpath for TestNG debugging
  (defun dape-testng-build-classpath
   ()
   "Build classpath for TestNG debugging from project structure."
   (let * ((project-root (dape-testng-project-root))
           (gradle-build (file-exists-p (expand-file-name "build.gradle" project-root)))
           (maven-build (file-exists-p (expand-file-name "pom.xml" project-root)))
           (classpath-parts '()))

     (cond
      ;; Gradle project
      (gradle-build
       (let ((gradle-cp-cmd (format "cd %s && ./gradlew testClasspath --quiet 2>/dev/null || gradle testClasspath --quiet 2>/dev/null"
                                    (shell-quote-argument project-root))))
         (cond ition-case nil
             (let ((gradle-cp (string-trim (shell-command-to-string gradle-cp-cmd))))
               (when (and gradle-cp (not (string-empty-p gradle-cp)))
                 (push gradle-cp classpath-parts)))
           (error nil))))

      ;; Maven project
      (maven-build
       (let ((maven-cp-cmd (format "cd %s && mvn dependency:build-classpath -DincludeScope=test -Dmdep.outputFile=/dev/stdout --quiet 2>/dev/null"
                                   (shell-quote-argument project-root))))
         (cond ition-case nil
             (let ((maven-cp (string-trim (shell-command-to-string maven-cp-cmd))))
               (when (and maven-cp (not (string-empty-p maven-cp)))
                 (push maven-cp classpath-parts)))
           (error nil)))))

     ;; Add common directories
     (dolist (dir '("target/classes" "target/test-classes"
                    "build/classes/java/main" "build/classes/java/test"
                    "out/production" "out/test"
                    "src/main/java" "src/test/java"))
       (let ((full-path (expand-file-name dir project-root)))
         (when (file-directory-p full-path)
           (push full-path classpath-parts))))

     ;; Join all classpath parts
     (if classpath-parts
         (string-join (reverse classpath-parts) ":")
       ".")))

  ;; Helper function to find TestNG configuration
  (defun dape-testng-find-config
   ()
   "Find TestNG configuration file in project."
   (let * ((project-root (dape-testng-project-root))
           (possible-configs '("testng.xml" "src/test/resources/testng.xml")
                              "test/testng.xml" "tests/testng.xml"))
     (cl-loop for config in possible-configs
              for full-path = (expand-file-name config project-root)
              when (file-exists-p full-path)
              return full-path)))

  ;; Helper function to get current test class
  (defun dape-testng-current-test-class
   ()
   "Get the current test class name."
   (when (and buffer-file-name
              (string-match-p "\\.java$" buffer-file-name))
     (let * ((project-root (dape-testng-project-root))
             (file-path (file-relative-name buffer-file-name project-root))
             (class-path (replace-regexp-in-string "^src/test/java/" "" file-path))
             (class-path (replace-regexp-in-string "^test/" "" class-path))
             (class-path (replace-regexp-in-string "\\.java$" "" class-path)))
       (replace-regexp-in-string "/" "." class-path))))

  ;; Enhanced TestNG debug configuration
  (add-to-list 'dape-configs
    `(testng-debug-enhanced
      modes (java-mode java-ts-mode)
      command "java"
      command-args (lambda ()
                     (let * ((project-root (dape-testng-project-root))
                             (classpath (dape-testng-build-classpath))
                             (config-file (dape-testng-find-config))
                             (test-class (dape-testng-current-test-class))
                             (testng-args '()))

                       ;; Build TestNG arguments
                       (cond
                        ;; Use config file if found
                        (config-file
                         (push config-file testng-args))
                        ;; Use current test class if available
                        (test-class
                         (push "-testclass" testng-args)
                         (push test-class testng-args))
                        ;; Default to testng.xml
                        (t
                         (push "testng.xml" testng-args)))

                       ;; Return command args vector
                       (apply #'vector
                              "-cp" classpath
                              "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                              "org.testng.TestNG"
                              (reverse testng-args))))
      command-cwd (lambda () (dape-testng-project-root))
      host "localhost"
      port 5005))

  ;; Simple TestNG debug configuration (fallback)
  (add-to-list 'dape-configs
    `(testng-debug-simple
      modes (java-mode java-ts-mode)
      command "java"
      command-args (lambda ()
                     (let * ((project-root (dape-testng-project-root))
                             (test-class (dape-testng-current-test-class))
                             (basic-cp (string-join)
                                       (list (expand-file-name "target/classes" project-root)
                                             (expand-file-name "target/test-classes" project-root)
                                             (expand-file-name "build/classes/java/main" project-root)
                                             (expand-file-name "build/classes/java/test" project-root))
                                       ":")
                             (testng-jar "~/.m2/repository/org/testng/testng/7.8.0/testng-7.8.0.jar")
                             (full-cp (format "%s:%s" basic-cp testng-jar)))
                       (if test-class
                           (vector "-cp" full-cp
                                   "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                                   "org.testng.TestNG"
                                   "-testclass" test-class)
                         (vector "-cp" full-cp
                                 "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                                 "org.testng.TestNG"
                                 (or (dape-testng-find-config) "testng.xml")))))
      command-cwd (lambda () (dape-testng-project-root))
      host "localhost"
      port 5005))

  ;; TestNG debug with Maven classpath
  (add-to-list 'dape-configs
    `(testng-debug-maven
      modes (java-mode java-ts-mode)
      command "java"
      command-args (lambda ()
                     (let * ((project-root (dape-testng-project-root))
                             (maven-cp-cmd (format "cd %s && mvn dependency:build-classpath -DincludeScope=test -Dmdep.outputFile=/dev/stdout --quiet 2>/dev/null"
                                                   (shell-quote-argument project-root)))
                             (maven-cp (cond ition-case nil
                                           (string-trim (shell-command-to-string maven-cp-cmd))
                                         (error "")))
                             (test-class (dape-testng-current-test-class))
                             (classpath (if (and maven-cp (not (string-empty-p maven-cp)))
                                            (concat maven-cp ":"
                                                    (expand-file-name "target/classes" project-root) ":"
                                                    (expand-file-name "target/test-classes" project-root))
                                          (concat (expand-file-name "target/classes" project-root) ":"
                                                  (expand-file-name "target/test-classes" project-root)))))
                       (if test-class
                           (vector "-cp" classpath
                                   "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                                   "org.testng.TestNG"
                                   "-testclass" test-class)
                         (vector "-cp" classpath
                                 "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                                 "org.testng.TestNG"
                                 (or (dape-testng-find-config) "testng.xml")))))
      command-cwd (lambda () (dape-testng-project-root))
      host "localhost"
      port 5005))

  ;; TestNG debug with Gradle classpath
  (add-to-list 'dape-configs
    `(testng-debug-gradle
      modes (java-mode java-ts-mode)
      command "java"
      command-args (lambda ()
                     (let * ((project-root (dape-testng-project-root))
                             (gradle-cp-cmd (format "cd %s && ./gradlew testClasspath --quiet 2>/dev/null || gradle testClasspath --quiet 2>/dev/null"
                                                    (shell-quote-argument project-root)))
                             (gradle-cp (cond ition-case nil
                                            (string-trim (shell-command-to-string gradle-cp-cmd))
                                          (error "")))
                             (test-class (dape-testng-current-test-class))
                             (classpath (if (and gradle-cp (not (string-empty-p gradle-cp)))
                                            gradle-cp
                                          (concat (expand-file-name "build/classes/java/main" project-root) ":"
                                                  (expand-file-name "build/classes/java/test" project-root)))))
                       (if test-class
                           (vector "-cp" classpath
                                   "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                                   "org.testng.TestNG"
                                   "-testclass" test-class)
                         (vector "-cp" classpath
                                 "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                                 "org.testng.TestNG"
                                 (or (dape-testng-find-config) "testng.xml")))))
      command-cwd (lambda () (dape-testng-project-root))
      host "localhost"
      port 5005)))

;; Keybindings for easy access
(map! :after dape
      :map java-mode-map
      :localleader
      (:prefix ("d" . "debug")
       :desc "Debug TestNG (enhanced)" "t" (lambda () (interactive) (dape 'testng-debug-enhanced))
       :desc "Debug TestNG (Maven)" "m" (lambda () (interactive) (dape 'testng-debug-maven))
       :desc "Debug TestNG (Gradle)" "g" (lambda () (interactive) (dape 'testng-debug-gradle))
       :desc "Debug TestNG (simple)" "s" (lambda () (interactive) (dape 'testng-debug-simple))))

(provide 'enhanced-dape-testng-config)
