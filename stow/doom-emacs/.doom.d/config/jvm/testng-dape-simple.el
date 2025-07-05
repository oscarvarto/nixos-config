;;; testng-dape-simple.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package dape
  :config
  ;; Remove or override the problematic jdtls configuration
  ;;(setq dape-configs (assq-delete-all 'jdtls dape-configs))

  ;; Add a simple TestNG configuration that doesn't rely on jdtls main class detection
  (add-to-list 'dape-configs
               '(testng
                 modes (java-mode java-ts-mode)
                 command "java"
                 command-args ("-cp"
                               "/Users/oscarvarto/ir/shakedown-api-test-automation/target/test-classes:/Users/oscarvarto/ir/shakedown-api-test-automation/target/classes:~/.m2/repository/org/testng/testng/7.8.0/testng-7.8.0.jar"
                               "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                               "org.testng.TestNG"
                               "testng.xml")
                 command-cwd "/Users/oscarvarto/ir/shakedown-api-test-automation"
                 host "localhost"
                 port 5005
                 :request "attach"))

  ;; Add a configuration for a specific test class (hardcoded for now)
  (add-to-list 'dape-configs
               '(testng-execute-service-check
                 modes (java-mode java-ts-mode)
                 command "java"
                 command-args ("-cp"
                               "/Users/oscarvarto/ir/shakedown-api-test-automation/target/test-classes:/Users/oscarvarto/ir/shakedown-api-test-automation/target/classes:~/.m2/repository/org/testng/testng/7.8.0/testng-7.8.0.jar"
                               "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                               "org.testng.TestNG"
                               "-testclass"
                               "shakedown.servicecheck.ExecuteServiceCheckP0Test")
                 command-cwd "/Users/oscarvarto/ir/shakedown-api-test-automation"
                 host "localhost"
                 port 5005
                 :args ""
                 :stopOnEntry t
                 :type "java"
                 :request "attach"
                 :console "integratedConsole"
                 :internalConsoleOptions "neverOpen"))


  ;; Key bindings
  (map! :after dape
        :map java-mode-map
        :localleader
        (:prefix ("d" . "debug")
         :desc "Debug TestNG (xml)" "x" (lambda () (interactive) (dape 'testng))
         :desc "Debug TestNG ExecuteServiceCheck" "c" (lambda () (interactive) (dape 'testng-execute-service-check)))))

(provide 'testng-dape-simple)
