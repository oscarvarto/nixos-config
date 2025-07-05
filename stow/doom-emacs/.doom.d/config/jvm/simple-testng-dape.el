;;; simple-testng-dape.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package dape
  :config
  ;; Simple TestNG debug configuration
  (add-to-list 'dape-configs
               `(testng-simple
                 modes (java-mode java-ts-mode)
                 command "java"
                 command-args ["-cp" "/Users/oscarvarto/ir/shakedown-api-test-automation/target/classes:/Users/oscarvarto/ir/shakedown-api-test-automation/target/test-classes:~/.m2/repository/org/testng/testng/7.8.0/testng-7.8.0.jar"
                               "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"
                               "org.testng.TestNG"
                               "-testclass"
                               "shakedown.servicecheck.ExecuteServiceCheckP0Test"]
                 command-cwd "/Users/oscarvarto/ir/shakedown-api-test-automation"
                 host "localhost"
                 port 5005))

  ;; Key binding for easy access
  (map! :after dape
        :map java-mode-map
        :localleader
        (:prefix ("d" . "debug")
         :desc "Debug TestNG simple" "t" (lambda () (interactive) (dape 'testng-simple)))))

(provide 'simple-testng-dape)
