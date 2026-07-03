(require 'ob-http)

(ert-deftest ob-http-test-construct-url ()
  (let ((params '((:username . "demo")
                  (:password . "secret")
                  (:host . "localhost"))))
    (should (equal (ob-http-construct-url "/" params)
                   "http://demo:secret@localhost/")))

  (let ((params '((:host . "localhost"))))
    (should (equal (ob-http-construct-url "/" params)
                   "http://localhost/")))

  (let ((params '((:host . "localhost"))))
    (should (equal (ob-http-construct-url "http://local" params)
                   "http://local")))

  (let ((params '((:host . "localhost")
                  (:port . 8080)
                  (:schema . "https"))))
    (should (equal (ob-http-construct-url "/" params)
                   "https://localhost:8080/"))))

(ert-deftest ob-http-test-format-variable ()
  (should (equal (ob-http-format-variable "hello") "\"hello\""))
  (should (equal (ob-http-format-variable 42) "42"))
  (should (equal (ob-http-format-variable 3.14) "3.14"))
  (should (equal (ob-http-format-variable t) "true"))
  (should (equal (ob-http-format-variable nil) "null"))
  (should (equal (ob-http-format-variable :null) "null"))
  (should (equal (ob-http-format-variable :false) "false")))

(ert-deftest ob-http-test-normalize-bindings-plist ()
  (let ((result (ob-http-normalize-bindings '(:planId "abc" :version 7))))
    (should (equal (assoc-default "planId" result) "abc"))
    (should (equal (assoc-default "version" result) 7))))

(ert-deftest ob-http-test-normalize-bindings-alist ()
  (let ((result (ob-http-normalize-bindings '(("planId" . "abc") ("version" . 7)))))
    (should (equal (assoc-default "planId" result) "abc"))
    (should (equal (assoc-default "version" result) 7))))

(ert-deftest ob-http-test-normalize-bindings-symbol-keys ()
  (let ((result (ob-http-normalize-bindings '((planId . "abc") (version . 7)))))
    (should (equal (assoc-default "planId" result) "abc"))
    (should (equal (assoc-default "version" result) 7))))

(ert-deftest ob-http-test-substitute-variables ()
  (let ((bindings '(("planId" . "CfZHxrSgHHQJfQJR") ("targetVersionNumber" . 7))))
    (should (equal (ob-http-substitute-variables "{\"planId\": $planId}" bindings)
                   "{\"planId\": \"CfZHxrSgHHQJfQJR\"}"))
    (should (equal (ob-http-substitute-variables "{\"v\": $targetVersionNumber}" bindings)
                   "{\"v\": 7}"))
    (should (equal (ob-http-substitute-variables "{\"planId\": ${planId}}" bindings)
                   "{\"planId\": \"CfZHxrSgHHQJfQJR\"}"))
    (should (equal (ob-http-substitute-variables "no-match-here" bindings)
                   "no-match-here"))))

(ert-deftest ob-http-test-parse-repeat-over-json ()
  (let ((result (ob-http-parse-repeat-over
                 "[{\"planId\":\"abc\",\"targetVersionNumber\":7}]")))
    (should (= (length result) 1))
    (should (equal (assoc-default "planId" (car result)) "abc"))
    (should (equal (assoc-default "targetVersionNumber" (car result)) 7))))

(ert-deftest ob-http-test-parse-repeat-over-elisp ()
  (let ((result (ob-http-parse-repeat-over
                 "(list (list :planId \"abc\" :targetVersionNumber 7))")))
    (should (= (length result) 1))
    (should (equal (assoc-default "planId" (car result)) "abc"))
    (should (equal (assoc-default "targetVersionNumber" (car result)) 7))))

(ert-deftest ob-http-test-parse-repeat-over-list ()
  (let ((result (ob-http-parse-repeat-over
                 '((:planId "abc" :targetVersionNumber 7)
                   (:planId "def" :targetVersionNumber 8)))))
    (should (= (length result) 2))
    (should (equal (assoc-default "planId" (car result)) "abc"))
    (should (equal (assoc-default "planId" (cadr result)) "def"))))
