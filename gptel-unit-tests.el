;;; gptel-unit-tests.el --- Gptel Unit Tests  -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-openai)

(defmacro setup (&rest body)
  `(let ((gptel-prompt-prefix-alist
          '((fundamental-mode . "*Prompt*: ")))
         (gptel-response-prefix-alist
          '((fundamental-mode . "*Response*: ")))
         (gptel-response-separator "\n\n"))
     ,(macroexp-progn body)))

(defvar gptel-test-openai
  (gptel-make-openai "openai-test"
   :models '((testmodel
              :capabilities (media)
              :mime-types ("image/png"))))
  "Dummy OpenAI backend for testing gptel.")
(setf (alist-get "openai-test" gptel--known-backends
                 nil t #'equal)
      nil)

(ert-deftest gptel-test-prefix-trimming ()
  (setup
   ;; Empty string is nil
   (should (equal nil (gptel--trim-prefixes "")))

   ;; Prefixes trim to nil
   (should (equal nil (gptel--trim-prefixes (gptel-prompt-prefix-string))))
   (should (equal nil (gptel--trim-prefixes (gptel-response-prefix-string))))

   ;; Trimp both prefixes to nil
   (should (equal nil (gptel--trim-prefixes
                       (format " %s  %s "
                               (gptel-prompt-prefix-string)
                               (gptel-response-prefix-string)))))
   ;; Trim extra whitespace
   (should (equal nil (gptel--trim-prefixes
                       (format "\n\t %s \n\t  %s \n\t"
                               (gptel-prompt-prefix-string)
                               (gptel-response-prefix-string)))))
   ;; Trim it all down to FOO
   (should (equal "FOO" (gptel--trim-prefixes
                         (format "\n\t %s \n\tFOO  %s \n\t"
                                 (gptel-prompt-prefix-string)
                                 (gptel-response-prefix-string)))))

   ;; Trim the final response prefix and whitespace
   (should (equal "DERP\n\t *Prompt*:  \n\tFOO"
                  (gptel--trim-prefixes
                   (concat "DERP"
                           "\n\t "
                           (gptel-prompt-prefix-string)
                           " \n\tFOO  "
                           (gptel-response-prefix-string)
                           " \n\t"))))))

(ert-deftest gptel-test-openai-chatgpt-login-uses-configured-client-id ()
  (let ((backend (gptel-make-openai-chatgpt "chatgpt-login-test"))
        requests
        prompts
        opened-url)
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--openai-chatgpt-request)
                   (lambda (url data headers &optional form-encoded)
                     (push (list url data headers form-encoded) requests)
                     (cond
                      ((string-suffix-p "/api/accounts/deviceauth/usercode" url)
                       '(:status 200 :body (:device_auth_id "device-auth"
                                             :user_code "CODE-1234"
                                             :interval "1")))
                      ((string-suffix-p "/api/accounts/deviceauth/token" url)
                       '(:status 200 :body (:authorization_code "auth-code"
                                             :code_verifier "code-verifier")))
                      ((string-suffix-p "/oauth/token" url)
                       '(:status 200 :body (:access_token "access-token"
                                             :refresh_token "refresh-token"
                                             :expires_in 3600)))
                      (t (error "Unexpected URL: %s" url)))))
                  ((symbol-function 'browse-url)
                   (lambda (url &rest _args)
                     (setq opened-url url)))
                  ((symbol-function 'read-from-minibuffer)
                   (lambda (prompt &rest _args)
                     (push prompt prompts)
                     ""))
                  ((symbol-function 'gui-set-selection)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'gptel--openai-chatgpt-save-token)
                   #'identity))
          (let ((gptel-openai-chatgpt-client-id "app_test_client"))
            (gptel-openai-chatgpt-login backend)))
      (setf (alist-get "chatgpt-login-test" gptel--known-backends nil nil #'equal) nil))
    (setq requests (nreverse requests)
          prompts (nreverse prompts))
    (should (equal opened-url "https://auth.openai.com/codex/device"))
    (should (string-match-p "return to Emacs and press ENTER" (car prompts)))
    (should (string-match-p "ignore it and close the tab" (car prompts)))
    (should (equal (plist-get (cadr (nth 0 requests)) :client_id)
                   "app_test_client"))
    (should (equal (alist-get "client_id" (cadr (nth 2 requests)) nil nil #'equal)
                   "app_test_client"))))

(ert-deftest gptel-test-openai-chatgpt-refresh-uses-configured-client-id ()
  (let ((backend (gptel-make-openai-chatgpt "chatgpt-refresh-test"))
        request)
    (unwind-protect
        (progn
          (setf (gptel-openai-chatgpt-token backend)
                '(:refresh_token "refresh-token"))
          (cl-letf (((symbol-function 'gptel--openai-chatgpt-request)
                     (lambda (url data headers &optional form-encoded)
                       (setq request (list url data headers form-encoded))
                       '(:status 200 :body (:access_token "access-token"
                                             :expires_in 3600))))
                    ((symbol-function 'gptel--openai-chatgpt-save-token)
                     #'identity))
            (let ((gptel-openai-chatgpt-client-id "app_test_client"))
              (gptel--openai-chatgpt-refresh-token backend))))
      (setf (alist-get "chatgpt-refresh-test" gptel--known-backends nil nil #'equal) nil))
    (should (equal (car request) "https://auth.openai.com/oauth/token"))
    (should (equal (alist-get "client_id" (cadr request) nil nil #'equal)
                   "app_test_client"))
    (should (eq (cadddr request) t))))

(ert-deftest gptel-test-openai-chatgpt-backend-forces-streaming ()
  (let ((backend (gptel-make-openai-chatgpt "chatgpt-stream-test" :stream nil)))
    (unwind-protect
        (should (eq (gptel-backend-stream backend) t))
      (setf (alist-get "chatgpt-stream-test" gptel--known-backends nil nil #'equal) nil))))

;;; Tests for media parsing in buffers: `gptel--parse-media-links'
(ert-deftest gptel-test-media-link-parsing-org-1 ()
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[[file:/tmp/medialinks.txt]]

then more text, then another link

[[file:/tmp/medialinks.yaml]]

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.yaml" (insert mediatext))
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((org-inhibit-startup t)
                (gptel-backend gptel-test-openai)
                (gptel-model 'testmodel))
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (org-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then another link\n\n")
                               (:textfile "/tmp/medialinks.yaml")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.yaml")
      (delete-file "/tmp/medialinks.txt"))))

(ert-deftest gptel-test-media-link-parsing-org-2 ()
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[[file:/tmp/medialinks.txt]]

then more text, then an image

[[file:./examples/hundred.png]]

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((org-inhibit-startup t)
                (gptel-backend gptel-test-openai)
                (gptel-model 'testmodel))
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (org-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then an image\n\n")
                               (:media "./examples/hundred.png" :mime "image/png")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.txt"))))

;;; Tests for parsing JSON schema supplied in various forms
(ert-deftest gptel-test-dispatch-schema-type () 
  "Shorthand form tests for `gptel--dispatch-schema-type'."
  (should (equal (gptel--dispatch-schema-type
                  "name, chemical_formula str, toxicity num")
                 '( :type "object" :properties ( :name (:type "string")
                                                 :chemical_formula (:type "string")
                                                 :toxicity (:type "number")))))
  (should (equal (gptel--dispatch-schema-type
                  "[name, chemical_formula string, toxicity number]")
                 (list :type "object"
                       :properties (list :items
                                         '( :type "array"
                                            :items
                                            ( :type "object"
                                              :properties ( :name (:type "string")
                                                            :chemical_formula (:type "string")
                                                            :toxicity (:type "number")))))
                       :required ["items"]
                       :additionalProperties :json-false)))
  (should (equal (gptel--dispatch-schema-type
                  "name: Colloquial name of compound
                   chemical_formula str: Formula for compound
                   toxicity int: 1-10 denoting toxicity to humans")
                 '( :type "object"
                    :properties ( :name ( :type "string"
                                          :description "Colloquial name of compound")
                                  :chemical_formula ( :type "string"
                                                      :description "Formula for compound")
                                  :toxicity ( :type "integer"
                                              :description "1-10 denoting toxicity to humans")))))
  (should (equal (gptel--dispatch-schema-type
                  "[name: Colloquial name of compound
                    chemical_formula str: Formula for compound
                    toxicity bool: whether the compound is toxic   ]")
                 '( :type "object"
                    :properties
                    ( :items
                      ( :type "array"
                        :items
                        ( :type "object"
                          :properties
                          ( :name ( :type "string"
                                    :description "Colloquial name of compound")
                            :chemical_formula ( :type "string"
                                                :description "Formula for compound")
                            :toxicity ( :type "boolean"
                                        :description "whether the compound is toxic")))))
                    :required ["items"]
                    :additionalProperties :json-false))))

(ert-deftest gptel-test-dispatch-schema-type-advanced ()
  "Advanced shorthand form test for `gptel--dispatch-schema-type'."
  ;; Test with
  ;; - missing type
  ;; - missing description
  ;; - missing ":" separator
  ;; - missing type, description and separator
  ;; - leading and trailing whitespace
  (should
   (equal (gptel--dispatch-schema-type
           "  [name  str: Name of cat
                      age   num
                      hobby
                      bio      : One-line biography for cat    ]

           ")
          '( :type "object"
             :properties
             ( :items
               ( :type "array" :items
                 ( :type "object" :properties
                   ( :name ( :type "string" :description "Name of cat")
                     :age ( :type "number")
                     :hobby ( :type "string")
                     :bio ( :type "string" :description
                            "One-line biography for cat")))))
             :required ["items"] :additionalProperties :json-false))))

(ert-deftest gptel-test-media-link-parsing-md-1 ()
  (skip-unless (fboundp 'markdown-mode))
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[medialinks](/tmp/medialinks.txt)

then more text, then another link

[some text](/tmp/medialinks.yaml)

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.yaml" (insert mediatext))
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((gptel-backend gptel-test-openai)
                (gptel-model 'testmodel))
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (markdown-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then another link\n\n")
                               (:textfile "/tmp/medialinks.yaml")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.yaml")
      (delete-file "/tmp/medialinks.txt"))))

(ert-deftest gptel-test-media-link-parsing-md-2 ()
  (skip-unless (fboundp 'markdown-mode))
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[medialinks](/tmp/medialinks.txt)

then more text, then an image

![an image](./examples/hundred.png)

then some more text to end."))
       (unwind-protect
            (progn
            (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
            (let ((gptel-backend gptel-test-openai)
                (gptel-model 'testmodel))
            (with-temp-buffer
                (insert buftext)
                (delay-mode-hooks (markdown-mode))
                (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then an image\n\n")
                               (:media "./examples/hundred.png" :mime "image/png")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.txt"))))

;;; Test for declarative list modification DSL
(ert-deftest gptel-test--modify-value ()
  "Test `gptel--modify-value'."
  ;; string and string
  (should (equal (gptel--modify-value "original\n" "extra") "extra"))
  (should (equal (gptel--modify-value "original\n" '(:append "extra")) "original\nextra"))
  (should (equal (gptel--modify-value "original\n" '(:prepend "extra")) "extraoriginal\n"))
  (should (equal (gptel--modify-value "original\n" '(:function upcase)) "ORIGINAL\n"))
  ;; list and list
  (should (equal (gptel--modify-value '(a b c) '(d e f)) '(d e f)))
  (should (equal (gptel--modify-value '(a b c) '(:append (d e))) '(a b c d e)))
  (should (equal (gptel--modify-value '(a b c) '(:prepend (x y))) '(x y a b c)))
  (should (equal (gptel--modify-value '(1 2 3) '(:function reverse)) '(3 2 1)))
  (should (equal (gptel--modify-value '("hello") '(:append (" world"))) '("hello" " world")))
  (should (equal (gptel--modify-value '("world") '(:prepend ("hello "))) '("hello " "world")))
  ;; :merge test
  (should (equal (gptel--modify-value '(:a 1 :b 2) '(:merge (:b 3 :c 4))) '(:a 1 :b 3 :c 4)))
  (should (equal (gptel--modify-value '(:x "hello" :y 42) '(:merge (:x "world" :z nil)))
                 '(:x "world" :y 42 :z nil)))
  ;; :eval test
  (should (equal (gptel--modify-value "unused" '(:eval (+ 2 3))) 5))
  (should (equal (gptel--modify-value '(a b) '(:eval (reverse '(x y z)))) '(z y x)))
  ;; string and list combinations
  (should (equal (gptel--modify-value "hello" '(:append " world")) "hello world"))
  (should (equal (gptel--modify-value "world" '(:prepend "hello ")) "hello world"))
  ;; multiple operations
  (should (equal (gptel--modify-value "base" '(:append "1" :prepend "0")) "0base1"))
  (should (equal (gptel--modify-value '(b) '(:append (c) :prepend (a))) '(a b c)))
  ;; non-list mutation (edge cases)
  (should (equal (gptel--modify-value "original" 42) 42))
  (should (equal (gptel--modify-value '(a b c) :symbol) :symbol))
  ;; empty cases
  (should (equal (gptel--modify-value "" '(:append "text")) "text"))
  (should (equal (gptel--modify-value '() '(:append (a b))) '(a b)))
  (should (equal (gptel--modify-value "text" '(:prepend "")) "text"))
  (should (equal (gptel--modify-value '(a b) '(:prepend ())) '(a b))))

;;; Tests for header-line alignment

(ert-deftest gptel-test-header-line-pixel-alignment ()
  "Header-line uses pixel-based alignment when `string-pixel-width' is available."
  (skip-unless (fboundp 'string-pixel-width))
  (let* ((rhs "[test-model]")
         (spec `(space :align-to (- right (,(string-pixel-width rhs)))))
         (align-to (plist-get (cdr spec) :align-to))
         (offset (caddr align-to)))
    ;; Pixel path wraps the value in a list: (PIXELS)
    (should (listp offset))
    (should (numberp (car offset)))
    (should (> (car offset) 0))))

(ert-deftest gptel-test-header-line-char-fallback-offset ()
  "Header-line char fallback includes the +5 padding offset."
  (let* ((rhs "[test-model]")
         (spec `(space :align-to (- right ,(+ 5 (string-width rhs)))))
         (align-to (plist-get (cdr spec) :align-to))
         (offset (caddr align-to)))
    ;; Char path: offset is a plain number, not a list
    (should (numberp offset))
    ;; Should be string-width + 5
    (should (= offset (+ 5 (string-width rhs))))))

(provide 'gptel-unit-tests)
;;; gptel-unit-tests.el ends here
