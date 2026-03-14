;;; gptel-openai-chatgpt-tests.el --- Tests for ChatGPT Responses tool handling -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-openai)

(defun gptel-openai-chatgpt-test-tool ()
  (gptel-make-tool
   :name "shell_execute"
   :function (lambda (cmd) cmd)
   :description "Execute a shell command."
   :args (list '(:name "cmd"
                 :type string
                 :description "Command to execute."))))

(defmacro gptel-openai-chatgpt-test-with-env (&rest body)
  `(let* ((backend (gptel-make-openai-chatgpt "Test ChatGPT"))
          (gptel-backend backend)
          (gptel-model 'gpt-5.4)
          (gptel-stream t)
          (gptel-use-tools t)
          (gptel-tools nil)
          (gptel-temperature nil)
          (gptel-max-tokens nil)
          (gptel--system-message nil)
          (gptel--schema nil)
          (gptel--request-params nil))
     ,@body))

(ert-deftest gptel-openai-chatgpt-request-data-shapes-tools-for-responses ()
  (gptel-openai-chatgpt-test-with-env
   (let* ((gptel--system-message "System")
          (gptel-tools (list (gptel-openai-chatgpt-test-tool)))
          (gptel-temperature 1.0)
          (payload (gptel--request-data
                    backend
                    '((:role "user" :content "Run pwd"))))
          (input (plist-get payload :input))
          (tool (aref (plist-get payload :tools) 0))
          (content (plist-get (aref input 0) :content)))
     (should-not (plist-member payload :messages))
     (should-not (plist-member payload :temperature))
     (should (equal (plist-get payload :instructions) "System"))
     (should (eq (plist-get payload :store) :json-false))
     (should (eq (plist-get payload :stream) t))
     (should (equal (plist-get tool :type) "function"))
     (should (equal (plist-get tool :name) "shell_execute"))
     (should-not (plist-member tool :function))
     (should (equal (plist-get (aref input 0) :role) "user"))
     (should (equal (plist-get (aref content 0) :type) "input_text"))
     (should (equal (plist-get (aref content 0) :text) "Run pwd")))))

(ert-deftest gptel-openai-chatgpt-parse-response-injects-function-calls ()
  (gptel-openai-chatgpt-test-with-env
   (let* ((info (list :backend backend :data (list :input [])))
          (response
           '(:response
             (:status "completed"
              :output
              [(:type "function_call"
                :call_id "call_1"
                :name "shell_execute"
                :arguments "{\"cmd\":\"pwd\"}")]))))
     (should-not (gptel--parse-response backend response info))
     (should (equal (plist-get (car (plist-get info :tool-use)) :name)
                    "shell_execute"))
     (should (equal (plist-get (plist-get (car (plist-get info :tool-use)) :args) :cmd)
                    "pwd"))
     (let* ((input (plist-get (plist-get info :data) :input))
            (item (aref input 0)))
       (should (equal (plist-get item :type) "function_call"))
       (should (equal (plist-get item :call_id) "call_1"))
       (should (equal (plist-get item :name) "shell_execute"))))))

(ert-deftest gptel-openai-chatgpt-parse-tool-results-uses-function-call-output-items ()
  (gptel-openai-chatgpt-test-with-env
   (let* ((data (list :input []))
          (items (gptel--parse-tool-results
                  backend
                  '((:id "call_1" :result "ok")))))
     (gptel--inject-prompt backend data items)
     (let ((item (aref (plist-get data :input) 0)))
       (should (equal (plist-get item :type) "function_call_output"))
       (should (equal (plist-get item :call_id) "call_1"))
       (should (equal (plist-get item :output) "ok"))))))

(ert-deftest gptel-openai-chatgpt-inject-tool-call-updates-input-items ()
  (gptel-openai-chatgpt-test-with-env
   (let* ((data (list :input
                      [(:type "function_call"
                        :call_id "call_1"
                        :name "shell_execute"
                        :arguments "{\"cmd\":\"pwd\"}")]))
          (tool-call '(:id "call_1" :name "shell_execute" :args (:cmd "pwd"))))
     (gptel--inject-tool-call
      backend data tool-call '(:name "shell_list" :args (:cmd "ls")))
     (let ((item (aref (plist-get data :input) 0)))
       (should (equal (plist-get item :name) "shell_list"))
       (should (equal (plist-get item :arguments) "{\"cmd\":\"ls\"}"))))))

(ert-deftest gptel-openai-chatgpt-parse-stream-collects-tool-calls ()
  (gptel-openai-chatgpt-test-with-env
   (let ((info (list :backend backend :data (list :input []))))
     (with-temp-buffer
       (insert-file-contents
        "/work/test/examples-responses/openai/chatgpt-codex-tool-call-stream-01.txt")
       (goto-char (point-min))
       (should (equal (gptel-curl--parse-stream backend info) "")))
     (should (equal (plist-get (car (plist-get info :tool-use)) :name)
                    "echo_text"))
     (should (equal (plist-get (plist-get (car (plist-get info :tool-use)) :args) :text)
                    "tool call success"))
     (let ((item (aref (plist-get (plist-get info :data) :input) 0)))
       (should (equal (plist-get item :type) "function_call"))
       (should (equal (plist-get item :call_id) "call_UdIVO5T01fxPEpTiwA5Ir7hf"))
       (should (equal (plist-get item :name) "echo_text"))))))
