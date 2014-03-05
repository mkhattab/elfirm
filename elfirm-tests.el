;;; elfirm-tests.el --- tests for Elfirm

;;; Code:
(require 'ert)
(require 'elfirm-mode)

(defun elfirm-test-root-dir-fixture (func)
  (unwind-protect
      (progn
        (setq elfirm-test-old-root elfirm-root-directory)
        (setq elfirm-root-directory (make-temp-file "elfirm-tests" t))
        (let* ((category-dirname
                (elfirm-f-as-d (elfirm-category-dirname 'agenda)))
               (default-directory
                 elfirm-root-directory)
               (agent-dirname (elfirm-f-as-d "acme")))
          (make-directory (concat category-dirname agent-dirname) t)
          (funcall func)))
    (delete-directory elfirm-root-directory t)
    (setq elfirm-root-directory elfirm-test-old-root)))

(ert-deftest elfirm-test-string-starts-with ()
    "Tests the string-starts-with predicate."
  (should (string= (elfirm-str-starts-with-p "../dir" "..") t))
  (should (string= (elfirm-str-starts-with-p "../dir" "/") nil))
  (should (string= (elfirm-str-starts-with-p "/dir" "/") t)))

(ert-deftest elfirm-test-string-ends-with ()
  "Tests the string-ends-with predicate."
  (should (string= (elfirm-str-ends-with-p "../dir" "dir") t))
  (should (string= (elfirm-str-ends-with-p "../dir" "..") nil)))

(ert-deftest elfirm-test-category-dirname ()
  "Should return category directory name, if exists."
  (let ((elfirm-categories
         '((agenda .  ("agenda" nil nil nil)))))
      (should (string= (elfirm-category-dirname 'agenda) "agenda"))
      (should (string= (elfirm-category-dirname 'monkey) nil))))

(ert-deftest elfirm-test-locate-category ()
  "Should return path of category within Elfirm root directory."
  (elfirm-test-root-dir-fixture
   (lambda ()
     (let ((category-path
            (concat (elfirm-f-as-d elfirm-root-directory)
                    (elfirm-f-as-d "agenda"))))
       (should (equal (elfirm-locate-category-path 'agenda) category-path))
       (should (equal (elfirm-locate-category-path 'ledger) nil))))))

(ert-deftest elfirm-test-locate-client ()
  "Should return path of client within category."
  (elfirm-test-root-dir-fixture
   (lambda ()
     (let ((client-path
            (concat (elfirm-f-as-d elfirm-root-directory)
                    "agenda/acme/")))
       (should (equal (elfirm-locate-client-path "acme" 'agenda) client-path))
       (should (equal (elfirm-locate-client-path "monkeys" 'agenda) nil))
       (should (equal (elfirm-locate-client-path "acme" 'ledger) nil))))))

(ert-deftest elfirm-test-list-clients ()
  "Should return a list of clients within a category."
  (elfirm-test-root-dir-fixture
   (lambda ()
     (let ((client-list '("acme")))
       (should (equal (elfirm-list-clients 'agenda) client-list))))))
