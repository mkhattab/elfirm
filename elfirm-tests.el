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
               (agent-dirs (elfirm-p-j "acme" "latest"))
               (file-path
                (concat (elfirm-p-j elfirm-root-directory category-dirname agent-dirs) "agenda.org")))
          (make-directory (elfirm-p-j category-dirname agent-dirs) t)
          (append-to-file "" "" file-path)
          (funcall func)))
    (delete-directory elfirm-root-directory t)
    (setq elfirm-root-directory elfirm-test-old-root)))

(ert-deftest elfirm-test-path-join ()
  "Should return a joined path string."
  (should
   (string= (elfirm-path-join "one" "two" "three") "one/two/three/"))
  (should
   (string= (elfirm-path-join "one") "one/"))
  (should
   (string= (elfirm-path-join) "")))

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
         '((agenda .  ((:dirname . "agenda"))))))
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
     (let* ((client-path
             (elfirm-p-j elfirm-root-directory "agenda" "acme"))
            (client-with-default
             (elfirm-p-j client-path "latest")))
       (should
        (equal (elfirm-locate-client-path "acme" 'agenda) client-path))
       (should
        (equal (elfirm-locate-client-path "monkeys" 'agenda) nil))
       (should
        (equal (elfirm-locate-client-path "acme" 'ledger) nil))
       (should
        (equal (elfirm-locate-client-path "acme" 'agenda t) client-with-default))))))

(ert-deftest elfirm-test-locate-client-file ()
  (elfirm-test-root-dir-fixture
   (lambda ()
     (should
      (equal (stringp (elfirm-locate-client-file "acme" 'agenda "agenda.org")) t))
     (should
      (equal (stringp (elfirm-locate-client-file "acme" 'agenda)) t))
     (should
      (equal (elfirm-locate-client-file "acme" 'agenda "monkey.org") nil)))))

(ert-deftest elfirm-test-list-clients ()
  "Should return a list of clients within a category."
  (elfirm-test-root-dir-fixture
   (lambda ()
     (let ((client-list '("acme")))
       (should (equal (elfirm-list-clients 'agenda) client-list))))))

(ert-deftest elfirm-test-list-categories ()
  "Should return a list of categories."
  (let ((elfirm-categories
         '((agenda .  ((:dirname . "agenda")))
           (monkeys . ((:dirname . "monkeys"))))))
    (should
     (equal (elfirm-list-categories) '("agenda" "monkeys")))))

(ert-deftest elfirm-test-list-files ()
  "Should return a list of files within a client category."
  (elfirm-test-root-dir-fixture
   (lambda ()
     (should
      (equal (elfirm-list-files 'agenda "acme") '("agenda.org"))))))
