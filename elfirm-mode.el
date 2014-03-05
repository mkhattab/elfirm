;;; elfirm-mode.el --- Minor mode for managing a consulting firm.
;;
;; Copyright (c) 2014 Mustafa Khattab
;;
;; Author: Mustafa Khattab
;; Version: 0.01
;; Created: 3 Mar 2014

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Minor mode for managing a consulting and contracting company.


;;; Code:

(defconst elfirm-version "0.01"
  "Elfirm mode version.")

(defgroup elfirm nil
  "A minor mode for managing a consulting firm."
  :group 'convenience)

(defcustom elfirm-root-directory nil
  "The data directory for the consulting firm.

By default this is the parent directory of the Elfirm package."
  :group 'elfirm
  :type 'directory)

(defcustom elfirm-categories
  '((agenda . ("agenda" "latest" "agenda.org"))
    (documents . ("documents" nil nil))
    (invoices . ("invoices" "latest" "invoices.org"))
    (ledger . ("ledger" "latest" "acct.dat"))
    (reports . ("reports" "latest" nil)))
  "Alist defining a client list spec for each category.

Each symbol maps to a spec according to the following format (in
order):
 - dirname: category directory name.
 - default-directory: default directory within client directory.
 - default-file: default file within `default directory'."
  :group 'elfirm)

(defcustom elfirm-ignore-client-names ()
  "List of client names to ignore.

This list is useful for dummy clients, i.e. general accounts or
projects."
  :group 'elfirm
  :type 'list)

(defvar elfirm-current-client nil
  "The current client by which Elfirm operations are based.

Typically this variable is set via local or directory local
variable and the client name is displayed in the mode-line. If
this variable is nil, any operations will require a client to
be selected.")

;;; Functions:

(defun elfirm-version ()
  "Reports elfirm version number."
  (interactive)
  (message elfirm-version))

(defalias 'elfirm-f-as-d 'file-name-as-directory)

(defun elfirm-str-starts-with-p (str begins)
  "Return non-nil if string STR start with BEGINS."
  (and (string-match (rx-to-string `(and bos ,begins) t) str) t))

(defun elfirm-str-ends-with-p (str ends)
  "Return non-nil if string STR ends with ENDS."
  (and (string-match (rx-to-string `(and ,ends eos) t) str) t))

(defun elfirm-category-dirname (category)
  "Return CATEGORY directory name."
  (cadr (assq category elfirm-categories)))

(defun elfirm-locate-category-path (category)
  "Return the directory path for symbol CATEGORY, if exists, otherwise nil."
  (let ((category-path
         (concat (elfirm-f-as-d elfirm-root-directory)
                 (elfirm-category-dirname category))))
    (if (file-exists-p category-path)
        (elfirm-f-as-d category-path))))

(defun elfirm-locate-client-path (client category)
  "Return the client path for string CLIENT within symbol CATEGORY.

If CLIENT does not exist, return nil."
  (let* ((category-path
          (elfirm-locate-category-path category))
         (client-path
          (and category-path (concat category-path client))))
    (if (and client-path
             (file-exists-p client-path))
        (elfirm-f-as-d client-path))))

(defun elfirm-client-p (client)
  "Return non-nil if CLIENT string is not in ignored list."
  (null (memq client elfirm-ignore-client-names)))

(defun elfirm-list-clients (category)
  "List clients within CATEGORY."
  (let* ((category-path
         (elfirm-locate-category-path category))
        (clients
         (mapcar (lambda (c)
                   (and
                    (not (or (string= c ".") (string= c "..")))
                    (elfirm-client-p c)
                    c))
                 (directory-files category-path))))
    (delq nil clients)))

(define-minor-mode elfirm-mode
  "Toggle Elfirm mode.

When Elfirm mode is enabled, it provides various keybindings to
jump between clients and accounts within an Elfirm data
directory."
  nil
  "Elfirm"
  '()
  :group 'elfirm)

(provide 'elfirm-mode)

;;; elfirm-mode.el ends here
