;;; elfirm-mode.el --- Minor mode for managing a consulting firm.
;;
;; Copyright (c) 2014 Mustafa Khattab
;;
;; Author: Mustafa Khattab
;; Version: 0.1.0
;; Created: 3 Mar 2014

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Minor mode for managing a consulting and contracting company.


;;; Code:

(defconst elfirm-version "0.1.0"
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
  '((agenda . ((:dirname . "agenda")
               (:default-directory . "latest")
               (:default-file . "agenda.org")))
    (documents . ((:dirname . "documents")))
    (invoices . ((:dirname . "invoices")
                 (:default-directory . "latest")
                 (:default-file . "invoices.org")))
    (ledger . ((:dirname . "ledger")
               (:default-directory . "latest")
               (:default-file . "acct.dat")))
    (reports . ((:dirname . "reports")
                (:default-directory . "latest"))))
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

(defun elfirm-path-join (&rest paths)
  "Return a concatenated string of PATHS."
  (apply 'concat (mapcar 'elfirm-f-as-d paths)))

(defalias 'elfirm-p-j 'elfirm-path-join)
(defalias 'elfirm-f-as-d 'file-name-as-directory)

(defmacro elfirm-category-property (property category)
  "Return PROPERTY of CATEGORY."
  `(cdr (assq ,property (cdr (assq ,category elfirm-categories)))))

(defun elfirm-version ()
  "Reports elfirm version number."
  (interactive)
  (message elfirm-version))

(defun elfirm-str-starts-with-p (str begins)
  "Return non-nil if string STR start with BEGINS."
  (and (string-match (rx-to-string `(and bos ,begins) t) str) t))

(defun elfirm-str-ends-with-p (str ends)
  "Return non-nil if string STR ends with ENDS."
  (and (string-match (rx-to-string `(and ,ends eos) t) str) t))

(defun elfirm-category-dirname (category)
  "Return CATEGORY directory name."
  (elfirm-category-property :dirname category))

(defun elfirm-locate-category-path (category)
  "Return the directory path for symbol CATEGORY, if exists, otherwise nil."
  (let ((category-path
         (elfirm-p-j elfirm-root-directory
                     (elfirm-category-dirname category))))
    (if (file-exists-p category-path)
        (elfirm-f-as-d category-path))))

(defun elfirm-locate-client-path (client category &optional use-default directory)
  "Return the client path for string CLIENT within symbol CATEGORY.

If CLIENT does not exist, return nil. If USE-DEFAULT is t, then
append default directory to CATEGORY path. If DIRECTORY is
non-nil, then find that sub-directory."
  (let* ((category-path
          (elfirm-locate-category-path category))
         (client-path
          (and category-path (elfirm-p-j category-path client)))
         (client-default-path           ;append default path if
                                        ;use-default is non-nil
          (if use-default
              (elfirm-p-j client-path (or directory
                                          (elfirm-category-property
                                           :default-directory category))))))
    (if (and client-path (file-exists-p client-path))
        (cond ((and use-default (file-exists-p client-default-path))
               client-default-path)
              (t
               client-path))
      nil)))

(defun elfirm-locate-client-file (client category &optional file directory)
  "Return default file path in CLIENT within CATEGORY default directory.

If FILE is non-nil, then find that file. And if DIRECTORY is
non-nil, then find FILE within that DIRECTORY. Otherwise, use the
default directory specified in `elfirm-categories'."
  (let* ((filename
         (or file (elfirm-category-property :default-file category)))
         (client-path
          (elfirm-locate-client-path client category t directory))
         (file-path
          (concat client-path filename)))
    (if (file-exists-p file-path)
        file-path)))

(defun elfirm-client-p (client)
  "Return non-nil if CLIENT string is not in ignored list."
  (null (memq client elfirm-ignore-client-names)))

(defun elfirm-list-clients (category)
  "List clients within CATEGORY."
  (let* ((category-path
         (elfirm-locate-category-path category))
        (clients
         (mapcar (lambda (c)
                   (and (elfirm-client-p c)
                    c))
                 (directory-files category-path nil "[^\.\.\.]"))))
    (delq nil clients)))

(defun elfirm-list-categories ()
  (mapcar (lambda (cat)
             (symbol-name (car cat)))
          elfirm-categories))

(defun elfirm-list-files (category client)
  (let* ((category-path
          (elfirm-locate-client-path client category t))
         (files
          (directory-files category-path nil "[^\.\.\.]")))
    files))

(defun elfirm-select-category ()
  (intern-soft (completing-read "category: " (elfirm-list-categories))))

(defun elfirm-select-client (prefix category)
  (let ((cr-client
         (lambda ()
           (completing-read "client: " (elfirm-list-clients category)))))
    (cond ((equal prefix '(16)) (funcall cr-client))
          (elfirm-current-client)
          (t (funcall cr-client)))))

(defun elfirm-select-file (prefix category client)
  (cond ((equal prefix '(4))
         (completing-read "file: " (elfirm-list-files category client)))
        (t
         (elfirm-category-property :default-file category))))

(defun elfirm-find-category-directory (category client &optional other)
  "Find CATEGORY directory for CLIENT.

If a command prefix is passed then prompt for other directory.
OTHER is non-nil, prompt for sub directory."
  (interactive
   (let ((category
          (elfirm-select-category)))
     (list (elfirm-select-client current-prefix-arg category)
           category
           (and (equal current-prefix-arg '(4)) t))))
  (dired (elfirm-locate-client-path client 'agenda (null other))))

(defun elfirm-find-category-file (category client &optional file)
  "Find default file in CATEGORY.

If a command prefix is passed, then prompt for other FILE."
  (interactive
   (let* ((category
           (elfirm-select-category))
          (client
           (elfirm-select-client current-prefix-arg category))
          (file
           (elfirm-select-file current-prefix-arg category client)))
     (list category client file)))
  (find-file (elfirm-locate-client-file client category file)))

(defmacro elfirm-deffind-category (name category)
  "Define an interactive command NAME that will find a CATEGORY."
  `(defun ,(intern (concat "elfirm-find-category-->" name)) (client &optional other)
     (interactive
      (list
       (elfirm-select-client current-prefix-arg ,category)
       (and (equal current-prefix-arg '(4)) t)))
     (elfirm-find-category-directory ,category client other)))

(defmacro elfirm-deffind-file (name category)
  "Define an interactive command NAME that will find file in CATEGORY."
  `(defun ,(intern (concat "elfirm-find-file-->" name)) (client &optional file)
     (interactive
      (let* ((client
              (elfirm-select-client current-prefix-arg ,category))
             (file
              (elfirm-select-file current-prefix-arg ,category client)))
        (list client file)))
     (elfirm-find-category-file ,category client file)))

(elfirm-deffind-category "agenda" 'agenda)
(elfirm-deffind-category "documents" 'documents)
(elfirm-deffind-category "invoices" 'invoices)
(elfirm-deffind-category "ledger" 'ledger)
(elfirm-deffind-category "reports" 'reports)

(elfirm-deffind-file "agenda" 'agenda)
(elfirm-deffind-file "documents" 'documents)
(elfirm-deffind-file "invoices" 'invoices)
(elfirm-deffind-file "ledger" 'ledger)
(elfirm-deffind-file "reports" 'reports)

;; (defun elfirm-create-client-direcotry (name &optional categories))
;; (defun elfirm-create-root-directory ())

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
