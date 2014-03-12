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

(defun elfirm-str-trim (str)
  "Trim leading and trailing whitespace from STR."
  (while (string-match "^ +\\| +$" str)
    (setq str (replace-match "" t t str)))
  str)

(defun elfirm-str-split (str sep &optional trim)
  "Split STR with separator SEP.

If TRIM is t, then trim invidual strings."
  (let ((strings
         (split-string str sep t)))
    (if trim
        (mapcar 'elfirm-str-trim strings)
      strings)))

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
                                           :default-directory category)
                                          "")))))
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
        file-path
      (error (format "default-file: %s is missing" filename)))))

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
  "Return a list of categories."
  (mapcar (lambda (cat)
             (symbol-name (car cat)))
          elfirm-categories))

(defun elfirm-list-files (client category)
  "Return a list of files within a CLIENT in CATEGORY."
  (let* ((category-path
          (elfirm-locate-client-path client category t))
         (files
          (directory-files category-path nil "[^\.\.\.]")))
    files))

(defun elfirm-select-category ()
  "Return selected category."
  (intern-soft (completing-read "category: " (elfirm-list-categories))))

(defun elfirm-select-client (category &optional prefix)
  "Return selected client within a CATEGORY.

This function is used in an interactive command, if run with a
PREFIX \\[universal-argument] \\[universal-argument], then select
a client from a list. Otherwise, use `elfirm-current-client', if
set."
  (let ((cr-client
         (lambda ()
           (completing-read "client: " (elfirm-list-clients category)))))
    (cond ((equal prefix '(16)) (funcall cr-client))
          (elfirm-current-client)
          (t (funcall cr-client)))))

(defun elfirm-select-file (client category &optional prefix)
  "Return a selected file in CLIENT within CATEGORY.

This function is used in an interactive command, if run with a
PREFIX \\[universal-argument] \\[universal-argument], then select
a client from a list. Otherwise, use `elfirm-current-client', if
set."
  (cond ((equal prefix '(4))
         (completing-read "file: " (elfirm-list-files category client)))
        (t
         (elfirm-category-property :default-file category))))

(defun elfirm-find-category-directory (client category &optional other)
  "Find CLIENT directory within CATEGORY.

If a command prefix is passed then prompt for other directory.
OTHER is non-nil, prompt for sub directory."
  (interactive
   (let ((category
          (elfirm-select-category)))
     (list (elfirm-select-client category current-prefix-arg)
           category
           (and (equal current-prefix-arg '(4)) t))))
  (dired (elfirm-locate-client-path client category (null other))))

(defun elfirm-find-category-file (client category &optional file)
  "Find default file in CLIENT within CATEGORY.

If a command prefix is passed, then prompt for other FILE."
  (interactive
   (let* ((category
           (elfirm-select-category))
          (client
           (elfirm-select-client category current-prefix-arg))
          (file
           (elfirm-select-file client category current-prefix-arg )))
     (list category client file)))
  (find-file (elfirm-locate-client-file client category file)))

(defmacro elfirm-deffind-category (name category)
  "Define an interactive command NAME that will find a CATEGORY."
  `(defun ,(intern (concat "elfirm-find-category-->" name)) (client &optional other)
     (interactive
      (list
       (elfirm-select-client ,category current-prefix-arg)
       (and (equal current-prefix-arg '(4)) t)))
     (elfirm-find-category-directory client ,category other)))

(defmacro elfirm-deffind-file (name category)
  "Define an interactive command NAME that will find file in CATEGORY."
  `(defun ,(intern (concat "elfirm-find-file-->" name)) (client &optional file)
     (interactive
      (let* ((client
              (elfirm-select-client ,category current-prefix-arg))
             (file
              (elfirm-select-file client ,category current-prefix-arg)))
        (list client file)))
     (elfirm-find-category-file client ,category file)))

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

(defun elfirm-create-root-directory ()
  "Create root directory."
  (make-directory elfirm-root-directory)
  elfirm-root-directory)

(defun elfirm-create-category-directory (name)
  "Create a category directory NAME, if it doesn't exist."
  (let ((category-path
         (elfirm-p-j elfirm-root-directory name)))
    (if (and (file-exists-p elfirm-root-directory)
             (not (file-exists-p category-path)))
        (make-directory category-path))))

(defun elfirm-create-client-directory (name &optional categories)
  "Create a client with NAME, if it doesn't exist.

Create client directories in a subset of CATEGORIES, if non-nil."
  (let ((categorylist (or categories
                          (elfirm-list-categories))))
    (dolist (category categorylist)
      (if (file-exists-p
           (elfirm-locate-category-path (intern-soft category)))
          (make-directory
           (elfirm-p-j elfirm-root-directory
                       category
                       name
                       (or (elfirm-category-property :default-directory
                                                     (intern-soft category))
                           ""))
           t)                           ;create parent dirs
        (error "Category directory doesn't exist")))))

(defun elfirm-init-root ()
  "Initialize root directory, creating category directories."
  (interactive)
  (let ((confirm
         (yes-or-no-p
          (format "init elfirm data directory at %s?" elfirm-root-directory))))
    (if confirm
        (progn
          (if (not (file-exists-p elfirm-root-directory))
              (elfirm-create-root-directory))
          (dolist (category (elfirm-list-categories))
            (elfirm-create-category-directory category))))))

(defun elfirm-init-client (name &optional categories)
  "Initialize client NAME.

If passed with \\[universal-argument], then select CATEGORIES."
  (interactive
   (let ((categories
          (mapconcat
           'identity (elfirm-list-categories) ", ")))
     (list
      (read-string "client: ")
      (if current-prefix-arg
          (read-string "categories: " categories nil categories)
        categories))))
  (if name
      (elfirm-create-client-directory
       name
       (elfirm-str-split categories "," t))
    (error "Please enter a client name")))

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
