;; av-psql.el --- Generate functions to connect to Postgres database

;;; Commentary:
;; this assumes you only care about postgres
;; this assumes you are using ~/.pg_service.conf
;; this creates a function db-<service-name> for every entry in the conf file

;; When invoking the function, you will be connected to that service
;; and a scratch sql buffer will be created for you that's tied to
;; that buffer.
;;
;; If you use ido, there is also M-x av-pg-server-connect which will give you a list to choose from.
;;
;;; Code:

(defvar av-pg-conf-file-path (expand-file-name "~/.pg_service.conf")
  "Path to find pg service file.  Assumed to be $HOME/.pg_service.conf.")

(defun av-pg-service-name->func-sym (name)
  "Provides the standard function name to connect to service NAME."
  (intern (concat "db-" name)))

(defun av-sql-interactive-mode-hook ()
  "Things to do after loading `sql-interactive-mode'
- Set prompts to work with underscore in the db name
- Name the sql process buffer after the database name
- Don't wrap lines
- enable full font-locking"
   (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] "
         sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")

   (setq sql-alternate-buffer-name sql-database)
   (sql-rename-buffer)
   (toggle-truncate-lines t)
   (sql-product-font-lock nil nil))

(defun av-sql-postgres(service)
  "Starts a sqli buffer for SERVICE.

The sql interactive buffer is named after the service's name. It
will create a scratch buffer and name it after the service's
name. It will connect that buffer to the new sqli buffer.

If the scratch buffer already exists, a new connection will not
be created."
  (let ((existing-buffer (get-buffer (format "*SQL Scratch %s*" service))))
    (if existing-buffer t
      (pop-to-buffer existing-buffer)
      (let ((buf (generate-new-buffer (format "*SQL Scratch %s*" service))))
            (with-current-buffer buf
              (switch-to-buffer buf)
              (sql-mode)
              (setq sql-product 'postgres)
              (sql-connect service)
              (pop-to-buffer buf))))))

(defun av-delete-buffer-comments ()
  "Deletes comments from every line in current buffer."
  (goto-char (point-min))
  (comment-kill (count-lines (point-min) (point-max))))

(defun av-read-buffer-lines ()
  "Return list of lines in current buffer.  Empty lines are omitted."
  (split-string (buffer-string) "\n" t))

(defun av-read-lines-without-comments (file-path file-mode)
  "Returns a list of lines from FILE-PATH withouth comments or
empty lines. FILE-MODE is required so that the correct comment
signifier is used."
  (with-temp-buffer
    (insert-file-contents file-path)
    (funcall file-mode)
    (av-delete-buffer-comments)
    (av-read-buffer-lines)))

(defun av-read-lines-from-conf-file(conf-file-path)
  "Return lines without comments or blanks from the conf-file
CONF-FILE-PATH. It will use `conf-unix-mode' for its comment
definition."
  (av-read-lines-without-comments conf-file-path 'conf-unix-mode))

(defun av-services-from-list (list)
  "Return a list of services extracted from LIST.

LIST is assumed to be the lines of a conf file.  Services are
defined by text found within brackets.  For example:
\"[apples-oranges]\" would result in \"apples-oranges\""
  (delq nil (mapcar (lambda(str)
                      (when (string-match "\\[\\(.*\\)\\]" str)
                        (match-string 1 str)))
                    list )))

(defun av-pg-service-list (conf-file-path)
  "Return list of services in CONF-FILE-PATH."
  (av-services-from-list (av-read-lines-from-conf-file conf-file-path)))

(defun av-add-service-to-connection-list (service)
  "Creates a connection spec for SERVICE and appends it to
`sql-connection-alist'. It assumes the product is postgres."
  (add-to-list 'sql-connection-alist `(,service
                                       (sql-product 'postgres)
                                       (sql-database  ,(concat "service=" service)))))

(defun av-create-pg-service-connection-defun (service)
  "Creates an interactive function called db-<service> which will
connect to the SERVICE using `av-sql-postgres'"
  (fset (av-pg-service-name->func-sym  service)
        `(lambda ()
           (interactive)
           (av-sql-postgres ,service))))

(defvar av-pg-services (av-pg-service-list av-pg-conf-file-path)
  "Memoizing the list of services in .pg_service.conf.")

(defun av-pg-setup-service (service)
  (av-add-service-to-connection-list service)
  (av-create-pg-service-connection-defun service))

(defun av-wireup-pg-stuff ()
  "For each service in `av-pg-services add a connection spec to
`sql-connection-alist' and create a helper function to
connect. It will also add the `av-sql-interactive-mode-hook' to
`sql-interactive-mode-hook'"

  (add-hook 'sql-interactive-mode-hook 'av-sql-interactive-mode-hook)
  (setq sql-postgres-login-params `((database :default ,(user-login-name))))
  (mapc #'av-pg-setup-service av-pg-services))

(defun av-pg-server-connect (service-name)
  "Interactive function presenting a list of of services from `av-pg-services' and connects to one selected.
Argument SERVICE-NAME adsfads."
  (interactive (list (ido-completing-read+ "Select server: " av-pg-services)))
  (funcall (av-pg-service-name->func-sym service-name)))


;;; Fire it up, hoss! Fire it up!
(require 'sql)
(provide 'av-psql)
;(av-wireup-pg-stuff)
