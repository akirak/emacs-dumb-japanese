;;; riben-sqlite.el --- Sqlite support -*- lexical-binding: t -*-

(declare-function emacsql-sqlite-default-connection "ext:emacsql-sqlite-common")
(declare-function emacsql-sqlite-connection-p "ext:emacsql-sqlite")
(declare-function emacsql-connection "ext:emacsql")

(defun riben-sqlite-open (file)
  (require 'emacsql-sqlite)
  (require 'emacsql-sqlite-common)
  (let ((db (make-instance (emacsql-sqlite-default-connection)
                           :file file)))
    (when (emacsql-sqlite-connection-p db)
      (set-process-query-on-exit-flag (emacsql-connection db) nil))
    db))

(provide 'riben-sqlite)
;;; riben-sqlite.el ends here
