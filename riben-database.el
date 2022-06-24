;;; riben-database.el --- Database backend -*- lexical-binding: t -*-

(require 'emacsql)
(require 'emacsql-sqlite)

(defgroup riben-database nil
  "Database support for riben."
  :group 'riben)

(defcustom riben-database-backend
  (if (and (version< "29" emacs-version)
           (require 'emacsql-sqlite-builtin nil t))
      'emacsql-sqlite-builtin
    'emacsql-sqlite)
  "Whether to use the builtin SQLite support."
  :type '(choice (const emacsql-sqlite-builtin)
                 (const emacsql-sqlite)
                 (const emacsql-sqlite3)))

(provide 'riben-database)
;;; riben-database.el ends here
