;;; zAsar-parser.el --- Parsing for zAsar Mode -*- lexical-binding: t; -*-

; Parses the disassembly and current project for labels and symbols

(defvar zAsar-labels nil
  "List of labels parsed from the disassembly files.")

(defvar zAsar-project-labels nil
  "List of labels parsed from the current project.")

(defun zAsar-parser-initialize ()
  "Initialize the parser by parsing disassembly and project files."
  (zAsar-parse-disassembly)
  (zAsar-parse-current-project))

;; Include parsing functions here
(defun zAsar-parse-disassembly ()
  "Parse the disassembly files in `zAsar-disassembly-directory'."
  (interactive)
  (setq zAsar-labels nil)
  (let ((disassembly-directory zAsar-disassembly-directory))
    (when (file-directory-p disassembly-directory)
      (dolist (file (directory-files disassembly-directory t "bank_[0-9A-F][0-9A-F]\\.asm"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^\$begin:math:text$[A-Za-z_][A-Za-z0-9_]*\\$end:math:text$:" nil t)
            (add-to-list 'zAsar-labels
                         (cons (match-string 1) (cons file (match-beginning 0))))))))))

(defun zAsar-parse-current-project ()
  "Parse the current project for labels."
  (interactive)
  (setq zAsar-project-labels nil)
  (let ((project-directory (locate-dominating-file default-directory ".git")))
    (when project-directory
      (dolist (file (directory-files-recursively project-directory "\\.asm\\'"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^\$begin:math:text$[A-Za-z_][A-Za-z0-9_]*\\$end:math:text$\\s-*\$begin:math:text$:\\\\|=\\$end:math:text$" nil t)
            (add-to-list 'zAsar-project-labels
                         (cons (match-string 1) (cons file (match-beginning 0))))))))))

(provide 'zAsar-parser)
;;; zAsar-parser.el ends here