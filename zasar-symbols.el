;;; zAsar-symbols.el --- Symbol Handling for zAsar Mode -*- lexical-binding: t; -*-

(defvar zAsar-symbols nil
  "List of symbols parsed from symbol files.")

(defvar zAsar-symbol-files '("wram.asm" "sram.asm"))

(defun zAsar-parse-symbols ()
  "Parse symbols from predefined symbol files."
  (interactive)
  (setq zAsar-symbols nil)
  (dolist (sym-file zAsar-symbol-files)
    (let ((sym-path (expand-file-name sym-file zAsar-disassembly-directory)))
      (when (file-exists-p sym-path)
        (with-temp-buffer
          (insert-file-contents sym-path)
          (goto-char (point-min))
          (while (re-search-forward "^\\(!?\\)\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*=[ \t]*\\$\\([0-9A-Fa-f]+\\)" nil t)
            (let* ((symbol-name (match-string 2))
                   (address (string-to-number (match-string 3) 16)))
              (push (list :name symbol-name :address address) zAsar-symbols))))))))

(defun zAsar-setup-symbol-highlighting ()
  "Set up font-lock rules for symbol references."
  (font-lock-add-keywords nil
                          `((,(regexp-opt (mapcar #'(lambda (sym) (concat "\\<" (car sym) "\\>")) zAsar-symbols) 'symbols)
                             . font-lock-constant-face))))

;; List all the symbols in a separate buffer
(defun zAsar-list-symbols ()
  "List all the symbols in the disassembly."
  (interactive)
  (let ((buffer (get-buffer-create "*zAsar Symbols*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (propertize "Symbols\n" 'face 'font-lock-keyword-face))
      (insert (make-string 80 ?-))
      (insert "\n")
      (dolist (sym (sort zAsar-symbols
                         (lambda (a b)
                           (< (plist-get a :address)
                              (plist-get b :address))))
        (insert (propertize
                 (format "%-40s 0x%06X\n"
                         (plist-get sym :name)
                         (plist-get sym :address))
                 'face 'font-lock-constant-face)))
      (goto-char (point-min))
      (read-only-mode 1)))))

(defun zAsar-symbols-initialize ()
  "Initialize symbol parsing and highlighting."
  (zAsar-parse-symbols)
  (zAsar-setup-symbol-highlighting))

(provide 'zAsar-symbols)
;;; zAsar-symbols.el ends here