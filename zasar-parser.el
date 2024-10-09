;;; zAsar-parser.el --- Parsing for zAsar Mode -*- lexical-binding: t; -*-

; Parses the disassembly and current project for labels and symbols

(defvar zAsar-labels nil
  "List of labels parsed from the disassembly files.")

(defvar zAsar-project-labels nil
  "List of labels parsed from the current project.")

(defun zAsar-push-label (label-name address file type)
  "Push a label to the label list."
  (push (list 
          :name label-name 
          :address address 
          :file file 
          :position (point)
          :type type) zAsar-labels))

(defun zAsar-push-project-label (label-name address file type)
  "Push a label to the project label list."
  (push (list 
          :name label-name 
          :address address 
          :file file 
          :position (point)
          :project t
          :type type) zAsar-project-labels))

(defun zAsar-parse-disassembly ()
  "Parse disassembly files for labels and symbols."
  (setq zAsar-labels nil)
  (interactive)
  (dolist (file (directory-files zAsar-disassembly-directory t "bank_[0-9A-F][0-9A-F]\\.asm"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((current-bank (string-to-number (substring (file-name-base file) 5) 16))) ; Extract bank from filename
        ;; Parse labels
        ;; Get the address from the line below the label
        (while (re-search-forward "^\\([A-Za-z_][A-Za-z0-9_]*\\):" nil t)
          (let ((label-name (match-string 1)))
            (when (re-search-forward "^\\s-*#_\\([0-9A-Fa-f]+\\):" (line-end-position 2) t)
              (let ((address (string-to-number (match-string 1) 16)))
              (zAsar-push-label label-name address file 'main)))))
        ;; Parse org directives
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*org\\s-+\\$\\([0-9A-Fa-f]+\\)" nil t)
          (let ((address (string-to-number (match-string 1) 16)))
            (push (list :address address :bank current-bank :file file :type 'org) zAsar-labels)))))
    (message "Parsed %d labels from %s" (length zAsar-labels) file)))

(defun zAsar-parse-current-project ()
  "Parse current project files for labels and symbols."
  (interactive)
  (setq zAsar-project-labels nil)
  (let ((project-directory (projectile-project-root)))
    (when project-directory
      (dolist (file (projectile-current-project-files))
        (when (string-match-p "\\.asm\\'" file)
          (with-temp-buffer
            (insert-file-contents (expand-file-name file project-directory))
            (goto-char (point-min))
            ;; Parse labels
            ;; Project labels typically don't have a #_XXXXX address
            ;; so we just add the label name and file and set the address to 0
            (while (re-search-forward "^\\([A-Za-z_][A-Za-z0-9_]*\\):" nil t)
              (let ((label-name (match-string 1)))
                (zAsar-push-project-label label-name 0 file 'project)))
            ;; Parse org directives
            (goto-char (point-min))
            (while (re-search-forward "^\\s-*org\\s-+\\$\\([0-9A-Fa-f]+\\)" nil t)
              (let ((address (string-to-number (match-string 1) 16)))
                (push (list :address address :file file :type 'org) zAsar-project-labels))))))
      (message "Parsed %d labels from project files" (length zAsar-project-labels)))))

(defun zAsar-list-references ()
  "List all the references in the disassembly."
  (interactive)
  (if zAsar-labels
      (with-output-to-temp-buffer "*zAsar References*"
        (dolist (label zAsar-labels)
          (let ((name (plist-get label :name))
                (address (plist-get label :address))
                (bank (plist-get label :bank))
                (file (plist-get label :file))
                (type (plist-get label :type)))
            (princ (format "%s, Address: %s, Bank: %s, Type: %s\n"
                           name
                           (if address (format "$%04X" address) "N/A")
                           (if bank (format "%02X" bank) "N/A")
                           type)))))
    (message "No labels found. Did you run `zAsar-parse-disassembly`?")))

(defun zAsar-parser-initialize ()
  "Initialize the parser by parsing disassembly and project files."
  (zAsar-parse-disassembly)
  (zAsar-parse-current-project))

(provide 'zAsar-parser)

;;; zAsar-parser.el ends here