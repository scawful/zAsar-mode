;;; zAsar-navigation.el --- Navigation and Imenu Integration for zAsar Mode -*- lexical-binding: t; -*-

(require 'zAsar-parser)
(require 'zAsar-symbols)
(require 'zAsar-utils)

(defun zAsar-jump-to-reference-at-point ()
  "Jump to the definition of the label or symbol under point."
  (interactive)
  (let* ((label (thing-at-point 'symbol t))
         (clean-label (zAsar-clean-label label))
         (label-entry (or (zAsar-find-label clean-label zAsar-labels)
                          (zAsar-find-label clean-label zAsar-project-labels)
                          (zAsar-find-symbol-by-address (zAsar-label-address-from-code)))))
    (if label-entry
        (progn
          (find-file (plist-get label-entry :file))
          (goto-char (plist-get label-entry :position)))
      (message "Label or symbol '%s' not found." label))))

(defun zAsar-clean-label (label)
  "Clean the LABEL by substituting dots with underscores if necessary."
  (if (and label (string-prefix-p "." label))
      (replace-regexp-in-string "^\\." "_" label)
    label))

(defun zAsar-find-label (label-name label-list)
  "Find a label by LABEL-NAME in LABEL-LIST."
  (cl-find-if (lambda (lbl)
               (string= (plist-get lbl :name) label-name))
             label-list))

(defun zAsar-find-symbol-by-address (address)
  "Find a symbol by ADDRESS in `zAsar-symbols`."
  (cl-find-if (lambda (sym)
               (= (plist-get sym :address) address))
             zAsar-symbols))

(defun zAsar-label-address-from-code ()
  "Extract address from code under point, e.g., #$7E000A."
  (save-excursion
    (when (re-search-backward "#\\$\$begin:math:text$[0-9A-Fa-f]+\\$end:math:text$" (line-beginning-position) t)
      (string-to-number (match-string 1) 16))))

(defun zAsar-imenu-create-index ()
  "Create an imenu index for zAsar-mode with hierarchical entries."
  (let ((index '()))
    ;; Main Labels
    (dolist (label (cl-remove-if-not (lambda (lbl)
                                       (eq (plist-get lbl :type) 'main))
                                     zAsar-labels))
      (push (cons (plist-get label :name) (plist-get label :position)) index))
    ;; Pools with their associated functions
    (dolist (pool (cl-remove-if-not (lambda (lbl)
                                      (eq (plist-get lbl :type) 'pool))
                                    zAsar-labels))
      (let ((pool-name (zAsar-extract-pool-function-name (plist-get pool :name))))
        (let ((functions (cl-remove-if-not (lambda (lbl)
                                            (and (eq (plist-get lbl :type) 'main)
                                                 (string= (plist-get lbl :name) pool-name)))
                                          zAsar-labels)))
          (when functions
            (push (cons (plist-get pool :name)
                        (mapcar (lambda (f) (cons (plist-get f :name) (plist-get f :position)))
                                functions))
                  index))))))
    index)

(provide 'zAsar-navigation)
;;; zAsar-navigation.el ends here