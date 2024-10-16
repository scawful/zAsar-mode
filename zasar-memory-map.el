;;; zAsar-memory-map.el --- Memory Map Visualization for zAsar Mode -*- lexical-binding: t; -*-

(require 'zAsar-parser)
(require 'zAsar-utils)

(defvar zAsar-memory-map-data nil "Data for memory map visualization.")

;; zAsar Memory Map Mode 
(define-derived-mode zAsar-memory-map-mode special-mode "zAsar Memory Map"
  "Major mode for displaying the memory map of a ROM."
  (setq buffer-read-only t))

(defun zAsar-generate-memory-map ()
  "Generate the memory map data based on parsed labels."
  (interactive)
  (setq zAsar-memory-map-data nil)
  (dolist (label zAsar-labels)
      (push (list :name    (plist-get label :name)
                  :address (plist-get label :address)
                  :position   (plist-get label :position)
                  :file    (plist-get label :file)
                  :type    (plist-get label :type))
            zAsar-memory-map-data))
  (dolist (label zAsar-project-labels)
      (push (list :name    (plist-get label :name)
                  :address (plist-get label :address)
                  :position   (plist-get label :position)
                  :file    (plist-get label :file)
                  :type    (plist-get label :type))
            zAsar-memory-map-data)))
(defun zAsar-display-memory-map ()
  "Display the ROM memory map in a separate buffer with a fixed header."
  (interactive)
  (zAsar-generate-memory-map)
  (let ((buffer (get-buffer-create "*zAsar Memory Map*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      ;; Insert Header
      (insert (propertize (format "%-7s %-40s %-20s %s\n"
                                  "Address" "Label" "File" "Type")
                          'face 'font-lock-keyword-face))
      (insert (make-string 80 ?-))
      (insert "\n")
      ;; Insert Entries
      (dolist (entry (sort zAsar-memory-map-data
                            (lambda (a b)
                              (< (plist-get a :address)
                                (plist-get b :address)))))
        (let ((face (cond
                      ((eq (plist-get entry :type) 'main) 'font-lock-function-name-face)
                      ((eq (plist-get entry :type) 'sublabel) 'font-lock-variable-name-face)
                      (t 'font-lock-constant-face))))
          (insert (propertize
                    (format "$%06X %-40s %-20s %s\n"
                            (plist-get entry :address)
                            (plist-get entry :name)
                            (file-name-nondirectory (plist-get entry :file))
                            (symbol-name (plist-get entry :type)))
                    'face face))))
      (goto-char (point-min))
      (read-only-mode 1)
      ;; Make header fixed
      (setq header-line-format
            (propertize (format "%-8s %-40s %-20s %s"
                                        "Address" "Label" "File" "Type")
                                'face 'font-lock-keyword-face)
                    ))
    (display-buffer buffer)))

(defun zAsar-group-consecutive (numbers)
  "Group a list of NUMBERS into consecutive ranges.
Returns a list of cons cells (START . END)."
  (let ((result '())
        (current-start nil)
        (current-end nil))
    (dolist (n (sort numbers #'<))
      (cond
       ((null current-start)
        (setq current-start n)
        (setq current-end n))
       ((= n (1+ current-end))
        (setq current-end n))
       (t
        (push (cons current-start current-end) result)
        (setq current-start n)
        (setq current-end n))))
    (when current-start
      (push (cons current-start current-end) result))
    (nreverse result)))

(provide 'zAsar-memory-map)
;;; zAsar-memory-map.el ends here