;;; zAsar-keybindings.el --- Keybindings and Menu Setup for zAsar Mode -*- lexical-binding: t; -*-

(require 'easymenu)
(require 'zAsar-navigation)
(require 'zAsar-memory-map)
(require 'zAsar-symbols)

;; Keymap
(defvar zAsar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f12>") #'zAsar-jump-to-reference-at-point)
    (define-key map (kbd "C-c C-m") #'zAsar-display-memory-map)
    (define-key map (kbd "C-c C-h") #'zAsar-open-hex-at-label)
    map)
  "Keymap for `zAsar-mode'.")

;; Menu
(easy-menu-define zAsar-mode-menu zAsar-mode-map
  "Menu for `zAsar-mode'."
  '("zAsar"
    ["Jump to Reference" zAsar-jump-to-reference-at-point
     :help "Jump to reference in disassembly or project."]
    ["Parse Disassembly" zAsar-parse-disassembly
     :help "Parse labels from the disassembly directory."]
    ["Parse Current Project" zAsar-parse-current-project
     :help "Parse labels from the current project."]
    ["List References" zAsar-list-references
     :help "List all parsed references."]
    ["View Memory Map" zAsar-display-memory-map
      :help "Display the ROM memory map"]
    ["Open Hex at Label" zAsar-open-hex-at-label
      :help "Open hex editor at the address of the label under point"]))

(defun zAsar-setup-menu ()
  "Set up the zAsar menu."
  (easy-menu-define zAsar-menu zAsar-mode-map "zAsar Menu" zAsar-mode-menu)
  (easy-menu-add zAsar-menu zAsar-mode-map))

(defun zAsar-keybindings-initialize ()
  "Initialize keybindings and menus for zAsar-mode."
  (zAsar-setup-menu))

(provide 'zAsar-keybindings)
;;; zAsar-keybindings.el ends here