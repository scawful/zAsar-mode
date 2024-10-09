;;; zAsar-mode.el --- Major mode for zAsar 65816 Assembly -*- lexical-binding: t; -*-

;; Author: scawful
;; URL: https://github.com/scawful/zAsar-mode
;; Version: 1.0
;; Keywords: languages, assembly, 65816

;;; Commentary:

;; This mode provides syntax highlighting, code navigation, and other
;; features for editing zAsar 65816 assembly code in Emacs.

;;; Code:

(require 'easymenu)


;; Customizable variables
(defgroup zAsar nil
  "Major mode for editing zAsar 65816 assembly code."
  :prefix "zAsar-"
  :group 'languages)

(defcustom zAsar-disassembly-directory "/Users/scawful/code/usdasm/"
  "Directory containing disassembly files."
  :type 'directory
  :group 'zAsar)

;; Keymap
(defvar zAsar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f12>") #'zAsar-jump-to-reference-at-point)
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
     :help "List all parsed references."]))

;; Imenu
(defvar zAsar-imenu-generic-expression
  '(("Labels" "^\\([A-Za-z_][A-Za-z0-9_]*\\):" 1))
    ; (nil "^\\([A-Za-z_][A-Za-z0-9_]*\\):" 1))
  "Imenu expression for `zAsar-mode'. Captures labels.")

;; Syntax table
(defvar zAsar-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments start with ';' and end with a newline
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Define word constituents
    (modify-syntax-entry ?_ "w" st)
    ;; Special characters
    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?@ "." st)
    st)
  "Syntax table for `zAsar-mode'.")

;; Font-lock keywords
(defvar zAsar-font-lock-keywords
  (let* ((instructions '("ADC" "AND" "ASL" "BCC" "BCS" "BEQ" "BIT" "BMI" "BNE" "BPL"
                         "BRA" "BRK" "BVC" "BVS" "CLC" "CLD" "CLI" "CLV" "CMP" "CPX"
                         "CPY" "DEC" "DEX" "DEY" "EOR" "INC" "INX" "INY" "JMP" "JSL"
                         "JSR" "JML" "LDA" "LDX" "LDY" "LSR" "NOP" "ORA" "PHA" "PHB"
                         "PHP" "PHX" "PLX" "PHY" "PLY" "BRL" "MVN" "TYX" "TXY"
                         "PHK" "PLB" "PLA" "PLP" "ROL" "ROR" "RTI" "RTS" "SBC" "SEC"
                         "SED" "SEI" "STA" "STX" "STY" "STZ" "TAX" "TAY" "TSX" "TXA"
                         "TXS" "TYA" "RTL" "REP" "SEP" "PEA" "PEI" "PER" "PHD" "PLD"
                         "TCD" "TDC" "TSC" "TSB" "TRB" "WAI" "XBA" "XCE"))
         (directives '("db" "dw" "dl" "incbin" "org" "include" "incsrc" "rept"
                       "endr" "macro" "namespace" "endmacro" "assert" "error" "warn"
                       "pushpc" "pullpc" "struct" "endstruct" "skip" "pc" "base" "off"
                       "print" "clean" ">" "if" "else" "endif" "elif"))
         (registers '("A" "X" "Y" "S" "SP" "PC"))
         (block-start-regex "{")
         (block-end-regex "}")
         (comment-regex ";.*$")
         (label-regex "\\([A-Za-z_][A-Za-z0-9_]*\\):")
         (label2-regex "\\!\\([A-Za-z_][A-Za-z0-9_]*\\)")
         (instruction-regex (regexp-opt instructions 'words))
         (directive-regex (regexp-opt directives 'words))
         (register-regex (regexp-opt registers 'words)))
    `(
      ;; Instructions
      (,instruction-regex . font-lock-keyword-face)
      ;; Directives
      (,directive-regex . font-lock-type-face)
      ;; Registers
      (,register-regex . font-lock-variable-name-face)
      ;; Labels
      (,label-regex 1 font-lock-function-name-face)
      ;; Blocks
      (,block-start-regex . font-lock-keyword-face)
      (,block-end-regex . font-lock-keyword-face)
      ;; Comments
      (,comment-regex . font-lock-comment-face)
      ; Labels with !
      (,label2-regex 1 font-lock-function-name-face)
      ;; Labels
      ("^\$begin:math:text$[A-Za-z_][A-Za-z0-9_]*\\$end:math:text$:" 1 font-lock-function-name-face)
      ("\\b\$begin:math:text$\\\\sw+\\$end:math:text$:" 1 font-lock-function-name-face)
      ;; Macros
      ("\\<%[A-Za-z_][A-Za-z0-9_]*\\>" . font-lock-function-name-face)
      ;; Registers with %
      ("%\\sw+" . font-lock-variable-name-face)
      ;; Numbers and constants
      ("\\$[0-9A-Fa-f]+" . font-lock-constant-face)  ; Hexadecimal
      ("\\#\\%[01]+" . font-lock-constant-face)      ; Binary
      ("\\@[0-7]+" . font-lock-constant-face)        ; Octal
      ("\\b[0-9]+\\b" . font-lock-constant-face)     ; Decimal
      ("\\#[A-Za-z0-9$%]+" . font-lock-string-face)  ; Constants with '#'
      ;; Strings
      ("\"[^\"]*\"" . font-lock-string-face)
      ;; Comments
      (";.*$" . font-lock-comment-face)))
  "Keyword highlighting specification for `zAsar-mode'.")

;; Variables to hold parsed labels
(defvar zAsar-labels nil
  "List of labels parsed from the disassembly files.")

(defvar zAsar-project-labels nil
  "List of labels parsed from the current project.")

;; Parsing functions
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

(defun zAsar-list-references ()
  "List all the references in the disassembly."
  (interactive)
  (if zAsar-labels
      (with-output-to-temp-buffer "*zAsar References*"
        (dolist (label zAsar-labels)
          (princ (format "%s\n" (car label)))))
    (message "No labels found. Did you run `zAsar-parse-disassembly`?")))

;; Jump functions
(defun zAsar-jump-to-reference-at-point ()
  "Jump to the definition of the label under point."
  (interactive)
  (let* ((label (thing-at-point 'symbol t))
         (file-pos (or (assoc-default label zAsar-labels)
                       (assoc-default label zAsar-project-labels)))
         (file (car file-pos))
         (position (cdr file-pos)))
    (if file
        (progn
          (find-file file)
          (goto-char position))
      (message "Label '%s' not found in disassembly or project." label))))

;; ========================================================


;; Define the mode
;;;###autoload
(define-derived-mode zAsar-mode prog-mode "zAsar"
  "Major mode for editing zAsar 65816 assembly code."
  :syntax-table zAsar-mode-syntax-table
  (setq-local font-lock-defaults '(zAsar-font-lock-keywords))
  (setq-local font-lock-keywords-case-fold-search t)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local imenu-generic-expression zAsar-imenu-generic-expression)
  ;; Optionally parse the disassembly when the mode is loaded
  (zAsar-parse-disassembly))

;; Auto-mode association
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\$begin:math:text$s\\\\|asm\\$end:math:text$\\'" . zAsar-mode))

(provide 'zAsar-mode)

;;; zAsar-mode.el ends here
