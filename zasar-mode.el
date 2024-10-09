;;; zAsar-mode.el --- Major mode for zAsar 65816 Assembly -*- lexical-binding: t; -*-

;; Author: scawful
;; URL: https://github.com/scawful/zAsar-mode
;; Version: 1.0
;; Keywords: languages, assembly, 65816

;;; Commentary:

;; This mode provides syntax highlighting, code navigation, and other
;; features for editing zAsar 65816 assembly code in Emacs.

(load-file "/Users/scawful/Code/lisp/zAsar-mode/zAsar-utils.el")
(load-file "/Users/scawful/Code/lisp/zAsar-mode/zAsar-parser.el")
(load-file "/Users/scawful/Code/lisp/zAsar-mode/zAsar-memory-map.el")
(load-file "/Users/scawful/Code/lisp/zAsar-mode/zAsar-symbols.el")
(load-file "/Users/scawful/Code/lisp/zAsar-mode/zAsar-navigation.el")
(load-file "/Users/scawful/Code/lisp/zAsar-mode/zAsar-keybindings.el")

;;; Code:
(require 'zAsar-utils)
(require 'zAsar-parser)
(require 'zAsar-memory-map)
(require 'zAsar-navigation)
(require 'zAsar-keybindings)
(require 'zAsar-symbols)
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
  (zAsar-parser-initialize)
  (zAsar-symbols-initialize)  
  (zAsar-keybindings-initialize)
  ; Hook to parse the current project if there are no labels
  (unless zAsar-project-labels
    (add-hook 'after-save-hook #'zAsar-parse-current-project nil t))
)

;; Auto-mode association
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\$begin:math:text$s\\\\|asm\\$end:math:text$\\'" . zAsar-mode))

(provide 'zAsar-mode)

;;; zAsar-mode.el ends here
