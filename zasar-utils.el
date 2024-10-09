;;; zAsar-utils.el --- Utility Functions for zAsar Mode -*- lexical-binding: t; -*-

(defun zAsar-snes-to-pc (snes-address)
  "Convert SNES_ADDRESS to PC address.
Placeholder implementation—replace with actual conversion logic."
  (- snes-address 0x8000))

(defun zAsar-pc-to-snes (pc-address)
  "Convert PC_ADDRESS to SNES address.
Placeholder implementation—replace with actual conversion logic."
  (+ pc-address 0x8000))

(defun zAsar-address-to-bank (address)
  "Determine bank number from ADDRESS.
Placeholder logic—replace with actual bank determination."
  (floor (/ address 0x10000)))

(defun zAsar-extract-pool-function-name (pool-directive)
  "Extract the function name from a POOL-DIRECTIVE."
  (if (string-match "pool \\([A-Za-z_][A-Za-z0-9_]*\\)" pool-directive)
      (match-string 1 pool-directive)
    ""))

(provide 'zAsar-utils)
;;; zAsar-utils.el ends here