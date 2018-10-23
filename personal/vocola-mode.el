;;; vocola-mode.el -- Major mode for Vocola.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(if (featurep 'vocola-mode) (unload-feature 'vocola-mode))

(defvar vocola-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?` "w" table)
    (modify-syntax-entry ?\; "." table)
    ;; Don't notice strings in qoutes. We would like to maintain all the usual
    ;; syntax highlighting within the strings.
    (modify-syntax-entry ?\' "w" table)
    (modify-syntax-entry ?\" "w" table)
    table)
  "Syntax table for vocola-mode.")

;; (defvar vocola-mode-map
;;   (let ((keymap (make-sparse-keymap)))
;;     (define-key keymap (kbd "RET") 'reindent-then-newline-and-indent)
;;     keymap)
;;   "Keymap for vocola-mode.")

(defvar vocola-functions
  (list "ActiveControlPick" "ActiveMenuPick" "AppBringUp" "AppSwapWith" "Beep"
        "ButtonClick" "ClearDesktop" "ControlPick" "DdeExecute" "DllCall" "DdePoke"
        "DragToPoint" "Eval" "EvalTemplate" "GoToSleep" "If" "HeardWord" "HTMLHelp"
        "MenuCancel" "MenuPick" "PlaySound" "MouseGrid" "MsgBoxConfirm" "RememberPoint"
        "Repeat" "RunScriptFile" "SendDragonKeys" "SendKeys" "SendSystemKeys"
        "SetMicrophone" "ShiftKey" "SetMousePosition" "SetNaturalText" "ShellExecute"
        "TTSPlayString" "Unimacro" "Wait" "WaitForWindow" "WakeUp" "WinHelp" "When")
  "List of Vocola built-in functions for vocola-mode.")

(defvar vocola-keywords
  (list "include")
  "List of Vocola keywords for vocola-mode.")

(defun vocola-regexp-opt (keywords)
  "Make an optimized regexp from the list of KEYWORDS."
  (regexp-opt keywords 'symbols))

(defvar vocola-font-locks
  `(( ,(vocola-regexp-opt vocola-keywords) . font-lock-builtin-face)
    ( ,(vocola-regexp-opt vocola-functions) . font-lock-builtin-face)
    ("\$[a-zA-Z0-9_]+" . font-lock-variable-name-face)
    ("<[^>]+>" . font-lock-variable-name-face)
    ("[A-Za-z.]*(" . font-lock-builtin-face)
    ("[|;\"']" . font-lock-keyword-face)
    ))

(defvar vocola-indent 2
  "Indentation size for vocola-mode.")

(define-derived-mode vocola-mode prog-mode "vocola"
  "A major mode for editing Vocola files."
  :syntax-table vocola-mode-syntax-table
  (setq-local font-lock-defaults '(vocola-font-locks nil t))
  (setq-local indent-line-function 'vocola-indent-line)
  (if (featurep 'rainbow-delimiters) (rainbow-delimiters-mode-enable)))

(add-to-list 'auto-mode-alist '("\\.vcl\\'" . vocola-mode))
(add-to-list 'auto-mode-alist '("\\.vch\\'" . vocola-mode))

(provide 'vocola-mode)
;;; vocola-mode.el ends here
