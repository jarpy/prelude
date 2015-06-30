;;; jarpy.el -- Jarpy's Emacs config

;;; Commentary:
;; Jarpy's Emacs (Prelude) config.
;; https://github.com/jarpy/prelude

;;; Code:

;; General Behaviours
(setq prelude-guru nil)
(setq read-file-name-completion-ignore-case 1)
(setq mouse-yank-at-point t)
(global-unset-key "\C-x\C-c")
(global-set-key (kbd "M-p") 'ace-window)


(defun indent-buffer ()
  (interactive)
  (indent-region 0 (buffer-size))
  (whitespace-cleanup)
  )

;; Auto save
(prelude-require-package 'auto-save-buffers-enhanced)
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced-include-only-checkout-path t)
(auto-save-buffers-enhanced t)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-theme-directory "~/.emacs.d/themes")
(prelude-require-package 'solarized-theme)
(load-theme 'solarized-dark t)
(prelude-require-package 'xterm-color)
(require 'xterm-color)
(set-face-attribute 'default () :family "Bitstream Vera Sans Mono" :height 110 :background "#0C1A1F")
(set-face-attribute 'mode-line () :height 80 :background "#0C1F1F")
(set-face-attribute 'mode-line-inactive () :height 80 :background "#0C1F1F")

(set-face-attribute 'cursor () :background "#0f0")
(add-hook 'minibuffer-setup-hook (lambda ()
  (setq-local face-remapping-alist '((default :height 100)))))
(with-current-buffer (get-buffer " *Echo Area 0*")
  (setq-local face-remapping-alist '((default :height 100))))
(with-current-buffer (get-buffer " *Echo Area 1*")
  (setq-local face-remapping-alist '((default :height 100))))

(add-hook
 'window-configuration-change-hook
; FIXME: No lambda kebabs
 (lambda ()
   (set-frame-parameter (selected-frame) 'right-divider-width 1)
   (set-frame-parameter (selected-frame) 'left-fringe 3)
   (set-frame-parameter (selected-frame) 'right-fringe 3)))

;; comint install
(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions(
          remove 'ansi-color-process-output comint-output-filter-functions))
       (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

(add-hook 'term-mode-hook
          (lambda()
            (set-face-attribute 'term () :family "Bitstream Vera Sans Mono" :background "#0C1A1F")))

(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)


;; Look and Feel
;(global-rainbow-delimiters-mode)
(scroll-bar-mode -1)
(global-hl-line-mode -1)
(electric-indent-mode -1)
(global-prettify-symbols-mode)
(custom-set-variables '(speedbar-show-unknown-files t))
(defun set-prelude-prog-mode-defaults ()
  (turn-off-flyspell)
  (diff-hl-mode -1)
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("function" . 402)
          ("defun" . 402))))
(add-hook 'prelude-prog-mode-hook 'set-prelude-prog-mode-defaults t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; Key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(key-chord-define-global "jj" nil)
(key-chord-define-global "jk" nil)
(key-chord-define-global "jl" nil)
(key-chord-define-global "JJ" nil)
(key-chord-define-global "uu" nil)
(key-chord-define-global "xx" nil)
(key-chord-define-global "yy" nil)
(key-chord-define-global "jw" 'ace-jump-word-mode)
(key-chord-define-global "jc" 'ace-jump-char-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
;; (key-chord-define-global "GG" 'find-file-at-point)
(key-chord-define-global "FF" 'ido-find-file-other-window)
;; (key-chord-define-global "BB" 'ace-jump-buffer-other-window)
(key-chord-define-global "fj" 'hippie-expand)
;; (key-chord-define-global "aa" 'prelude-move-beginning-of-line)
(key-chord-define-global "YY" 'prelude-duplicate-current-line-or-region)
(key-chord-define-global "DD" 'prelude-kill-whole-line)
;; (key-chord-define-global "kk" 'easy-kill)
;; (key-chord-define-global "jk" 'sp-kill-hybrid-sexp)
;; (key-chord-define-global "yy" 'yank)
;; (key-chord-define-global "WW" 'kill-region)
(key-chord-define-global "XX" 'delete-window)
;; (key-chord-define-global "MM" 'delete-other-windows)
(key-chord-define-global "jb" 'ido-switch-buffer)

;; (setq ace-jump-mode-end-hook 'recenter)
;; Use Firefox as the default browser
(setq browse-url-browser-function 'browse-url-firefox)

;; whitespace-mode
(setq prelude-clean-whitespace-on-save nil)
(setq whitespace-line-column 120)

;; Ruby
(prelude-require-package 'rubocop)
(require 'rubocop)

;; Puppet
(defun puppet-lint-fix ()
  "Run the current buffer's file through 'puppet-lint --fix'."
  (interactive)
  (puppet-run-check-command (concat "puppet-lint --fix " (buffer-file-name)) "*puppet-lint*")
  )

;; Gherkin
(prelude-require-package 'feature-mode)

;; Javascript
(flycheck-define-checker javascript-jslint-reporter
  "A JavaScript syntax and style checker based on JSLint Reporter.
  See URL https://github.com/FND/jslint-reporter"

  :command ("~/.emacs.d/personal/jslint-reporter" source)
  :error-patterns
  ((error line-start (1+ nonl) ":" line ":" column ":" (message) line-end))
  :modes (js-mode js2-mode js3-mode))
(add-hook 'js-mode-hook (lambda ()
                          (flycheck-select-checker 'javascript-jslint-reporter)
                          (flycheck-mode)))
;; Mediawiki
(prelude-require-package 'mediawiki)

;; "ERC" IRC client
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Auto-complete
(prelude-require-package 'auto-complete)
(prelude-require-package 'ac-etags)
(require 'auto-complete)
(global-auto-complete-mode t)

;; Python
(prelude-require-package 'jedi)
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode."
  (if (equal major-mode 'python-mode) `no-indent' nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

(defun set-python-mode-defaults ()
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-python-mode-defaults)
(add-hook 'python-mode-hook
          (lambda ()
            (push '("self" . ?自) prettify-symbols-alist)
            (modify-syntax-entry ?. ".")))
(setq flycheck-flake8-maximum-line-length 120)

;; iPython as python-shell
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; iPython Notebook Support
(prelude-require-package 'ein)
(require 'ein)

;; Org-mode
(setq org-export-html-postamble nil)
(setq org-latex-tables-centered nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-agenda-files "~/Dropbox/org/agenda-files")
(global-set-key (kbd "C-c =")
                (lambda () (interactive) (find-file-other-window "~/Dropbox/org/index.org")))
(setq org-directory "~/Dropbox/org")

;(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
;(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-todo-keywords
      '((sequence "TODO" "CURRENT" "|" "DONE")))
;; Shell
(setq sh-indentation 2)

;; Make windmove work in Org-mode
(setq org-replace-disputed-keys t)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Javascript
(defun jarpy-prettify-json ()
  (mark-whole-buffer))

;; Turn on some languages for Org/Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (ruby . t)
   ))


(provide 'jarpy)
;;; jarpy.el ends here
