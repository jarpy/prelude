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
(setq exec-path (append exec-path '("~/go/bin" "/usr/local/go/bin/go")))
(setenv "PATH" (concat (getenv "PATH") "~/go/bin" "/usr/local/go/bin/go"))
(setq-default fill-column 80)
(setq create-lockfiles nil)
(setq frame-title-format "Emacs visiting %b in %m mode")
(prelude-require-package 'window-purpose)

;; I love undo-tree, but it's bugged.
(global-undo-tree-mode -1)

;; Purpose
(prelude-require-package 'window-purpose)
(purpose-mode)
(setq purpose-user-mode-purposes
      '((clojure-mode . source)
        (cider-repl-mode . repl)
        (cider-stacktrace-mode . compilation)
        (compilation-mode . compilation)
        (term-mode . terminal)
        ))
(setq purpose-user-name-purposes
      '(("*buffer-selection*" . source)
        ))
(purpose-compile-user-configuration)
(add-hook 'compilation-mode-hook (lambda () (text-scale-decrease 2)))
(global-set-key (kbd "C-x f") (without-purpose-command #'ido-find-file))


;; Projectile
(add-to-list 'projectile-project-root-files "Makefile" "Chart.yaml")
(setq projectile-use-git-grep nil)

(defun indent-buffer ()
  (interactive)
  (indent-region 0 (buffer-size))
  (whitespace-cleanup))

;; Auto save
(prelude-require-package 'auto-save-buffers-enhanced)
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced-include-only-checkout-path t)
(auto-save-buffers-enhanced t)

;; Theme
(prelude-require-package 'solarized-theme)
(prelude-require-package 'afternoon-theme)
(prelude-require-package 'xterm-color)
(load-theme 'sanityinc-tomorrow-night)

(require 'xterm-color)

(setq jarpy-font-size 180)
(set-face-attribute 'default ()
                    :family "Bitstream Vera Sans Mono"
                    :height jarpy-font-size
                    :foreground "#d0d0d0"
                    :background "#202020")
;;(set-face-attribute 'mode-line () :background "#262626")
;;(set-face-attribute 'mode-line-inactive () :background "#101010")
;;(set-face-attribute 'minibuffer-prompt () :background "#262626")
;;(set-face-attribute 'flyspell-incorrect () :background "#803330")
(set-face-attribute 'cursor () :background "#0f0" :foreground "#202020")
(set-face-attribute 'region () :background "#070" :foreground nil)
(set-face-attribute 'whitespace-trailing () :background nil :foreground "#c36")
(set-face-attribute 'whitespace-line () :background "#c36" :foreground nil)
(set-face-attribute 'font-lock-comment-face () :foreground "#a8a8a8")


(add-hook
 'window-configuration-change-hook
 (lambda ()
   (set-frame-parameter (selected-frame) 'right-divider-width 1)
   (set-frame-parameter (selected-frame) 'left-fringe 5)
   (set-frame-parameter (selected-frame) 'right-fringe 5)))

;; comint install
(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq
        comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
       (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region))

(add-hook 'term-mode-hook
          (lambda()
            (set-face-attribute 'term ()
                                :family "Bitstream Vera Sans Mono"
                                :background "#202020")
            (set-face-attribute 'term-color-black ()
                                :foreground "#d0d0d0"
                                :background "#202020")
            (setq term-default-bg-color "#202020")))

(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)

;; Look and Feel
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'yaml-mode-hook 'rainbow-delimiters-mode)
(add-hook 'yaml-mode-hook 'smartparens-mode)
(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
(scroll-bar-mode -1)
(global-hl-line-mode -1)
(electric-indent-mode -1)
(custom-set-variables '(speedbar-show-unknown-files t))

;; Glyph all the things!
(global-prettify-symbols-mode)
(prelude-require-package 'company-emoji)
(add-to-list 'company-backends 'company-emoji)
(defun --set-emoji-font (frame)
  (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend))
(add-hook 'after-make-frame-functions '--set-emoji-font)

(defun set-prelude-prog-mode-defaults ()
  (turn-off-flyspell)
  ;(diff-hl-mode -1)
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("function" . 402)
          ("defun" . 402))))
(add-hook 'prelude-prog-mode-hook 'set-prelude-prog-mode-defaults t)

;; Beacon
(setq beacon-blink-duration 0.6)
(setq beacon-color "#0f0")

(defun jarpy-kill-current-buffer ()
  (interactive)
  (kill-buffer nil))

(defun jarpy-jump-term ()
  (interactive)
  (switch-to-buffer (get-buffer "*ansi-term*")))

;; Key bindings
(prelude-require-package 'ace-jump-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [f5] 'compile)

(global-set-key (kbd "C-<backspace>") 'sp-backward-kill-sexp)
(global-set-key (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(global-set-key (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
(global-set-key (kbd "C-M-\\") 'sp-split-sexp)
(key-chord-define-global "^^" 'sp-splice-sexp)

(key-chord-define-global "jj" nil)
;; (key-chord-define-global "jk" nil)
;; (key-chord-define-global "jl" nil)
(key-chord-define-global
 "JJ"
 (without-purpose-command
   #'crux-switch-to-previous-buffer))

(key-chord-define-global "jt" 'jarpy-jump-term)
(key-chord-define-global "uu" nil)
(key-chord-define-global "xx" nil)
(key-chord-define-global "yy" nil)
(key-chord-define-global "jw" 'avy-goto-word-1)
;; (key-chord-define-global "jk" 'avy-goto-char)
;; (key-chord-define-global "jl" 'avy-goto-line)
;; (key-chord-define-global "GG" 'find-file-at-point)
;;(key-chord-define-global "FF" 'ido-find-file-other-window)

(key-chord-define-global "FF" (without-purpose-command #'ido-find-file))


;; (key-chord-define-global "BB" 'ace-jump-buffer-other-window)
(key-chord-define-global "fj" 'hippie-expand)
;; (key-chord-define-global "aa" 'prelude-move-beginning-of-line)
(key-chord-define-global "YY" 'crux-duplicate-current-line-or-region)
(key-chord-define-global "DD" 'kill-whole-line)
;; (key-chord-define-global "jz" 'ace-jump-zap-to-char)
;; (key-chord-define-global "jk" 'sp-kill-hybrid-sexp)
;; (key-chord-define-global "yy" 'yank)
;; (key-chord-define-global "WWX" 'kill-region)
(key-chord-define-global "WW" 'fill-region)
(key-chord-define-global "XX" 'kill-buffer-and-window)
(key-chord-define-global "ZZ" 'jarpy-kill-current-buffer)
;; (key-chord-define-global "MM" 'delete-other-windows)
;;(key-chord-define-global "jb" 'ace-jump-buffer)
(key-chord-define-global "jb" (without-purpose-command #'ace-jump-projectile-buffers))

;; Use Firefox as the default browser
(setq browse-url-browser-function 'browse-url-firefox)

;; whitespace-mode
(setq prelude-clean-whitespace-on-save nil)
;;(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 80)

;; highlight-indent-guides-mode
(setq highlight-indent-guides-auto-enabled nil)
;(set-face-background 'highlight-indent-guides-odd-face "#202020")
;(set-face-background 'highlight-indent-guides-even-face "#272727")

;; smartparens
(set-face-attribute 'sp-show-pair-match-content-face()
                    :weight 'bold
                    :background "#323"
                    :slant 'italic)

(set-face-attribute 'sp-show-pair-match-face()
                    :weight 'normal
                    :background "#d3f"
                    :foreground "#202020")

(set-face-attribute 'sp-show-pair-mismatch-face()
                    :weight 'normal
                    :background "#a33"
                    :foreground "#202020")

;; Face for parens getting slurped and barfed.
(set-face-attribute 'sp-show-pair-enclosing()
                    :weight 'bold
                    :slant 'italic
                    :background "#f76"
                    :foreground "#202020")


;; Git
(setq git-commit-fill-column 72)

;; Go
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq auto-save-buffers-enhanced-interval 3)))

;; Ruby
(prelude-require-package 'rubocop)
(require 'rubocop)

;; Yaml
(prelude-require-package 'highlight-indent-guides)
(add-hook 'yaml-mode-hook
          (lambda ()
            (highlight-indent-guides-mode t)))

;; Puppet
(defun puppet-lint-fix ()
  "Run the current buffer's file through 'puppet-lint --fix'."
  (interactive)
  (puppet-run-check-command (concat "puppet-lint --fix " (buffer-file-name)) "*puppet-lint*"))

(setq puppet-validate-command "puppet parser validate --color=false")
(add-hook 'puppet-mode-hook (lambda ()
                              (setq puppet-validate-command "puppet parser validate --color=false")))

(setq puppet-lint-command
      "puppet-lint --no-autoloader_layout-check --with-context --log-format \"%{path}:%{linenumber}: %{kind}: %{message} (%{check})\"")

(add-to-list 'flycheck-puppet-lint-disabled-checks "autoloader_layout")
(add-to-list 'flycheck-puppet-lint-disabled-checks "variable_scope")

;; Gherkin
(prelude-require-package 'feature-mode)

;; Javascript
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(setq js-indent-level 2)

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)



;; Mediawiki
(prelude-require-package 'mediawiki)

;; "ERC" IRC client
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Auto-complete
(prelude-require-package 'auto-complete)
(prelude-require-package 'ac-etags)
(require 'auto-complete)
(global-auto-complete-mode t)
(setq tab-always-indent 'complete)

;; Python
(prelude-require-package 'jedi)
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode."
  (if (equal major-mode 'python-mode) `no-indent' nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

(defun set-python-mode-defaults ()
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
;;  (flycheck-add-next-checker 'python-flake8 'python-mypy)
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-python-mode-defaults)
(add-hook 'python-mode-hook
          (lambda ()
            (push '("self" . ?自) prettify-symbols-alist)
            (modify-syntax-entry ?. ".")))
(setq flycheck-flake8-maximum-line-length 80)

;; iPython as python-shell
(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args "--simple-prompt"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
 flycheck-python-pycompile-executable "python3"
 flycheck-python-pylint-executable "python3"
 flycheck-python-flake8-executable "python3")

;; iPython Notebook Support
(prelude-require-package 'ein)
(require 'ein)

;; Org-mode
(setq org-export-html-postamble nil)
(setq org-latex-tables-centered nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(global-set-key (kbd "C-c =")
                (lambda () (interactive) (find-file-other-window "~/Dropbox/org/index.org")))

;; Shell
(setq sh-indentation 2)
;; Highlights variables inside strings
(font-lock-add-keywords
 'sh-mode '(("\\(\\${.*?}\\)" 1 'font-lock-variable-name-face prepend)))

;; Make windmove work in Org-mode
(setq org-replace-disputed-keys t)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Elasticsearch
(prelude-require-package 'es-mode)
(defun jarpy-prettify-es-response (status content-type buffer)
  (if (string-prefix-p "application/json" content-type)
      (progn
        (json-mode)
        (json-pretty-print (point-min) (point-max)))))

(add-hook 'es-mode-hook
          (lambda()
            (add-to-list
             'es-response-success-functions 'jarpy-prettify-es-response)
            (add-to-list
             'es-response-failure-functions 'jarpy-prettify-es-response)))

(setq es-response-success-functions ())

;; Perl
(fset 'perl-mode 'cperl-mode)
(add-hook
 'cperl-mode-hook
 (lambda()
   (setq cperl-close-paren-offset -4
         cperl-continued-statement-offset 0
         cperl-indent-level 4
         cperl-indent-parens-as-block t
         cperl-tabs-always-indent t)))

;; Terraform
(prelude-require-package 'terraform-mode)
(defun jarpy-terraform-mode-before-save-hook ()
  (when (eq major-mode 'terraform-mode)
    (terraform-format-buffer)))
(add-hook 'before-save-hook 'jarpy-terraform-mode-before-save-hook)

;; Helpers for voice control with Dragon.
(defun jarpy-goto-word ()
  "Dragon friendly avy-goto-word-0 with numeric targets."
  (interactive)
  (let ((avy-keys (number-sequence ?0 ?9)))
    (avy-goto-word-0 nil)))

(defun jarpy-goto-line ()
  "Dragon friendly avy-goto-line with numeric targets."
  (interactive)
  (let ((avy-keys (number-sequence ?0 ?9)))
    (avy-goto-line nil)))

(defun jarpy-goto-char (char)
  "Dragon friendly avy-goto-char with numeric targets for CHAR."
  (interactive (list (read-char "char: " t) current-prefix-arg))
  (let ((avy-keys (number-sequence ?0 ?9)))
    (avy-goto-char char)))

(defun jarpy-goto-buffer ()
  "Dragon friendly ace-jump-buffer with numeric targets."
  (interactive)
  (let ((avy-keys (number-sequence ?0 ?9)))
    (ace-jump-buffer)))

(setq frame-title-format "emacs: %b in %m mode")

(provide 'jarpy)
;;; jarpy.el ends here
