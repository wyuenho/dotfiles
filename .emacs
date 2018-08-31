;;; -*- lexical-binding: t -*-

;; Set file, keyboard and terminal coding systems automatically
(prefer-coding-system 'utf-8)

;; Stop asking me if a theme is safe. The entirety of Emacs is built around
;; evaling arbitrary code...
(advice-add 'load-theme :around (lambda (old-load-theme &rest args)
                                  "Don't ask for confirmation when loading a theme."
                                  (apply old-load-theme (car args) t (cddr args))))

(when (< emacs-major-version 27)
  (package-initialize))

;; Tell Custom to write and find the custom settings elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Install missing packages
(let ((missing (cl-set-difference package-selected-packages package-activated-list)))
  (when missing
    (package-refresh-contents)
    (mapc (lambda (package)
            (package-install package t)
            (package-activate package))
          missing)
    (load custom-file)))

;; Sets $MANPATH, $PATH and exec-path from your shell, but only on OS X. This is
;; should be done ASAP on init.
(use-package exec-path-from-shell
  :if (memq (window-system) '(mac ns))
  :config (exec-path-from-shell-initialize))

(require 'quelpa-use-package)

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Remove all query on exit flags on all processes before quitting
(unless (boundp 'confirm-kill-processes) ;; new on Emacs 26
  (advice-add 'save-buffers-kill-emacs :before
              (lambda (&rest _)
                (defun processes-with-query (process)
                  (and (memq (process-status process) '(run stop open listen))
                       (process-query-on-exit-flag process)))
                (let ((processes (seq-filter 'processes-with-query (process-list))))
                  (dolist (process processes)
                    (set-process-query-on-exit-flag process nil)))))
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function kill-buffer-query-functions)))

;; Only turn on `auto-revert-mode' for Mac on Emacs >= 26 because kqueue file
;; notification is broken for Emacs < 26
(when (and (>= emacs-major-version 26)
           (memq (window-system) '(mac ns)))
  (global-auto-revert-mode t))

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Turn on line wrapping for programming, text and message buffers
(dolist (hook '(prog-mode-hook text-mode-hook messages-buffer-mode-hook))
  (add-hook hook 'visual-line-mode))

;; Turn on subword mode and linum mode for all prog and text modes
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda ()
                   (subword-mode t)
                   (if (fboundp 'display-line-numbers-mode)
                       (display-line-numbers-mode t)
                     (linum-mode t)
                     ;; Renumber the current buffer after reverting the buffer
                     (add-hook 'after-revert-hook 'linum-update-current)))))

;; More sensible comment-dwim
(advice-add 'comment-dwim :around
            (lambda (comment-dwim &rest args)
              "Replacement for the `comment-dwim' command.

If no region is selected and point is not at the end of the line,
comment the current line from the front, otherwise the
commentable lines in the region.  Replaces the default behaviour
of `comment-dwim', where it inserts comment at the end of the
line if no region is defined.

Optional argument ARG same as `comment-dwim''s."

              (interactive "*P")
              (if (and (not (use-region-p)))
                  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
                (apply comment-dwim args))))

;; I hate X mouse bindings
(when (and (display-graphic-p)
           (not (memq (window-system) '(x))))
  (bind-key "<mouse-3>" 'mouse-buffer-menu))

(bind-key "C-x t" 'display-time-world)

;; Completely unbind annoying abbrev, dabbrev, expand, hippie-expand. These
;; ancient completion commands are just too stupid for this day and age
(unbind-key "M-'")
(unbind-key "M-/")
(unbind-key "C-x '")
;; Always use M-g prefix to jump between errors
(unbind-key "C-x `")

;; Bind useful things to keys
(bind-keys ("<backtab>" . align)
           ("C-x f"     . follow-mode)
           ;; Replace default buffer menu with ibuffer
           ("C-x C-b"   . ibuffer)
           ("C-t"       . transpose-sexps)
           ("M-:"       . ielm)
           ("C-x ^"     . nil)
           ("C-{"       . enlarge-window)
           ("C-}"       . shrink-window)
           ("C-c l"     . browse-url-at-point))

;; Replace zap-to-char with the hidden zap-up-to-char
(autoload 'zap-up-to-char "misc")
(bind-key "M-z" 'zap-up-to-char)

;; Not that I use occur very often, but when I do, I'd like its keybindings the
;; same as grep mode's
(add-hook 'occur-mode-hook
          (lambda ()
            (bind-keys :map occur-mode-map
                       ("M-n" . nil)
                       ("M-p" . nil)
                       ("n"   . occur-next)
                       ("p"   . occur-prev))))

;; I use compilation mode more, so of course I have to do the same thing as
;; occur mode
(add-hook 'compilation-mode-hook
          (lambda ()
            (bind-keys :map compilation-mode-map
                       ("M-n" . nil)
                       ("M-p" . nil)
                       ("n"   . compilation-next-error)
                       ("p"   . compilation-previous-error))))

;; Persistent history for all the inferior modes
(add-hook 'comint-mode-hook
          (lambda ()
            (let ((proc (get-buffer-process (current-buffer))))
              (when proc
                (setq-local comint-input-ring-file-name
                            (expand-file-name
                             (format ".%s-history" major-mode)
                             user-emacs-directory))
                (when (file-exists-p comint-input-ring-file-name)
                  (let ((comint-input-ring-separator "\n"))
                    (comint-read-input-ring)))
                (set-process-sentinel
                 proc
                 (lambda (process state)
                   (let ((comint-input-ring-separator "\n"))
                     (comint-write-input-ring))
                   (quit-window 'kill)))))))

(use-package osx-trash
  :if (and (eq system-type 'darwin)
           (not (fboundp 'system-move-file-to-trash)))
  :config (osx-trash-setup))

;; Turn on keyboard shortcut remainder
(use-package which-key
  :delight
  :bind (("C-h b" . which-key-show-top-level)
         ("C-h m" . which-key-show-major-mode)))

;; Visual alignment
(use-package ialign
  :bind ("<A-tab>" . ialign))

;; Other missing essentials that I don't want to write
(use-package crux
  :bind (("C-x C-u" . crux-upcase-region)
         ("C-x C-l" . crux-downcase-region)
         ("C-x M-c" . crux-capitalize-region)
         ("C-c d"   . crux-delete-file-and-buffer)
         ("C-c r"   . crux-rename-file-and-buffer)
         ("C-c c"   . crux-copy-file-preserve-attributes)
         ("C-c u"   . crux-find-user-init-file)
         ("C-c ,"   . crux-find-user-custom-file)
         ("C-c s"   . crux-find-shell-init-file)
         ("C-c C-u" . crux-sudo-edit)
         ("C-c M-o" . crux-open-with)))

;; So I can see past kills that I can yank
(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

;; Sane keyboard scrolling
(use-package pager-default-keybindings)

;; Adjust frame-wide font size
(use-package default-text-scale
  :bind (("C-x C-=" . default-text-scale-increase)
         ("C-x C--" . default-text-scale-decrease)
         ("C-x C-0" . default-text-scale-reset)))

;; Enhances ido and isearch's fuzzy search
(use-package flx-ido
  :config (flx-ido-mode t))

(use-package flx-isearch
  :bind (("C-M-s" . flx-isearch-forward)
         ("C-M-r" . flx-isearch-backward))
  :config (flx-isearch-mode t))

;; Use ido with M-x
(use-package amx
  :bind (("M-X" . amx-major-mode-commands)))

;; Use ido for even more things than ido-everywhere
(use-package ido-completing-read+)
(use-package ido-vertical-mode)

;; Convenient iMenu entry search
(use-package imenu-anywhere
  :bind (("C-." . imenu-anywhere)))

;; Turn on iMenu for code outlines for all prog and text modes, if possible
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (ignore-errors (imenu-add-menubar-index)))))

;; Mark and edit multiple things at once
(use-package multiple-cursors
  :bind (("M-s M-e" . mc/edit-lines)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-M->"   . mc/skip-to-next-like-this)
         ("C-M-<"   . mc/skip-to-previous-like-this)
         ("M-s C->" . mc/mark-all-dwim)))

;; Construct regexp and search visually and incrementally
(use-package visual-regexp-steroids
  :bind (("M-%"   . vr/replace)
         ("C-M-%" . vr/query-replace)
         ("C-s"   . vr/isearch-forward)
         ("C-r"   . vr/isearch-backward)
         ("M-s m" . vr/mc-mark)))

;; More convenient region selection
(use-package expand-region
  :bind (("M-=" . er/expand-region)
         ("M--" . er/contract-region))
  :preface
  (defun load-html-mode-expansions (mode)
    (lambda ()
      (require 'html-mode-expansions)
      (er/enable-mode-expansions mode 'er/add-html-mode-expansions)))
  :config
  (add-hook 'js-jsx-mode-hook (load-html-mode-expansions 'js-jsx-mode))
  (with-eval-after-load 'js2-mode
    (add-hook 'js2-jsx-mode-hook (load-html-mode-expansions 'js2-jsx-mode)))
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook (load-html-mode-expansions 'rjsx-mode))))

(add-hook 'prog-mode-hook
          (lambda ()
            (use-package smartparens
              :config
              (require 'smartparens-config)
              :bind (:map smartparens-mode-map
                          ("C-M-a"                      . sp-beginning-of-sexp)
                          ([remap sp-beginning-of-sexp] . beginning-of-defun)
                          ("C-M-e"                      . sp-end-of-sexp)
                          ([remap sp-end-of-sexp]       . end-of-defun)

                          ("C-M-f" . sp-forward-sexp)
                          ("C-M-b" . sp-backward-sexp)

                          ("C-M-n" . sp-next-sexp)
                          ("C-M-p" . sp-previous-sexp)

                          ("C-M-d" . sp-down-sexp)
                          ("C-M-u" . sp-backward-up-sexp)
                          ("M-D"   . sp-backward-down-sexp)
                          ("M-U"   . sp-up-sexp)

                          ("C-S-f" . sp-forward-symbol)
                          ("C-S-b" . sp-backward-symbol)

                          ("A-<right>" . sp-slurp-hybrid-sexp)
                          ("M-<right>" . sp-forward-barf-sexp)
                          ("A-<left>"  . sp-backward-slurp-sexp)
                          ("M-<left>"  . sp-backward-barf-sexp)

                          ("C-M-w"   . sp-copy-sexp)
                          ("C-M-S-t" . sp-push-hybrid-sexp)
                          ("C-M-t"   . sp-transpose-hybrid-sexp)

                          ("C-S-d" . sp-kill-symbol)
                          ("C-M-k" . sp-kill-sexp)
                          ("C-k"   . sp-kill-hybrid-sexp)
                          ("M-k"   . sp-backward-kill-sexp)

                          ("M-<backspace>"               . backward-kill-word)
                          ("C-<backspace>"               . sp-backward-kill-word)
                          ([remap sp-backward-kill-word] . backward-kill-word)

                          ("M-(" . sp-wrap-round)
                          ("M-[" . sp-wrap-square)
                          ("M-{" . sp-wrap-curly)
                          ("M-}" . sp-unwrap-sexp)
                          ("M-]" . sp-backward-unwrap-sexp)))))

;; Cross-machine fomatting
(use-package editorconfig
  :delight
  :config
  (add-to-list 'editorconfig-indentation-alist
               '(rjsx-mode js2-basic-offset sgml-basic-offset)))

;; Prettier form-feeds
(use-package form-feed
  :delight
  :hook ((prog-mode . form-feed-mode)
         (text-mode . form-feed-mode)))

;; Quick Snippets
(use-package yasnippet
  :delight yas-minor-mode
  :commands yas-minor-mode
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (yas-reload-all)
  :bind (:map yas-minor-mode-map
              ("TAB"   . nil)
              ("<tab>" . nil)
              ("C-c i" . yas-expand-from-trigger-key)))

;; No sane person will program from right to left, so turn this major perf
;; bottleneck off
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local bidi-display-reordering nil)))

;; Turn on background color for HEX for specific modes
(dolist (hook `(emacs-lisp-mode-hook
                web-mode-hook
                js-mode-hook
                typescript-mode-hook
                sh-mode-hook
                ,@(if (< emacs-major-version 26)
                      css-mode-hook)))
  (add-hook hook (lambda ()
                   (use-package rainbow-mode
                     :config (rainbow-mode)))))

;; Cycle through most common programming identifier styles
(use-package string-inflection
  :preface
  (defun inflect-string ()
    (interactive)
    (cond ((memq major-mode '(java-mode js-mode js2-mode rjsx-mode typescript-mode go-mode))
           (string-inflection-java-style-cycle))
          ((memq major-mode '(python-mode ruby-mode c-mode rust-mode))
           (string-inflection-python-style-cycle))
          ((derived-mode-p major-mode 'prog-mode)
           (string-inflection-all-cycle))))
  :config
  (setq string-inflection-skip-backward-when-done t)
  :bind (("C-x C-y" . inflect-string)))

;; Cycle between quotes
(use-package cycle-quotes
  :bind (("C-x C-'" . cycle-quotes)))

;; Vim-like increment and decrement
(use-package evil-numbers
  :bind (("C-x =" . evil-numbers/inc-at-pt)
         ("C-x -" . evil-numbers/dec-at-pt)))

;; Modern code folding
(use-package origami
  :config
  (with-eval-after-load 'hideshow
    ;; Unloading is unsafe, so this the best I can do to pretend `hideshow'
    ;; never existed.
    (setq minor-mode-map-alist
          (assq-delete-all 'hs-minor-mode minor-mode-map-alist)
          minor-mode-alist
          (assq-delete-all 'hs-minor-mode minor-mode-alist)
          minor-mode-list
          (delq 'hs-minor-mode minor-mode-list)))
  :bind (:map origami-mode-map
              ("M-0"   . origami-open-all-nodes)
              ("M-9"   . origami-close-all-nodes)
              ("C-M-/" . origami-recursively-toggle-node)))

;; Minimum Distraction
(use-package olivetti
  :delight
  :bind ("C-c o" . olivetti-mode))

;; Much faster PDF viewing
(add-hook 'doc-view-mode-hook
          (lambda ()
            (use-package pdf-tools
              :config (pdf-tools-install))))

;; Static Analysis
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; Auto-completion
(use-package company
  :delight
  :preface
  (defun company-complete-common-or-cycle-next ()
    (interactive)
    (company-complete-common-or-cycle 1))
  (defun company-complete-common-or-cycle-previous ()
    (interactive)
    (company-complete-common-or-cycle -1))
  :bind (:map company-mode-map
              ("M-/" . company-manual-begin)
              :map company-active-map
              ("M-n" . company-complete-common-or-cycle-next)
              ("M-p" . company-complete-common-or-cycle-previous)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (if (fboundp 'company-flx-mode)
      (company-flx-mode))
  (setq company-backends
        `((company-eclim company-semantic company-clang)
          company-xcode
          company-cmake
          company-files
          ,@(if (and (memq (window-system) '(ns mac))
                     (fboundp 'company-emoji))
                '((company-emoji :separate company-capf :separate company-yasnippet))
              '(company-capf :separate company-yasnippet))
          (company-dabbrev-code
           company-gtags
           company-etags
           company-keywords))))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-lsp
  :config
  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (make-local-variable 'company-transformers)
              (setq company-transformers (remq 'company-sort-by-statistics company-transformers))
              (setq company-transformers (remq 'company-flx-transformer company-transformers))
              (setq-local company-backends `(,@(if (and (memq (window-system) '(ns mac))
                                                        (fboundp 'company-emoji))
                                                   '((company-emoji :separate company-lsp :separate company-yasnippet))
                                                 '(company-lsp :separate company-yasnippet))
                                             company-files
                                             company-capf
                                             company-keywords)))))

;; Linting
(use-package flycheck
  :delight
  :config
  (global-flycheck-mode t))

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode))

;; REST API
(use-package restclient
  :commands restclient-mode
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :config
  (add-hook 'restclient-mode-hook
            (lambda ()
              (use-package company-restclient
                :after company
                :config
                (setq-local company-backends `(company-restclient))))))

;; Term and shell
(add-hook 'sh-mode-hook
          (lambda ()
            (use-package company-shell
              :after company
              :config
              (setq-local company-backends
                          '(company-shell company-shell-env company-files company-capf)))))

(use-package multi-term
  :bind (("M-T" . multi-term)
         :map projectile-command-map
         ("x T" . multi-term)))

(setq eshell-directory-name (expand-file-name ".eshell/" user-emacs-directory))

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package flycheck-yamllint
  :after yaml-mode
  :config (flycheck-yamllint-setup))

;; Emacs Lisp
(bind-keys :map emacs-lisp-mode-map
           ("C-c e f" . byte-compile-file)
           ("C-c e c" . emacs-lisp-byte-compile)
           ("C-c e l" . emacs-lisp-byte-compile-and-load)
           ("C-c e b" . eval-buffer)
           ("C-c e r" . eval-region)
           ("C-c e e" . eval-print-last-sexp))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e x" . macrostep-expand)))

(use-package cl-lib-highlight
  :config
  (cl-lib-highlight-initialize)
  (cl-lib-highlight-warn-cl-initialize))

(use-package elisp-def
  :delight
  :hook (emacs-lisp-mode . elisp-def-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'lisp-imenu-generic-expression)
            (setq imenu-generic-expression
                  (add-to-list
                   'lisp-imenu-generic-expression
                   '("Packages" "^\\s-*(use-package\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 1)
                   t))))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)
         ("C-h p" . helpful-at-point)))

;; LSP for C/C++/Objective-C and Python
(dolist (hook '(c-mode-common-hook python-mode-hook))
  (add-hook hook
            (lambda ()
              (use-package eglot
                :config
                (add-hook 'eglot--managed-mode-hook
                          (lambda ()
                            (bind-keys :map eglot-mode-map
                                       ("C-h o"   . eglot-help-at-point)
                                       ("C-c C-r" . eglot-rename)
                                       ("C-c f"   . eglot-format)
                                       ("M-1"     . eglot-code-actions))))
                (with-eval-after-load 'company
                  (setq company-transformers (remq 'company-flx-transformer company-transformers))
                  (setq-local company-backends '(company-files
                                                 (company-capf :separate company-yasnippet)
                                                 company-keywords)))
                (eglot-ensure)))))

;; C/C++/Objective-C
(use-package flycheck-clang-analyzer
  :hook (flycheck-mode . flycheck-clang-analyzer-setup))

(use-package flycheck-objc-clang
  :hook (flycheck-mode . flycheck-objc-clang-setup))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

;; Javascript
(use-package lsp-javascript-typescript
  :preface
  (defun lsp-js-find-symbol ()
    (interactive)
    (let ((pattern (word-at-point)))
      (when pattern
        (lsp-ui-peek-find-workspace-symbol (substring-no-properties pattern)))))

  :config
  (add-hook 'js-mode-hook
            (lambda ()
              (unless (derived-mode-p 'json-mode)
                (lsp-javascript-typescript-enable)
                (bind-keys :map js-mode-map
                           ("M-."   . lsp-ui-peek-find-definitions)
                           ("M-?"   . lsp-ui-peek-find-references)
                           ("M-,"   . lsp-ui-peek-jump-backward)
                           ("C-M-." . lsp-js-find-symbol))))
            t))

(defun find-js-format-style ()
  (let* ((package-json-dir
          (locate-dominating-file (buffer-file-name) "package.json"))

         (package-json
          (if package-json-dir
              (json-read-file (f-join package-json-dir "package.json"))
            nil))

         (devDependencies
          (if package-json
              (alist-get 'devDependencies package-json)
            nil))

         (formatter-styles
          '((prettier            . prettier)
            (eslint              . eslint)
            (esformatter         . esfmt)
            (babel-preset-airbnb . airbnb)
            (standard            . standard))))

    (cdr (car (map-filter
               (lambda (package _)
                 (map-contains-key devDependencies package))
               formatter-styles)))))

(add-hook 'js-mode-hook
          (lambda ()
            (use-package add-node-modules-path
              :config (add-node-modules-path))

            (use-package import-js
              :bind (:map js-mode-map
                          ("C-c t i"   . import-js-import)
                          ("C-c t f"   . import-js-fix)
                          ("C-c t M-." . import-js-goto))
              :config (run-import-js))

            (use-package js-doc
              :bind (:map js-mode-map
                          ("C-c C-d m" . js-doc-insert-file-doc)
                          ("C-c C-d f" . js-doc-insert-function-doc-snippet)))

            (use-package nodejs-repl
              :bind(:map js-mode-map
                         ("C-c e e" . nodejs-repl-send-last-expression)
                         ("C-c e r" . nodejs-repl-send-region)
                         ("C-c e b" . nodejs-repl-send-buffer)
                         ("C-c e l" . nodejs-repl-load-file)
                         ("C-c M-:" . nodejs-repl-switch-to-repl)))

            (let ((style (find-js-format-style)))
              (cond ((eq style 'prettier)
                     (use-package prettier-js
                       :delight
                       :config
                       (prettier-js-mode t)
                       :bind (:map js-mode-map
                                   ("C-c f" . prettier-js))))

                    ((eq style 'eslint)
                     (use-package eslintd-fix
                       :delight
                       :config
                       (eslintd-fix-mode t)
                       :bind (:map js-mode-map
                                   ("C-c f" . eslintd-fix))))

                    ((memq style '(esfmt airbnb standard))
                     (use-package js-format
                       :config
                       (js-format-setup (symbol-name (find-js-format-style)))
                       :bind (:map js-mode-map
                                   ("C-c f" . js-format-buffer))))))

            (define-key js-mode-map [menu-bar] nil)))

(use-package js2-mode
  :interpreter ("node" . js2-mode)
  :config
  (js2-imenu-extras-mode t)
  (when (fboundp 'sp-kill-whole-line)
    (bind-key "C-k" 'sp-kill-whole-line js2-mode-map)))

(use-package js2-refactor
  :after js2-mode
  :delight
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c r"))

(use-package rjsx-mode
  :mode ("\\.jsx?\\'" "\\.mjs\\'"))

;; TypeScript
(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.mts\\'"))

(use-package ts-comint
  :after typescript-mode
  :bind (:map typescript-mode-map
              ("C-x C-e" . ts-send-last-sexp)
              ("C-c e e" . ts-send-last-sexp-and-go)
              ("C-c e r" . ts-send-region-and-go)
              ("C-c e b" . ts-send-buffer-and-go)
              ("C-c e l" . ts-load-file-and-go)
              ("C-c M-:" . switch-to-ts)))

(use-package tide
  :after typescript-mode
  :hook (typescript-mode . tide-setup)
  :config
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  :bind (:map tide-mode-map
              ("C-h p"   . tide-documentation-at-point)
              ("C-c 1"   . tide-fix)
              ("C-c f"   . tide-format)
              ("C-c C-d" . tide-jsdoc-template)
              ("C-c t f" . tide-organize-imports)
              ("M-RET"   . tide-refactor)
              ("M-?"     . tide-references)
              ("C-c r"   . tide-rename-file)
              ("C-c C-r" . tide-rename-symbol)))

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (use-package py-isort
              :config
              (add-hook 'before-save-hook 'py-isort-before-save))

            (use-package python-docstring
              :delight
              :config (python-docstring-mode))

            (use-package importmagic
              :delight
              :config (importmagic-mode)
              :bind (:map importmagic-mode-map
                          ("M-1" . importmagic-fix-imports)))

            (let ((python-version (shell-command-to-string
                                   (string-join `(,python-shell-interpreter "--version") " "))))
              (if (and (string-match "\\([0-9]+\\)\.[0-9]+\.[0-9]+" python-version)
                       (>= (string-to-number (match-string-no-properties 1 python-version)) 3))
                  (use-package blacken :delight)
                (use-package py-autopep8 :config (py-autopep8-enable-on-save))))))

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)

  (add-hook 'go-mode-hook
            (lambda ()
              (use-package company-go
                :after company go-mode
                :config
                (setq-local company-backends '(company-go)))

              (use-package lsp-go
                :config (lsp-go-enable))

              (use-package go-projectile
                :after projectile))))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (use-package racer
                :delight
                :after company
                :config
                (add-hook 'racer-mode-hook 'eldoc-mode)
                (add-hook 'racer-mode-hook 'company-mode)
                (racer-mode))

              (use-package cargo
                :delight
                :config (cargo-minor-mode)))))

;; Web
(use-package scss-mode
  :mode "\\.scss\\'")

(use-package web-mode
  :functions web-mode-language-at-pos
  :mode ("\\.vue\\'"
         "\\.tsx\\'"
         "\\.handlebars\\'"
         "\\.underscore\\'"
         "\\.html?\\'"
         "\\.jinja\\'"
         "\\.mako\\'"
         "\\.dtl\\'"
         "\\.jsp\\'"
         "\\.soy\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'")
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (use-package tern
                :delight
                :config
                (unbind-key "C-c C-r" tern-mode-keymap))

              (use-package company-tern
                :after company tern
                :config
                (setq-local company-backends
                            '(company-tern company-files))

                (advice-add 'company-tern :before
                            (lambda (&rest _)
                              (if (eq major-mode 'web-mode)
                                  (let ((web-mode-cur-language
                                         (web-mode-language-at-pos)))
                                    (if (or (string= web-mode-cur-language "javascript")
                                            (string= web-mode-cur-language "jsx"))
                                        (unless tern-mode (tern-mode))
                                      (if tern-mode (tern-mode t))))))))

              (use-package company-web-html
                :after company
                :config
                (setq-local company-backends
                            `(company-tern
                              company-web-html
                              ,@(unless (version<= "26" emacs-version)
                                  (list 'company-css))
                              company-yasnippet
                              company-files)))

              (use-package tide
                :if (string-equal "tsx" (file-name-extension buffer-file-name))
                :config
                (add-hook 'before-save-hook 'tide-format-before-save)
                (tide-setup t)
                (tide-hl-identifier-mode t))

              ;; Setup flycheck
              (let ((ext (file-name-extension buffer-file-name)))
                (when (fboundp 'flycheck-add-mode)
                  (cond ((string-equal "tsx" ext)
                         (flycheck-add-mode 'typescript-tslint 'web-mode))
                        ((string-equal "css" ext)
                         (flycheck-add-mode 'css-stylelint 'web-mode))
                        ((string-equal "scss" ext)
                         (flycheck-add-mode 'scss-stylelint 'web-mode))
                        (t nil)))))))

(use-package emmet-mode
  :delight
  :after (:any web-mode js2-mode rjsx-mode)
  :hook (sgml-mode nxml-mode web-mode js-jsx-mode js2-jsx-mode rjsx-mode)
  :config
  (add-hook 'emmet-mode-hook
            (lambda ()
              (when (or (member major-mode '(js-jsx-mode js2-jsx-mode rjsx-mode))
                        (and (eq major-mode 'web-mode)
                             (member (web-mode-language-at-pos) '("jsx" "tsx"))))
                (setq-local emmet-expand-jsx-className? t)))))

;; Project management
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map))

;; Search
(use-package ag
  :bind (("M-s a" . ag)))

(use-package wgrep-ag
  :after ag)

(use-package rg
  :after wgrep-ag projectile
  :config
  (add-hook 'rg-mode-hook
            (lambda ()
              (next-error-follow-minor-mode 0)
              (wgrep-ag-setup)))
  :bind (("M-s r" . rg)
         :map projectile-command-map
         ("s r" . rg-project)))

(use-package dumb-jump)

;; Version Control
(use-package diff-hl
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode t))
  (diff-hl-flydiff-mode t)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package magit)

(use-package magithub
  :config (magithub-feature-autoinject t))

(use-package magit-todos)

(use-package monky
  :bind (("C-x v M-m" . monky-status)))

;; File management
(add-hook 'dired-mode-hook
          (lambda ()
            (add-minor-mode 'dired-hide-details-mode "")
            (when (memq (window-system) '(mac ns))
              (bind-key "z" (lambda ()
                              (interactive)
                              (let ((file-name (dired-get-file-for-visit)))
                                (start-process "default-app" nil "open" file-name)))
                        dired-mode-map))))

(use-package all-the-icons-dired
  :after all-the-icons
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode)))

(use-package dired-single
  :after dired-hide-dotfiles
  :bind (:map dired-mode-map
              ("^"         . (lambda () (interactive) (dired-single-buffer "..")))
              ("<mouse-1>" . dired-single-buffer-mouse)
              ("\C-m"      . dired-single-buffer))
  :config
  ;; Make sure dired-hide-details-mode is preserved when reusing the dired
  ;; window
  (advice-add 'find-alternate-file :around
              (lambda (oldfun &rest args)
                (let ((is-dired (derived-mode-p 'dired-mode))
                      (hide-dotfiles (and (boundp 'dired-hide-dotfiles-mode) dired-hide-dotfiles-mode))
                      (hide-details dired-hide-details-mode)
                      (hide-information-lines dired-hide-details-hide-information-lines)
                      (hide-symlink-targets dired-hide-details-hide-symlink-targets)
                      (result (apply oldfun args)))
                  (when hide-dotfiles (dired-hide-dotfiles-mode))
                  (when is-dired
                    (setq-local dired-hide-details-hide-information-lines hide-information-lines)
                    (setq-local dired-hide-details-hide-symlink-targets hide-symlink-targets)
                    (when hide-details (dired-hide-details-mode)))
                  result))))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

;; Window management

;; Move around windows with shifted arrow keys
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package buffer-move
  :bind (("C-c b <left>"  . buf-move-left)
         ("C-c b <right>" . buf-move-right)
         ("C-c b <up>"    . buf-move-up)
         ("C-c b <down>"  . buf-move-down)))

(use-package imenu-list
  :quelpa (imenu-list :fetcher github :repo "wyuenho/imenu-list" :branch "clear-buffer"))

(use-package window-purpose
  :quelpa (window-purpose :fetcher github :repo "wyuenho/emacs-purpose" :files (:defaults "layouts") :branch "improve-code1")
  :config
  (define-key purpose-mode-map (kbd "C-c ,") nil)
  (define-key purpose-mode-map (kbd "C-c w") purpose-mode-prefix-map)

  (purpose-add-user-purposes
   :modes '((message-mode . edit)
            (ag-mode      . search)
            (rg-mode      . search)))

  (purpose-x-code1-setup)
  (purpose-x-popwin-setup)
  (purpose-x-kill-setup)
  (purpose-x-magit-single-on)

  (purpose-mode t)

  (add-hook 'after-init-hook
            (lambda ()
              (when (file-exists-p purpose-default-layout-file)
                (purpose-load-window-layout-file))
              (select-window (get-largest-window)))))

;; GUI
(when (display-graphic-p)
  ;; Set up default fonts
  (set-face-attribute 'default nil :family "Noto Sans Mono" :weight 'regular :width 'normal)

  (let ((win-sys (window-system)))
    (when (eq win-sys 'mac)
      ;; A bug in the mac port saves the mouse color when `frameset-save' is called,
      ;; but it's not desirable on macOS because the window server will decide the
      ;; color of the cursor according to the background color.
      (add-to-list 'frameset-filter-alist '(mouse-color . :never)))

    ;; Emacs 26 ns port new settings
    (when (eq win-sys 'ns)
      ;; Will at least display native Unicode emojis if the multicolor font
      ;; patch is applied
      (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
      (dolist (pair '((ns-transparent-titlebar . nil)
                      (ns-appearance . dark)))
        (push pair (alist-get 'ns window-system-default-frame-alist nil))
        (set-frame-parameter nil (car pair) (cdr pair)))
      (setq frame-title-format "%b"
            ns-use-thin-smoothing t
            ns-use-mwheel-momentum t
            ns-use-mwheel-acceleration t
            ;; MacPorts emacs-app port bug
            x-colors (ns-list-colors))))

  (set-frame-parameter nil 'fullscreen 'maximized))

;; Turn off useless mode lighters
(use-package delight
  :config
  (delight '((rainbow-mode)
             (lsp-mode)
             (isearch-mode            nil isearch)
             (abbrev-mode             nil abbrev)
             (purpose-mode            nil window-purpose)
             (eldoc-mode              nil eldoc)
             (move-dup-mode           nil move-dup)
             (smartparens-mode        nil smartparens)
             (which-key-mode          nil which-key)
             (whitespace-cleanup-mode nil whitespace)
             (undo-tree-mode          nil undo-tree)
             (auto-revert-mode        nil autorevert)
             (visual-line-mode        nil simple)
             (subword-mode            nil subword))))

;; Fancy mode line
(use-package spaceline
  :config
  (setq powerline-image-apple-rgb
        (and (eq (window-system) 'ns)
             ns-use-srgb-colorspace
             (< 11
                (string-to-number
                 (and (string-match "darwin\\([0-9]+\\)" system-configuration)
                      (match-string-no-properties 1 system-configuration))))))
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-toggle-buffer-encoding-abbrev-off))

;; Replace the major mode name with its icon
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (let* ((icon (all-the-icons-icon-for-mode major-mode))
                     (face-prop (and (stringp icon) (get-text-property 0 'face icon))))
                (when (and (stringp icon) (not (string= major-mode icon)) face-prop)
                  (setq mode-name (propertize icon 'display '(:ascent center))))))))

(use-package solarized-theme
  :if (display-graphic-p)
  :config
  (load-theme 'solarized-dark)

  (dolist (entry `((region . ((((type ns))
                               (:background "selectedTextBackgroundColor" :foreground "selectedTextColor"))))
                   (ediff-current-diff-C . ((((class color) (background light))
                                             (:background "#D1E2F2" :foreground "#004D7B"))
                                            (((class color) (background dark))
                                             (:background "#004D7B" :foreground "#D1E2F2"))))
                   (ediff-fine-diff-C . ((((class color) (background light))
                                          (:background "#D6E5E5" :foreground "#00547F"))
                                         (((class color) (background dark))
                                          (:background "#00547F" :foreground "#D6E5E5"))))))
    (let ((face (car entry))
          (spec (cdr entry)))
      (put face 'theme-face nil)
      (face-spec-set face spec)))

  (dolist (face-map '((diff-hl-insert              . magit-diff-added)
                      (diff-hl-change              . ediff-current-diff-C)
                      (diff-hl-delete              . magit-diff-removed)
                      (smerge-base                 . magit-diff-base)
                      (smerge-lower                . magit-diff-added)
                      (smerge-markers              . magit-diff-conflict-heading)
                      (smerge-refined-added        . magit-diff-added-highlight)
                      (smerge-refined-removed      . magit-diff-removed-highlight)
                      (smerge-upper                . magit-diff-removed)
                      (ediff-even-diff-A           . magit-diff-context-highlight)
                      (ediff-even-diff-Ancestor    . magit-diff-context)
                      (ediff-even-diff-B           . magit-diff-context-highlight)
                      (ediff-even-diff-C           . magit-diff-context-highlight)
                      (ediff-odd-diff-A            . magit-diff-context-highlight)
                      (ediff-odd-diff-Ancestor     . magit-diff-context)
                      (ediff-odd-diff-B            . magit-diff-context-highlight)
                      (ediff-odd-diff-C            . magit-diff-context-highlight)
                      (ediff-current-diff-A        . magit-diff-our)
                      (ediff-current-diff-Ancestor . magit-diff-base)
                      (ediff-current-diff-B        . magit-diff-their)
                      (ediff-fine-diff-A           . magit-diff-removed-highlight)
                      (ediff-fine-diff-Ancestor    . magit-diff-base-highlight)
                      (ediff-fine-diff-B           . magit-diff-added-highlight)
                      (diff-header                 . magit-diff-hunk-heading)
                      (diff-context                . magit-diff-context)
                      (diff-added                  . magit-diff-added)
                      (diff-removed                . magit-diff-removed)
                      (diff-changed                . smerge-refined-changed)
                      (diff-refine-added           . magit-diff-added-highlight)
                      (diff-refine-removed         . magit-diff-removed-highlight)
                      (diff-refine-changed         . ediff-fine-diff-C)
                      (diff-indicator-added        . magit-diffstat-added)
                      (diff-indicator-removed      . magit-diffstat-removed)))
    (let* ((face (car face-map))
           (alias (cdr face-map)))
      (put face 'theme-face nil)
      (put face 'face-alias alias)))

  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line nil :overline line :box nil)
    (set-face-attribute 'mode-line-inactive nil :overline line :underline line :box nil))

  (set-face-attribute 'dired-header nil :underline t :background nil :foreground nil))
