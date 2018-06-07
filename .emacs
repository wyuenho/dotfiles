;;; -*- lexical-binding: t -*-

;; Set file, keyboard and terminal coding systems automatically
(prefer-coding-system 'utf-8)

;; Stop asking me if a theme is safe. The entirety of Emacs is built around
;; evaling arbitrary code...
(advice-add 'load-theme :around (lambda (old-load-theme &rest args)
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

(require 'quelpa-use-package)

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

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
              (if (and (not (use-region-p))
                       (not (looking-at "[ \t]*\n")))
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

(defun upcase-region-dwim (beg end)
  "`upcase-region' when `transient-mark-mode' is on and region is active."
  (interactive "*r")
  (when (use-region-p)
    (upcase-region beg end)))

(defun downcase-region-dwim (beg end)
  "`downcase-region' when `transient-mark-mode' is on and region is active."
  (interactive "*r")
  (when (use-region-p)
    (downcase-region beg end)))

;; Bind useful things to keys
(bind-keys ("<backtab>" . align)
           ("C-x C-u"   . upcase-region-dwim)
           ("C-x C-l"   . downcase-region-dwim)
           ("C-x f"     . follow-mode)
           ;; Replace default buffer menu with ibuffer
           ("C-x C-b"   . ibuffer)
           ("C-t"       . transpose-sexps))

;; Replace zap-to-char with the hidden zap-up-to-char
(autoload 'zap-up-to-char "misc")
(bind-key "M-z" 'zap-up-to-char)

;; Other missing essentials that I don't want to write
(use-package crux
  :bind (("C-c D" . crux-delete-file-and-buffer)
         ("C-c R" . crux-rename-file-and-buffer)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)))

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

;; Turn off useless mode lighters
(use-package delight
  :config
  (delight '((rainbow-mode)
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

(use-package osx-trash
  :if (and (eq system-type 'darwin)
           (not (fboundp 'system-move-file-to-trash)))
  :config (osx-trash-setup))

(use-package ialign
  :bind ("<A-tab>" . ialign))

(use-package goto-chg
  :bind (("C-," . goto-last-change)))

;; Turn on keyboard shortcut remainder
(use-package which-key
  :delight
  :bind (("C-h b" . which-key-show-top-level)))

;; Completely unbind visual-line-mode's stupid bindings
(use-package visual-line-mode
  :hook (prog-mode text-mode messages-buffer-mode))

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
  :bind (("M-x" . amx)
         ("M-X" . amx-major-mode-commands))
  :config (amx-mode t))

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
  :bind (("M-%"     . vr/replace)
         ("C-M-%"   . vr/query-replace)
         ("M-s C-s" . vr/isearch-forward)
         ("M-s m"   . vr/mc-mark)))

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
              ("C-c i" . yas-expand)))

;; Cycle through most common programming identifier styles
(use-package string-inflection
  :preface
  (defun inflect-string ()
    (interactive)
    (cond ((memq major-mode '(java-mode js-mode js2-mode rjsx-mode typescript-mode go-mode))
           (string-inflection-java-style-cycle))
          ((memq major-mode '(python-mode ruby-mode))
           (string-inflection-ruby-style-cycle))
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

;; Auto-completion
(use-package company
  :delight
  :config
  :bind (:map company-mode-map
              ("M-/" . company-complete)))

(use-package company-flx
  :hook (company-mode . company-flx-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

;; Linting
(use-package flycheck
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
                          (add-to-list 'company-backends '(company-shell company-shell-env))))))

(use-package multi-term
  :bind (("M-T" . multi-term)))

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package flycheck-yamllint
  :after yaml-mode
  :config (flycheck-yamllint-setup))

;; Lisp
(bind-keys :map emacs-lisp-mode-map
           ("C-c e f" . byte-compile-file)
           ("C-c e c" . emacs-lisp-byte-compile)
           ("C-c e l" . emacs-lisp-byte-compile-and-load)
           ("C-c e b" . eval-buffer)
           ("C-c e r" . eval-region)
           ("C-c e e" . eval-print-last-sexp))

(use-package cl-lib-highlight
  :config
  (cl-lib-highlight-initialize)
  (cl-lib-highlight-warn-cl-initialize))

;; Javascript
(add-hook 'js-mode-hook
          (lambda ()
            (use-package tern
              :delight
              :config
              (tern-mode t)
              (unbind-key "C-c C-r" tern-mode-keymap))

            (use-package company-tern
              :after company tern
              :config
              (setq-local company-backends
                          (add-to-list 'company-backends 'company-tern)))

            (use-package js-doc
              :after yasnippet
              :bind (:map js-mode-map
                          ("C-c d m" . js-doc-insert-file-doc)
                          ("C-c d f" . js-doc-insert-function-doc-snippet)))

            (use-package add-node-modules-path
              :config (add-node-modules-path))

            (use-package import-js
              :config (run-import-js)
              :bind (:map js-mode-map
                          ("C-c t i"   . import-js-import)
                          ("C-c t f"   . import-js-fix)
                          ("C-c t M-." . import-js-goto)))

            (use-package f
              :preface
              (defun find-js-format-style ()
                (let* ((package-json-dir
                        (f-traverse-upwards
                         (lambda (path)
                           (f-exists? (f-expand "package.json" path)))
                         (buffer-file-name)))

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
              :config
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
                                     ("C-c f" . js-format-buffer)))))))

            (use-package nodejs-repl
              :bind(:map js-mode-map
                         ("C-x C-e" . nodejs-repl-send-last-expression)
                         ("C-c r"   . nodejs-repl-send-region)
                         ("C-c b"   . nodejs-repl-send-buffer)
                         ("C-c l"   . nodejs-repl-load-file)
                         ("C-c z"   . nodejs-repl-switch-to-repl)))

            (define-key js-mode-map [menu-bar] nil)))

(use-package js2-mode
  :defer t
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
  :quelpa (rjsx-mode :fetcher github :repo "wyuenho/rjsx-mode" :branch "indent-after-jsx-expr")
  :mode ("\\.jsx?\\'" "\\.mjs\\'"))

;; TypeScript
(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.mts\\'"))

(use-package ts-comint
  :after typescript-mode
  :bind (:map typescript-mode-map
              ("C-x C-e" . ts-send-last-sexp)
              ("C-M-x"   . ts-send-last-sexp-and-go)
              ("C-c r"   . ts-send-region-and-go)
              ("C-c b"   . ts-send-buffer-and-go)
              ("C-c l"   . ts-load-file-and-go)
              ("C-c z"   . switch-to-ts)))

(use-package tide
  :after typescript-mode
  :hook (typescript-mode . tide-setup)
  :config
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  :bind (:map tide-mode-map
              ("C-c f" . tide-format)
              ("C-c r" . tide-rename-symbol)
              ("C-c 1" . tide-fix)
              ("M-?"   . tide-references)
              ("C-c j" . tide-jsdoc-template)
              ("C-h d" . tide-documentation-at-point)))

;; Python
(use-package pyenv-mode
  :delight
  :hook (python-mode . pyenv-mode))

(use-package pipenv
  :delight
  :hook (python-mode . pipenv-mode)
  :config
  (setq pipenv-projectile-after-switch-function
        'pipenv-projectile-after-switch-extended))

(use-package anaconda-mode
  :delight
  :after company
  :hook (python-mode . anaconda-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (anaconda-eldoc-mode t)
              (setq-local company-backends
                          (add-to-list 'company-backends '(company-anaconda :with company-capf))))))

(add-hook 'python-mode-hook
          (lambda ()
            (use-package flycheck-mypy)))

(use-package python-docstring
  :delight
  :hook (python-mode . python-docstring-mode))

(use-package importmagic
  :delight
  :hook (python-mode . importmagic-mode)
  :config
  :bind (:map importmagic-mode-map
              ("M-1" . importmagic-fix-imports)))

(use-package blacken
  :delight
  :hook (python-mode . blacken-mode))

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (use-package company-go
                :after company
                :config
                (setq-local company-backends
                            (add-to-list 'company-backends 'company-go))))))

(use-package go-eldoc
  :after go-mode
  :hook (go-mode . go-eldoc-setup))

(use-package go-projectile
  :after go-mode projectile)

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package racer
  :delight
  :after rust-mode
  :hook (rust-mode . racer-mode)
  :config
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'racer-mode-hook 'company-mode))

(use-package cargo
  :delight
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

;; Web
(use-package scss-mode
  :mode "\\.scss\\'")

(use-package rainbow-mode
  :hook prog-mode
  :config
  (when (< emacs-major-version 26)
    (add-hook 'css-mode-hook 'rainbow-mode)))

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
                            '(company-web-html company-css company-tern company-files)))

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
  :config
  (projectile-mode t)
  (with-eval-after-load 'pyenv-mode
    (add-hook 'projectile-switch-project-hook
              (lambda ()
                "Set pyenv version matching project name."
                (let ((project (projectile-project-name)))
                  (if (member project (pyenv-mode-versions))
                      (pyenv-mode-set project)
                    (pyenv-mode-unset)))))))

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

(use-package magithub
  :config
  (magithub-feature-autoinject t))

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
  (require 'window-purpose)
  (purpose-add-user-purposes
   :modes '((ag-mode              . search)
            (rg-mode              . search)
            (shell-mode           . terminal)
            (inferior-python-mode . terminal))
   :names '(("*Pipenv shell*"     . terminal)))

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
  (set-face-attribute 'default nil :family "Noto Sans Mono" :weight 'regular)

  (with-eval-after-load 'linum
    (set-face-attribute 'linum nil :weight 'thin))
  (with-eval-after-load 'display-line-numbers
    (set-face-attribute 'line-number nil :weight 'thin))

  (let ((win-sys (window-system)))
    (when (eq win-sys 'mac)
      ;; A bug in the mac port saves the mouse color when `frameset-save' is called,
      ;; but it's not desirable on macOS because the window server will decide the
      ;; color of the cursor according to the background color.
      (add-to-list 'frameset-filter-alist '(mouse-color . :never))

      (bind-keys ("C-|" . mac-toggle-tab-group-overview)
                 ("C-{" . mac-previous-tab-or-toggle-tab-bar)
                 ("C-}" . mac-next-tab-or-toggle-tab-bar)))

    ;; Emacs 26 ns port new settings
    (when (eq win-sys 'ns)
      ;; Will at least display emojis if the multicolor font patch is applied
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
      (dolist (pair '((ns-transparent-titlebar . t) (ns-appearance . dark)))
        (push pair (alist-get 'ns window-system-default-frame-alist nil))
        (set-frame-parameter nil (car pair) (cdr pair)))
      (setq frame-title-format nil
            ns-use-proxy-icon nil
            ns-use-thin-smoothing t
            ns-use-mwheel-momentum t
            ns-use-mwheel-acceleration t
            ;; MacPorts emacs-app port bug
            x-colors (ns-list-colors)))

    ;; Sets $MANPATH, $PATH and exec-path from your shell, but only on OS X
    (use-package exec-path-from-shell
      :if (memq (window-system) '(mac ns))
      :config
      (run-with-idle-timer 1 nil 'exec-path-from-shell-initialize)))

  (set-frame-parameter nil 'fullscreen 'maximized))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-toggle-buffer-encoding-abbrev-off))

;; Replace the major mode name with its icon and
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (let* ((icon (all-the-icons-icon-for-mode major-mode))
                     (face-prop (and (stringp icon) (purecopy (get-text-property 0 'face icon)))))
                (when (and (stringp icon) (not (string= major-mode icon)) face-prop)
                  (setq mode-name icon))))))

(use-package solarized-theme
  :if (display-graphic-p)
  :config
  (load-theme 'solarized-dark)
  (dolist (entry '((region . ((((class color) (background light))
                               :background "#00629D" :foreground "#FDF6E3")
                              (((class color) (background dark))
                               :background "#69B7F0" :foreground "#002B36")))
                   (ediff-current-diff-C . ((((class color) (background light))
                                             (:background "#DDEEFF" :foreground "#005588"))
                                            (((class color) (background dark))
                                             (:background "#005588" :foreground "#DDEEFF"))))
                   (ediff-fine-diff-C . ((((class color) (background light))
                                          (:background "#EEFFFF" :foreground "#006699"))
                                         (((class color) (background dark))
                                          (:background "#006699" :foreground "#EEFFFF"))))
                   (imenu-list-entry-face-0 . ((t (:inherit
                                                   (imenu-list-entry-face font-lock-type-face)
                                                   :foreground nil))))
                   (imenu-list-entry-face-1 . ((t (:inherit
                                                   (imenu-list-entry-face font-lock-function-name-face)
                                                   :foreground nil))))
                   (imenu-list-entry-face-2 . ((t (:inherit
                                                   (imenu-list-entry-face font-lock-variable-name-face)
                                                   :foreground nil))))
                   (imenu-list-entry-face-3 . ((t (:inherit
                                                   (imenu-list-entry-face font-lock-string-face)
                                                   :foreground nil))))
                   (imenu-list-entry-subalist-face-0 . ((t (:inherit imenu-list-entry-face-0))))
                   (imenu-list-entry-subalist-face-1 . ((t (:inherit imenu-list-entry-face-1))))
                   (imenu-list-entry-subalist-face-2 . ((t (:inherit imenu-list-entry-face-2))))
                   (imenu-list-entry-subalist-face-3 . ((t (:inherit imenu-list-entry-face-3))))))
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

  (set-face-attribute 'dired-header nil :underline t :background nil :foreground nil)
  (set-face-attribute 'mode-line nil :overline nil :underline nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :overline nil :underline nil :box nil)
  (set-face-attribute 'header-line nil :overline nil :underline nil :box nil))
