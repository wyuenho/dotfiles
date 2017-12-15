;; Stop asking me if a theme is safe. The entirety of Emacs is built around
;; evaling arbitrary code...
(advice-add 'load-theme :around #'(lambda (old-load-theme &rest args)
                                    ;; Disable themes on terminals
                                    (when (display-graphic-p)
                                      (apply old-load-theme (car args) t (cddr args)))))

;; Emacs loads init file first and the packages last normally. Forcing the
;; packages to load first makes configuring them in the init file possible.
(package-initialize)

;; Tell Custom to write and find the custom settings elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Maximize frame on startup and set up default fonts
(when (display-graphic-p)
  (let* ((preferred-font-families '("Noto Sans Mono"
                                    "Roboto Mono"
                                    "DejaVu Sans Mono"
                                    "Bitstream Vera Sans Mono"
                                    "Fira Mono"
                                    "Monaco"
                                    "Menlo"
                                    "SF Mono"
                                    "Hack"))
         (font-family (seq-find
                       (lambda (elt) (member elt (font-family-list)))
                       preferred-font-families)))
    (set-face-attribute 'default nil :family font-family :weight 'regular)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-mouse-color "white")
    (with-eval-after-load 'linum
      (set-face-attribute 'linum nil :weight 'thin))))

;; Set file, keyboard and terminal coding systems automatically
(prefer-coding-system 'utf-8)

;; Enable initially disabled keys
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove all query on exit flags on all processes before quitting
(advice-add 'save-buffers-kill-emacs :before
            #'(lambda (&rest _)
                (defun processes-with-query (process)
                  (and (memq (process-status process) '(run stop open listen))
                       (process-query-on-exit-flag process)))
                (let ((processes (seq-filter 'processes-with-query (process-list))))
                  (dolist (process processes)
                    (set-process-query-on-exit-flag process nil)))))
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook #'(lambda () (auto-fill-mode 1)))

;; Turn on subword mode and linum mode for all prog and text modes
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'(lambda ()
                     (subword-mode 1)
                     (linum-mode 1))))

;; Renumber the current buffer after reverting the buffer
(add-hook 'after-revert-hook 'linum-update-current)

;; Turn on iMenu for code outlines for all prog and text modes, if possible
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'(lambda () (ignore-errors (imenu-add-menubar-index)))))

;; Save window config before ediff starts and restores it and cleans up when it quits, sanity!
(defvar ediff-saved-window-configuration)
(add-hook 'ediff-before-setup-hook
          #'(lambda () (setq ediff-saved-window-configuration (current-window-configuration))))
(let ((restore-window-configuration
       #'(lambda () (set-window-configuration ediff-saved-window-configuration))))
  (add-hook 'ediff-quit-hook restore-window-configuration 'append)
  (add-hook 'ediff-suspend-hook restore-window-configuration 'append))
(add-hook 'ediff-cleanup-hook
          #'(lambda ()
              (eval-and-compile (require 'ediff-util))
              (ediff-janitor nil nil)) 'append)

;; More sensible comment-dwim
(defun comment-dwim-line-or-region (&optional arg)
  "Replacement for the `comment-dwim' command.

If no region is selected then comment the current line from the
front, otherwise the commentable lines in the region.  Replaces
the default behaviour of `comment-dwim', where it inserts comment
at the end of the line.

Optional argument ARG same as `comment-dwim''s."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (use-region-p)))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(eval-when-compile (require 'use-package))
(require 'bind-key)

;; Unmap extraneous undo-tree mode keys
(assq-delete-all 'undo-tree-mode minor-mode-map-alist)

;; Unbind hide/show mode's ridiculous keybindings
(assq-delete-all 'hs-minor-mode minor-mode-map-alist)

(bind-keys ("M-s f"   . follow-mode)
           ("M-s s"   . align)
           ("M-;"     . comment-dwim-line-or-region)
           ;; Rebind windmove keys
           ("M-s h"   . windmove-left)
           ("M-s l"   . windmove-right)
           ("M-s k"   . windmove-up)
           ("M-s j"   . windmove-down)
           ;; Replace default buffer menu with ibuffer
           ("C-x C-b" . ibuffer)
           ;; Misc
           ("C-x u"   . undo-tree-visualize))

;; Completely unbind annoying abbrev, dabbrev, expand, hippie-expand. These
;; ancient completion commands are just too stupid for this day and age
(unbind-key "M-'")
(unbind-key "M-/")
(unbind-key "C-x '")
;; Always use M-g prefix to jump between errors
(unbind-key "C-x `")

(when (and (display-graphic-p)
           (not (memq (window-system) '(x))))
  (bind-key "<mouse-2>" 'mouse-buffer-menu)
  (unbind-key "<mouse-3>"))

;; Not that I use occur very often, but when I do, I'd like its keybindings the
;; same as grep mode's
(add-hook 'occur-mode-hook
          #'(lambda ()
              (bind-keys :map occur-mode-map
                         ("M-n" . nil)
                         ("M-p" . nil)
                         ("n"   . occur-next)
                         ("p"   . occur-prev))))

;; I use compilation mode more, so of course I have to do the same thing as
;; occur mode
(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook
            #'(lambda ()
                (bind-keys :map compilation-mode-map
                           ("M-n" . nil)
                           ("M-p" . nil)
                           ("n"   . compilation-next-error)
                           ("p"   . compilation-previous-error)))))

;; Completely unbind visual-line-mode's stupid bindings
(add-hook 'visual-line-mode-hook
          #'(lambda ()
              (bind-keys
               :map visual-line-mode-map
               ([remap move-beginning-of-line]   . nil)
               ([remap move-end-of-line]         . nil)
               ([remap beginning-of-visual-line] . move-beginning-of-line)
               ([remap end-of-visual-line]       . move-end-of-line)
               ([remap kill-line]                . nil)
               ([remap next-line]                . next-logical-line)
               ([remap previous-line]            . previous-logical-line))))

;; Sane scrolling
(use-package pager-default-keybindings)

;; Sets $MANPATH, $PATH and exec-path from your shell, but only on OS X and Linux.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Replace the major mode name with its icon and move the buffer name from the
;; mode line to the header line
(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (defun move-buffer-id-to-header-line ()
    (when (window-header-line-height)
      (setq header-line-format 'mode-line-buffer-identification)
      (setq mode-line-format (remove 'mode-line-buffer-identification mode-line-format))))
  (defun replace-mode-with-icon ()
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
      (when (and icon (not (string= major-mode icon)))
        (setq mode-name icon))))
  :config
  (dolist (hook '(window-configuration-change-hook after-change-major-mode-hook))
    (add-hook hook 'move-buffer-id-to-header-line))
  (add-hook 'after-change-major-mode-hook 'replace-mode-with-icon))

;; Use icons in dired
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after all-the-icons
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Turn on keyboard shortcut remainder
(use-package which-key
  :config
  (which-key-mode 1)
  (bind-keys ("C-h b" . which-key-show-top-level)))

;; Modern code folding
(use-package origami
  :config
  (bind-keys ("M-0"   . origami-open-all-nodes)
             ("M-9"   . origami-close-all-nodes)
             ("C-M-/" . origami-recursively-toggle-node)))

;; Enhances ido and isearch's fuzzy search
(use-package flx-ido
  :config
  (setq flx-ido-use-faces nil)
  (flx-ido-mode 1))

(use-package flx-isearch
  :config
  (flx-isearch-mode 1)
  (bind-keys ("C-M-s" . flx-isearch-forward)
             ("C-M-r" . flx-isearch-backward)))

;; Use ido with M-x
(use-package smex
  :config
  (smex-initialize)
  (bind-keys ("M-x" . smex)
             ("M-X" . smex-major-mode-commands)))

;; Use ido for even more things than ido-everywhere
(use-package ido-completing-read+)
(use-package ido-vertical-mode)

;; Mark and edit multiple things at once
(use-package multiple-cursors
  :config
  (bind-keys ("M-s M-e" . mc/edit-lines)
             ("C->"     . mc/mark-next-like-this)
             ("C-<"     . mc/mark-previous-like-this)
             ("C-M->"   . mc/skip-to-next-like-this)
             ("C-M-<"   . mc/skip-to-previous-like-this)
             ("M-s C->" . mc/mark-all-dwim)))

;; Construct regexp and search visually and incrementally
(use-package visual-regexp-steroids
  :after multiple-cursors
  :config
  (bind-keys ("M-%"     . vr/replace)
             ("C-M-%"   . vr/query-replace)
             ("M-s C-s" . vr/isearch-forward)
             ("M-s m"   . vr/mc-mark)))

(use-package expand-region
  :config
  (bind-keys ("M-=" . er/expand-region)
             ("M--" . er/contract-region)))

(use-package smartparens-config
  :init
  (require 'cl)
  (defmacro def-pairs (pairs)
    `(progn
       ,@(loop for (key . val) in pairs
               collect
               `(defun ,(read (concat  "wrap-with-" (prin1-to-string key) "s"))
                    (&optional _arg)
                  (interactive "P")
                  (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren        . "(")
              (bracket      . "[")
              (brace        . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (underscore   . "_")
              (back-quote   . "`")
              (angle        . "<")))

  :config
  (bind-keys :map smartparens-mode-map
             ("C-M-a" . sp-beginning-of-sexp)
             ("C-M-e" . sp-end-of-sexp)

             ("C-M-f" . sp-forward-sexp)
             ("C-M-b" . sp-backward-sexp)

             ("C-M-n" . sp-next-sexp)
             ("C-M-p" . sp-previous-sexp)

             ("C-M-d" . sp-down-sexp)
             ("C-M-u" . sp-backward-up-sexp)
             ("M-S-d" . sp-backward-down-sexp)
             ("M-S-u" . sp-up-sexp)

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

             ("M-[" . sp-backward-unwrap-sexp)
             ("M-]" . sp-unwrap-sexp)

             ("C-c ("   . wrap-with-parens)
             ("C-c ["   . wrap-with-brackets)
             ("C-c {"   . wrap-with-braces)
             ("C-c '"   . wrap-with-single-quotes)
             ("C-c \""  . wrap-with-double-quotes)
             ("C-c _"   . wrap-with-underscores)
             ("C-c `"   . wrap-with-back-quotes)
             ("C-c <"   . wrap-with-angles)))

;; Cycle thru most commonly programming identifier styles
(use-package string-inflection
  :init
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
  (bind-keys ("C-x C-y" . inflect-string)))

;; Cycle between quotes
(use-package cycle-quotes
  :config
  (bind-keys ("C-x C-'" . cycle-quotes)))

;; Vim-like increment and decrement
(use-package evil-numbers
  :config
  (bind-keys ("C-x =" . evil-numbers/inc-at-pt)
             ("C-x -" . evil-numbers/dec-at-pt)))

;; Adjust frame-wide font size
(use-package zoom-frm
  :config
  (bind-keys ("C-x C-+" . zoom-in/out)
             ("C-x C--" . zoom-in/out)
             ("C-x C-=" . zoom-in/out)
             ("C-x C-0" . zoom-in/out)))

;; Quick Snippets
(use-package yasnippet
  :config
  (yas-reload-all)
  (dolist (hook '(prog-mode-hook text-mode))
    (add-hook hook 'yas-minor-mode))
  (bind-keys :map yas-minor-mode-map
             ("TAB"   . nil)
             ("<tab>" . nil)
             ("C-c i" . yas-expand)))

;; Auto-completion
(use-package company
  :config
  (bind-key "M-/" 'company-complete company-mode-map)
  (add-hook 'company-mode-hook
            #'(lambda ()
                (use-package company-statistics
                  :config (company-statistics-mode 1))

                (use-package company-quickhelp
                  :config (company-quickhelp-mode 1))

                (use-package company-flx
                  :config (company-flx-mode 1)))))

;; Much faster PDF viewing
(add-hook 'doc-view-mode-hook
          #'(lambda ()
              (when (fboundp 'pdf-tools-install)
                (pdf-tools-install))))

;; Restclient
(use-package restclient
  :config
  (add-hook 'restclient-mode-hook
            #'(lambda ()
                (use-package company-restclient
                  :after company
                  :config
                  (add-to-list 'company-backends 'company-restclient)))))

;; Lisp
(bind-keys :map emacs-lisp-mode-map
           ("C-c e c" . emacs-lisp-byte-compile)
           ("C-c e l" . emacs-lisp-byte-compile-and-load))

;; Shell mode
(add-hook 'sh-mode-hook
          #'(lambda ()
              (use-package company-shell
                :after company
                :config
                (add-to-list 'company-backend '(company-shell company-shell-env)))))

;; Term and shell
(use-package bash-completion
  :config
  (dolist (hook '(shell-dynamic-complete-functions
                  term-dynamic-complete-functions))
    (add-hook hook #'bash-completion-dynamic-complete)))

(use-package multi-term
  :config
  (bind-keys ("M-T" . multi-term)))

;; YAML mode
(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
            #'(lambda ()
                (use-package flycheck-yamllint
                  :config
                  (flycheck-yamllint-setup)))))

;; Javascript
(add-hook 'js-mode-hook
          #'(lambda ()
              ;; Make sure fontification in js-mode and derived modes is up to
              ;; date with the latest ES2015 keywords
              (font-lock-add-keywords
               nil
               '("\\_<async\\_>"
                 "\\_<await\\_>"
                 ("\\_<import\\_>"
                  ("\\_<as\\_>" nil nil (0 font-lock-keyword-face))
                  ("\\_<from\\_>" nil nil (0 font-lock-keyword-face)))
                 ("\\_<for\\_>" "\\_<of\\_>" nil nil (0 font-lock-keyword-face))))

              (use-package tern
                :config
                (tern-mode 1)
                (unbind-key "C-c C-r" tern-mode-keymap))

              (use-package company-tern
                :after company
                :config
                (add-to-list 'company-backends 'company-tern))

              (use-package js-doc
                :after yasnippet
                :config
                (bind-keys :map js-mode-map
                           ("C-c d" . js-doc-insert-file-doc)
                           ("C-c f" . js-doc-insert-function-doc-snippet)))

              (use-package add-node-modules-path
                :config
                (add-node-modules-path))

              (use-package import-js
                :config
                (run-import-js)
                (bind-keys :map js-mode-map
                           ("C-c t i"   . import-js-import)
                           ("C-c t f"   . import-js-fix)
                           ("C-c t M-." . import-js-goto)))

              (use-package eslintd-fix
                :config
                (eslintd-fix-mode 1)
                (bind-keys :map js-mode-map
                           ("C-c C-f" . eslintd-fix)))

              (use-package js-format
                :config
                (js-format-setup "standard")
                (bind-keys :map js-mode-map
                           ("C-x j b" . js-format-buffer)))

              (use-package nodejs-repl
                :config
                (bind-keys :map js-mode-map
                           ("C-x C-e" . nodejs-repl-send-last-expression)
                           ("C-c r"   . nodejs-repl-send-region)
                           ("C-c b"   . nodejs-repl-send-buffer)
                           ("C-c l"   . nodejs-repl-load-file)
                           ("C-c z"   . nodejs-repl-switch-to-repl)))

              (define-key js-mode-map [menu-bar] nil)))

(use-package js2-mode
  :config
  (add-hook 'js2-mode-hook
            #'(lambda ()
                (bind-keys
                 :map js2-mode-map
                 ("C-k" . sp-kill-whole-line))

                (use-package xref-js2
                  :config
                  (unbind-key "M-." js2-mode-map)
                  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

                (use-package js2-refactor
                  :config
                  (js2-refactor-mode 1)
                  (js2r-add-keybindings-with-prefix "C-c r"))

                (use-package js2-imenu-extras
                  :config
                  (js2-imenu-extras-mode 1)))))

(use-package rjsx-mode
  :mode "\\.jsx?\\'")

;; TypeScript
(use-package typescript-mode)

(use-package ts-comint
  :after typescript-mode
  :config
  (bind-keys :map typescript-mode-map
             ("C-x C-e" . ts-send-last-sexp)
             ("C-M-x"   . ts-send-last-sexp-and-go)
             ("C-c r"   . ts-send-region-and-go)
             ("C-c b"   . ts-send-buffer-and-go)
             ("C-c l"   . ts-load-file-and-go)
             ("C-c z"   . switch-to-ts)))

(use-package tide
  :after typescript-mode
  :config
  (add-hook 'typescript-mode-hook
            #'(lambda ()
                (tide-setup)
                (tide-hl-identifier-mode 1)))

  (add-hook 'before-save-hook 'tide-format-before-save)

  (bind-keys :map typescript-mode-map
             ("C-c C-f" . tide-format)
             ("C-c n"   . tide-rename-symbol)
             ("M-1"     . tide-fix)
             ("M-?"     . tide-references)
             ("C-h o"   . tide-documentation-at-point)))

;; Python
(use-package pyenv-mode
  :config
  (bind-keys :map pyenv-mode-map
             ("C-c C-s" . nil)
             ("C-c C-u" . nil)))

(add-hook 'python-mode-hook
          #'(lambda ()
              (use-package anaconda-mode
                :after company-mode
                :config
                (anaconda-mode 1)
                (anaconda-eldoc-mode 1)
                (add-to-list 'company-backends '(company-anaconda :with company-capf))
                (bind-keys :map anaconda-mode-map
                           ("M-r"   . nil)
                           ("M-\""  . anaconda-mode-find-assignments)
                           ("M-?"   . anaconda-mode-find-references)
                           ("M-,"   . anaconda-mode-go-back)
                           ("C-h o" . anaconda-mode-show-doc)))

              (use-package py-autopep8
                :config (py-autopep8-enable-on-save))

              (use-package python-docstring
                :config (python-docstring-mode 1))

              (use-package importmagic
                :config
                (setq importmagic-be-quiet t)
                (importmagic-mode 1)
                (bind-keys :map importmagic-mode-map
                           ("M-1" . importmagic-fix-imports)))

              (use-package py-isort
                :config
                (bind-keys :map python-mode-map
                           ("C-c s" . py-isort-buffer)))))

;; Go
(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            #'(lambda ()
                (use-package company-go
                  :after company
                  :config
                  (setq-local company-backends '(company-go)))

                (use-package go-eldoc :config (go-eldoc-setup)))))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package racer
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package cargo
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

;; Web stuff
(use-package web-mode
  :functions web-mode-language-at-pos
  :mode ("\\.tsx\\'"
         "\\.handlebars\\'"
         "\\.underscore\\'"
         "\\.css\\'"
         "\\.html?\\'"
         "\\.jinja'"
         "\\.mako\\'"
         "\\.dtl\\'"
         "\\.jsp\\'"
         "\\.soy\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'")
  :config
  (add-hook 'web-mode-hook
            #'(lambda ()
                (use-package tern
                  :config
                  (unbind-key "C-c C-r" tern-mode-keymap))

                (use-package company-tern
                  :after company tern
                  :config
                  (setq-local company-backends
                              '(company-tern company-yasnippet company-files))

                  (advice-add 'company-tern :before
                              #'(lambda (&rest _)
                                  (if (eq major-mode 'web-mode)
                                      (let ((web-mode-cur-language
                                             (web-mode-language-at-pos)))
                                        (if (or (string= web-mode-cur-language "javascript")
                                                (string= web-mode-cur-language "jsx"))
                                            (unless tern-mode (tern-mode))
                                          (if tern-mode (tern-mode -1))))))))

                (use-package company-web-html
                  :after company
                  :config
                  (setq-local company-backends
                              '(company-tern company-web-html company-yasnippet company-files)))

                (when (and (string-equal "tsx" (file-name-extension buffer-file-name))
                           (featurep 'tide))
                  (flycheck-add-mode 'typescript-tslint 'web-mode)
                  (tide-setup)
                  (tide-hl-identifier-mode 1)))))

(use-package emmet-mode
  :after web-mode
  :config
  (dolist (hook '(sgml-mode-hook
                  nxml-mode-hook
                  web-mode-hook
                  js-jsx-mode-hook
                  js2-jsx-mode
                  rjsx-mode-hook))
    (add-hook hook #'(lambda ()
                       (emmet-mode 1)
                       (when (or (member major-mode '(js-jsx-mode js2-jsx-mode rjsx-mode))
                                 (and (eq major-mode 'web-mode)
                                      (string= (web-mode-language-at-pos) "jsx")))
                         (setq-local emmet-expand-jsx-className? t))))))

;; Project management, version control, file search and browser
(use-package projectile
  :after pyenv-mode
  :init (defun projectile-pyenv-mode-set ()
          "Set pyenv version matching project name."
          (let ((project (projectile-project-name)))
            (if (member project (pyenv-mode-versions))
                (pyenv-mode-set project)
              (pyenv-mode-unset))))
  :config
  (projectile-mode 1)
  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set))

(use-package go-projectile
  :after go-mode projectile)

;; Second fastest but the best find grep in terms of tooling support
(use-package ag
  :config
  (bind-keys ("M-s a" . ag)))

(use-package wgrep-ag
  :after ag)

;; Currently the fastest find grep
(use-package rg
  :after wgrep-ag projectile
  :config
  (add-hook 'rg-mode-hook
            #'(lambda ()
                (next-error-follow-minor-mode 0)
                (wgrep-ag-setup)))
  (bind-keys ("M-s r" . rg))
  (bind-keys :map projectile-command-map
             ("s r" . rp-project)))

;; Git
(use-package magit
  :config
  (bind-keys ("C-x v C-g" . magit-status)))

;; Hg
(use-package monky
  :config
  (bind-keys ("C-x v C-h" . monky-status)))

(use-package treemacs
  :config
  (setq treemacs-icon-fallback-text (propertize "  " 'face 'font-lock-keyword-face))
  (bind-keys ("<f12>"     . treemacs-toggle)
             ("C-x t o"   . treemacs-select-window)
             ("C-x t 1"   . treemacs-delete-other-windows)
             ("C-x t t"   . treemacs-toggle)
             ("C-x t T"   . treemacs)
             ("C-x t b"   . treemacs-bookmark)
             ("C-x t C-f" . treemacs-find-file)
             ("C-x t C-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  (add-hook 'projectile-find-file-hook #'treemacs-find-file)
  (add-hook 'projectile-after-switch-project-hook #'treemacs-projectile)
  (bind-keys ("C-x t P" . treemacs-projectile)
             ("C-x t p" . treemacs-projectile-toggle)))
