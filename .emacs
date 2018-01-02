;; Stop asking me if a theme is safe. The entirety of Emacs is built around
;; evaling arbitrary code...
(advice-add 'load-theme :around (lambda (old-load-theme &rest args)
                                  (apply old-load-theme (car args) t (cddr args))))

;; Emacs loads init file first and the packages last normally. Forcing the
;; packages to load first makes configuring them in the init file possible.
(package-initialize)

;; Tell Custom to write and find the custom settings elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Maximize frame on startup and set up default fonts
(when (display-graphic-p)
  (let ((preferred-font-families '("Noto Sans Mono"
                                   "Roboto Mono"
                                   "DejaVu Sans Mono"
                                   "Bitstream Vera Sans Mono"
                                   "Fira Mono"
                                   "Monaco"
                                   "Menlo"
                                   "SF Mono"
                                   "Hack")))
    (set-face-attribute 'default nil :family (car preferred-font-families) :weight 'regular)
    (add-to-list 'face-font-family-alternatives preferred-font-families)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (with-eval-after-load 'linum
      (set-face-attribute 'linum nil :weight 'thin))))

;; A bug in the mac port saves the mouse color when `frameset-save' is called,
;; but it's not desirable on macOS because the window server will decide the
;; color of the cursor according to the background color.
(when (memq (window-system) '(mac))
  (add-to-list 'frameset-filter-alist '(mouse-color . :never)))

;; Set file, keyboard and terminal coding systems automatically
(prefer-coding-system 'utf-8)

;; Enable initially disabled keys
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove all query on exit flags on all processes before quitting
(advice-add 'save-buffers-kill-emacs :before
            (lambda (&rest _)
              (defun processes-with-query (process)
                (and (memq (process-status process) '(run stop open listen))
                     (process-query-on-exit-flag process)))
              (let ((processes (seq-filter 'processes-with-query (process-list))))
                (dolist (process processes)
                  (set-process-query-on-exit-flag process nil)))))
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))

;; Turn on subword mode and linum mode for all prog and text modes
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (subword-mode 1) (linum-mode 1))))

;; Renumber the current buffer after reverting the buffer
(add-hook 'after-revert-hook 'linum-update-current)

;; Turn on iMenu for code outlines for all prog and text modes, if possible
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (ignore-errors (imenu-add-menubar-index)))))

;; Save window config before ediff starts and restores it and cleans up when it quits, sanity!
(defvar ediff-saved-window-configuration)
(add-hook 'ediff-before-setup-hook
          (lambda () (setq ediff-saved-window-configuration (current-window-configuration))))
(let ((restore-window-configuration
       (lambda () (set-window-configuration ediff-saved-window-configuration))))
  (add-hook 'ediff-quit-hook restore-window-configuration 'append)
  (add-hook 'ediff-suspend-hook restore-window-configuration 'append))
(add-hook 'ediff-cleanup-hook
          (lambda ()
            (eval-and-compile (require 'ediff-util))
            (ediff-janitor nil nil)) 'append)

;; Move the buffer name from the mode line to the header line
(dolist (hook '(window-configuration-change-hook after-change-major-mode-hook))
  (add-hook hook (lambda ()
                   (when (window-header-line-height)
                     (setq header-line-format 'mode-line-buffer-identification)
                     (setq mode-line-format (remove 'mode-line-buffer-identification mode-line-format))))))

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

;; I hate X mouse bindings
(when (and (display-graphic-p)
           (not (memq (window-system) '(x))))
  (bind-key "<mouse-2>" 'mouse-buffer-menu)
  (unbind-key "<mouse-3>"))

;; Completely unbind annoying abbrev, dabbrev, expand, hippie-expand. These
;; ancient completion commands are just too stupid for this day and age
(unbind-key "M-'")
(unbind-key "M-/")
(unbind-key "C-x '")
;; Always use M-g prefix to jump between errors
(unbind-key "C-x `")

;; Bind  useful things to keys
(bind-keys ("C-x f"     . follow-mode)
           ("<backtab>" . align)
           ("M-;"       . comment-dwim-line-or-region)
           ;; Replace default buffer menu with ibuffer
           ("C-x C-b"   . ibuffer))

(use-package undo-tree
  :config
  ;; Unmap extraneous undo-tree mode keys
  (assq-delete-all 'undo-tree-mode minor-mode-map-alist)
  ;; Map the usual undo key to undo-tree-visualize
  (bind-key "C-x u" 'undo-tree-visualize))

;; Move around windows with shifted arrow keys
(use-package windmove
  :config
  (windmove-default-keybindings))

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
(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook
            (lambda ()
              (bind-keys :map compilation-mode-map
                         ("M-n" . nil)
                         ("M-p" . nil)
                         ("n"   . compilation-next-error)
                         ("p"   . compilation-previous-error)))))

;; Completely unbind visual-line-mode's stupid bindings
(add-hook 'visual-line-mode-hook
          (lambda ()
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
  :if (memq (window-system) '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Replace the major mode name with its icon and
(use-package all-the-icons
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (let ((icon (all-the-icons-icon-for-mode major-mode)))
                (when (and icon (not (string= major-mode icon)))
                  (setq mode-name icon))))))

;; Saner dired
(use-package dired
  :config
  (add-minor-mode 'dired-hide-details-mode ""))

(use-package all-the-icons-dired
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (all-the-icons-dired-mode 1))))

(use-package dired-hide-dotfiles
  :config
  (bind-keys :map dired-mode-map
             ("." . dired-hide-dotfiles-mode)))

(use-package dired-single
  :after dired-hide-dotfiles
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
                  result)))
  (bind-keys :map dired-mode-map
             ("^"         . (lambda () (interactive) (dired-single-buffer "..")))
             ("<mouse-1>" . dired-single-buffer-mouse)
             ("\C-m"      . dired-single-buffer)))

(use-package dired-collapse
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-collapse-mode 1))))

;; Turn off useless mode lighters
(use-package delight
  :config
  (delight '((move-dup-mode nil move-dup)
             (smartparens-mode nil smartparens)
             (which-key-mode nil which-key)
             (whitespace-cleanup-mode nil whitespace)
             (undo-tree-mode nil undo-tree)
             (auto-revert-mode nil autorevert)
             (visual-line-mode nil simple)
             (subword-mode nil subword)
             (rainbow-mode nil rainbow-mode))))

;; Adjust frame-wide font size
(use-package zoom-frm
  :config
  (bind-keys ("C-x C-+" . zoom-in/out)
             ("C-x C--" . zoom-in/out)
             ("C-x C-=" . zoom-in/out)
             ("C-x C-0" . zoom-in/out)))

;; Turn on keyboard shortcut remainder
(use-package which-key
  :delight
  :config
  (bind-keys ("C-h b" . which-key-show-top-level)))

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

;; Quick Snippets
(use-package yasnippet
  :delight yas-minor-mode
  :config
  (yas-reload-all)
  (dolist (hook '(prog-mode-hook text-mode))
    (add-hook hook 'yas-minor-mode))
  (bind-keys :map yas-minor-mode-map
             ("TAB"   . nil)
             ("<tab>" . nil)
             ("C-c i" . yas-expand)))

;; Modern code folding
(use-package origami
  :config
  ;; Unbind hide/show mode's ridiculous keybindings
  (assq-delete-all 'hs-minor-mode minor-mode-map-alist)
  (bind-keys ("M-0"   . origami-open-all-nodes)
             ("M-9"   . origami-close-all-nodes)
             ("C-M-/" . origami-recursively-toggle-node)))

;; Auto-completion
(use-package company
  :delight
  :config
  (bind-key "M-/" 'company-complete company-mode-map)
  (add-hook 'company-mode-hook
            (lambda ()
              (use-package company-statistics
                :config (company-statistics-mode 1))

              (use-package company-quickhelp
                :config (company-quickhelp-mode 1))

              (use-package company-flx
                :config (company-flx-mode 1)))))

;; Much faster PDF viewing
(add-hook 'doc-view-mode-hook
          (lambda ()
            (when (fboundp 'pdf-tools-install)
              (pdf-tools-install))))

;; Restclient
(use-package restclient
  :config
  (add-hook 'restclient-mode-hook
            (lambda ()
              (use-package company-restclient
                :after company
                :config
                (add-to-list 'company-backends 'company-restclient)))))

;; Lisp
(bind-keys :map emacs-lisp-mode-map
           ("C-c e f" . byte-compile-file)
           ("C-c e c" . emacs-lisp-byte-compile)
           ("C-c e l" . emacs-lisp-byte-compile-and-load))

;; Shell mode
(add-hook 'sh-mode-hook
          (lambda ()
            (use-package company-shell
              :after company
              :config
              (add-to-list 'company-backend '(company-shell company-shell-env)))))

;; Term and shell
(use-package bash-completion
  :config
  (dolist (hook '(shell-dynamic-complete-functions
                  term-dynamic-complete-functions))
    (add-hook hook 'bash-completion-dynamic-complete)))

(use-package multi-term
  :config
  (bind-keys ("M-T" . multi-term)))

;; YAML mode
(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (use-package flycheck-yamllint
                :config
                (flycheck-yamllint-setup)))))

;; Javascript
(add-hook 'js-mode-hook
          (lambda ()
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
              :delight
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
                         (prettier-js-mode 1)
                         (bind-keys :map js-mode-map
                                    ("C-c C-f" . prettier-js))))

                      ((eq style 'eslint)
                       (use-package eslintd-fix
                         :delight
                         :config
                         (eslintd-fix-mode 1)
                         (bind-keys :map js-mode-map
                                    ("C-c C-f" . eslintd-fix))))

                      ((memq style '(esfmt airbnb standard))
                       (use-package js-format
                         :config
                         (js-format-setup (symbol-name (find-js-format-style)))
                         (bind-keys :map js-mode-map
                                    ("C-c C-f" . js-format-buffer)))))))

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
            (lambda ()
              (bind-keys
               :map js2-mode-map
               ("C-k" . sp-kill-whole-line))

              (use-package xref-js2
                :config
                (unbind-key "M-." js2-mode-map)
                (add-hook 'xref-backend-functions 'xref-js2-xref-backend nil t))

              (use-package js2-refactor
                :delight
                :config
                (js2-refactor-mode 1)
                (js2r-add-keybindings-with-prefix "C-c r"))

              (use-package js2-imenu-extras
                :delight
                :config
                (js2-imenu-extras-mode 1)))))

(use-package rjsx-mode
  :mode ("\\.jsx?\\'" "\\.mjs\\'"))

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
            (lambda ()
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
          (lambda ()
            (use-package anaconda-mode
              :delight
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
              :delight
              :config (python-docstring-mode 1))

            (use-package importmagic
              :delight
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
            (lambda ()
              (use-package company-go
                :after company
                :config
                (setq-local company-backends '(company-go)))

              (use-package go-eldoc :config (go-eldoc-setup)))))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package racer
  :delight
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'racer-mode-hook 'company-mode))

(use-package cargo
  :delight
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

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
            (lambda ()
              (use-package tern
                :delight
                :config
                (unbind-key "C-c C-r" tern-mode-keymap))

              (use-package company-tern
                :after company tern
                :config
                (setq-local company-backends
                            '(company-tern company-yasnippet company-files))

                (advice-add 'company-tern :before
                            (lambda (&rest _)
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
  :delight
  :after web-mode
  :config
  (dolist (hook '(sgml-mode-hook
                  nxml-mode-hook
                  web-mode-hook
                  js-jsx-mode-hook
                  js2-jsx-mode
                  rjsx-mode-hook))
    (add-hook hook (lambda ()
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
            (lambda ()
              (next-error-follow-minor-mode 0)
              (wgrep-ag-setup)))
  (bind-keys ("M-s r" . rg))
  (bind-keys :map projectile-command-map
             ("s r" . rg-project)))

;; Git
(use-package magit
  :config
  (bind-keys ("C-x v C-g" . magit-status)))

;; Hg
(use-package monky
  :config
  (bind-keys ("C-x v C-h" . monky-status)))

;; Saner window management
(use-package window-purpose
  :after magit undo-tree
  :config

  ;; Pending https://github.com/bmag/emacs-purpose/pull/116
  (advice-add 'undo-tree-visualize :after
              (lambda (&rest _)
                (let ((window (get-buffer-window undo-tree-visualizer-buffer-name)))
                  (set-window-dedicated-p window 'soft))))

  ;; Pending https://github.com/bmag/emacs-purpose/pull/117
  (defvar purpose-x-code1-dired-buffer-name "*Files*")
  (setq purpose-x-code1--window-layout
        '(nil
          (0 0 152 35)
          (t
           (0 0 29 35)
           (:purpose code1-dired :purpose-dedicated t :width 0.16 :height 0.5 :edges
                     (0.0 0.0 0.19333333333333333 0.5))
           (:purpose buffers :purpose-dedicated t :width 0.16 :height 0.4722222222222222 :edges
                     (0.0 0.5 0.19333333333333333 0.9722222222222222)))
          (:purpose edit :purpose-dedicated t :width 0.6 :height 0.9722222222222222 :edges
                    (0.19333333333333333 0.0 0.8266666666666667 0.9722222222222222))
          (:purpose ilist :purpose-dedicated t :width 0.15333333333333332 :height 0.9722222222222222 :edges
                    (0.8266666666666667 0.0 1.0133333333333334 0.9722222222222222))))

  (setq purpose-x-code1-purpose-config
        (purpose-conf "purpose-x-code1"
                      :mode-purposes
                      '((ibuffer-mode . buffers)
                        (imenu-list-major-mode . ilist))
                      :name-purposes
                      `((,purpose-x-code1-dired-buffer-name . code1-dired))))

  (advice-add 'purpose-x-code1-update-dired :override
              (lambda ()
                (save-selected-window
                  (let ((file-path (buffer-file-name)))
                    (when (and file-path
                               (cl-delete-if #'window-dedicated-p
                                             (purpose-windows-with-purpose 'code1-dired)))
                      ;; Prevents immediately closing the newly created popup help window
                      (letf (((symbol-value 'purpose-select-buffer-hook) nil))
                        (let ((buffer (dired-noselect (file-name-directory file-path))))
                          ;; Make sure code1 only creates 1 dired buffer
                          (dolist (other-buf (purpose-buffers-with-purpose 'code1-dired))
                            (when (and (not (eq buffer other-buf))
                                       (not (string= (buffer-name other-buf)
                                                     (purpose--dummy-buffer-name 'code1-dired))))
                              (kill-buffer other-buf)))
                          (with-current-buffer buffer
                            (rename-buffer purpose-x-code1-dired-buffer-name))
                          (switch-to-buffer buffer)
                          (dired-goto-file file-path)
                          (when (fboundp 'dired-hide-details-mode)
                            (when (not (assq 'dired-hide-details-mode minor-mode-alist))
                              (add-minor-mode 'dired-hide-details-mode ""))
                            (dired-hide-details-mode))
                          (bury-buffer (current-buffer)))))))))

  (advice-add 'purpose-x-code1-update-change :override
              (lambda ()
                (when (and
                       (frame-or-buffer-changed-p 'purpose-x-code1-buffers-changed)
                       (not (memq (purpose-buffer-purpose (current-buffer)) '(code1-dired buffers ilist)))
                       (not (minibufferp)))
                  (purpose-x-code1-update-dired)
                  (imenu-list-update-safe))))

  (purpose-x-code1-setup)
  (purpose-x-popwin-setup)
  (purpose-x-kill-setup)
  (purpose-x-magit-single-on)
  (purpose-add-user-purposes :modes '((ag-mode . search) (rg-mode . search))))

;; Customize solarized theme
(use-package solarized-theme
  :config
  (face-spec-set 'region '((((class color) (min-colors 89) (background light))
                            :background "#00629D" :foreground "#FDF6E3")
                           (((class color) (min-colors 89) (background dark))
                            :background "#69B7F0" :foreground "#002B36")))
  (set-face-background 'flycheck-fringe-error nil)
  (set-face-background 'flycheck-fringe-info nil)
  (set-face-background 'flycheck-fringe-warning nil)
  (set-face-attribute 'mode-line nil :overline nil :underline nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :overline nil :underline nil :box nil)
  (set-face-attribute 'header-line nil :overline nil :underline nil :box nil))
