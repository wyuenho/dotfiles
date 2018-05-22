;;; -*- lexical-binding: t -*-

;; (setq edebug-on-error t)

;; So `edebug' will print something useful as opposed to some bytecode hex
(fset 'edebug-prin1-to-string 'prin1-to-string)

;; A bug in the mac port saves the mouse color when `frameset-save' is called,
;; but it's not desirable on macOS because the window server will decide the
;; color of the cursor according to the background color.
(when (memq (window-system) '(mac))
  (add-to-list 'frameset-filter-alist '(mouse-color . :never)))

;; Emacs 26 ns port new settings
(dolist (pair '((ns-transparent-titlebar . t) (ns-appearance . dark)))
  (push pair (alist-get 'ns window-system-default-frame-alist nil))
  (set-frame-parameter nil (car pair) (cdr pair)))
(setq frame-title-format nil
      ns-use-proxy-icon nil
      ns-use-thin-smoothing t
      ns-use-mwheel-momentum t
      ns-use-mwheel-acceleration t)

;; Stop asking me if a theme is safe. The entirety of Emacs is built around
;; evaling arbitrary code...
(advice-add 'load-theme :around (lambda (old-load-theme &rest args)
                                  (apply old-load-theme (car args) t (cddr args))))

;; Emacs loads init file first and the packages last normally. Forcing the
;; packages to load first makes configuring them in the init file possible.
(when (< emacs-major-version 26)
  (package-initialize))

;; Tell Custom to write and find the custom settings elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Maximize frame on startup and set up default fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :family "Noto Sans Mono" :weight 'regular :width 'normal)
  (set-frame-parameter nil 'fullscreen 'maximized)
  (with-eval-after-load 'linum
    (set-face-attribute 'linum nil :weight 'thin)))

;; Set file, keyboard and terminal coding systems automatically
(prefer-coding-system 'utf-8)

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p #'y-or-n-p)

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

;; Only turn on `auto-revert-mode' for Mac on Emacs >= 26 because kqueue file
;; notification is broken for Emacs < 26
(when (and (>= emacs-major-version 26)
           (memq (window-system) '(mac ns)))
  (global-auto-revert-mode t))

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook (lambda () (auto-fill-mode t)))

;; Turn on subword mode and linum mode for all prog and text modes
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (subword-mode t) (linum-mode t))))

;; Renumber the current buffer after reverting the buffer
(add-hook 'after-revert-hook 'linum-update-current)

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

(eval-when-compile (require 'use-package))
;; (setq use-package-compute-statistics t)
(require 'bind-key)
(require 'quelpa-use-package)

;; I hate X mouse bindings
(when (and (display-graphic-p)
           (not (memq (window-system) '(x))))
  (bind-key "<mouse-3>" 'mouse-buffer-menu))

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
(bind-keys ("C-x C-u"   . upcase-region-dwim)
           ("C-x C-l"   . downcase-region-dwim)
           ("C-x f"     . follow-mode)
           ("<backtab>" . align)
           ;; Replace default buffer menu with ibuffer
           ("C-x C-b"   . ibuffer))

;; Replace zap-to-char with the hidden zap-up-to-char
(autoload 'zap-up-to-char "misc")
(bind-key "M-z" 'zap-up-to-char)

(use-package undo-tree
  :config
  ;; Unmap extraneous undo-tree mode keys
  (assq-delete-all 'undo-tree-mode minor-mode-map-alist)
  ;; Map the usual undo key to undo-tree-visualize
  (bind-key "C-x u" 'undo-tree-visualize))

;; Quiet down noisy config gracefully
(use-package shut-up)

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
;; (add-hook 'visual-line-mode-hook
;;           (lambda ()
;;             (bind-keys
;;              :map visual-line-mode-map
;;              ([remap move-beginning-of-line]   . nil)
;;              ([remap move-end-of-line]         . nil)
;;              ([remap beginning-of-visual-line] . move-beginning-of-line)
;;              ([remap end-of-visual-line]       . move-end-of-line)
;;              ([remap kill-line]                . nil)
;;              ([remap next-line]                . next-logical-line)
;;              ([remap previous-line]            . previous-logical-line))))

;; Sane keyboard scrolling
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
              (all-the-icons-dired-mode t))))

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
  :commands dired-collapse-mode
  :hook (dired-mode . dired-collapse-mode))

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
             (subword-mode nil subword))))

;; Adjust frame-wide font size
(use-package default-text-scale
  :bind (("C-x C-=" . default-text-scale-increase)
         ("C-x C--" . default-text-scale-decrease)
         ("C-x C-0" . default-text-scale-reset)))

;; Turn on keyboard shortcut remainder
(use-package which-key
  :delight
  :bind (("C-h b" . which-key-show-top-level)))

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

                          ("M-[" . sp-backward-unwrap-sexp)
                          ("M-]" . sp-unwrap-sexp)))))

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

;; Modern code folding
(use-package origami
  :config
  ;; Unbind hide/show mode's ridiculous keybindings
  (assq-delete-all 'hs-minor-mode minor-mode-map-alist)
  :bind (("M-0"   . origami-open-all-nodes)
         ("M-9"   . origami-close-all-nodes)
         ("C-M-/" . origami-recursively-toggle-node)))

;; Auto-completion
(use-package company
  :delight
  :bind (:map company-mode-map
              ("M-/" . company-manual-begin))
  :config
  (add-hook 'company-mode-hook
            (lambda ()
              (use-package company-statistics
                :config (company-statistics-mode t))

              (use-package company-quickhelp
                :config (company-quickhelp-mode t))

              (use-package company-flx
                :config (company-flx-mode t)))))

;; Linting
(with-eval-after-load 'flycheck
  (when (featurep 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
  (when (featurep 'flycheck-pos-tip)
    (flycheck-pos-tip-mode)))

;; Much faster PDF viewing
(add-hook 'doc-view-mode-hook
          (lambda ()
            (when (fboundp 'pdf-tools-install)
              (pdf-tools-install))))

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
                (add-to-list 'company-backends 'company-restclient)))))

;; Lisp
(bind-keys :map emacs-lisp-mode-map
           ("C-c e f" . byte-compile-file)
           ("C-c e c" . emacs-lisp-byte-compile)
           ("C-c e l" . emacs-lisp-byte-compile-and-load))

;; Term and shell
(add-hook 'sh-mode-hook
          (lambda ()
            (use-package company-shell
              :after company
              :config
              (add-to-list 'company-backend '(company-shell company-shell-env)))))

(use-package multi-term
  :bind (("M-T" . multi-term)))

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
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
            ;; (font-lock-add-keywords
            ;;  nil
            ;;  '("\\_<async\\_>"
            ;;    "\\_<await\\_>"
            ;;    ("\\_<import\\_>"
            ;;     ("\\_<as\\_>" nil nil (0 font-lock-keyword-face))
            ;;     ("\\_<from\\_>" nil nil (0 font-lock-keyword-face)))
            ;;    ("\\_<for\\_>" "\\_<of\\_>" nil nil (0 font-lock-keyword-face))))

            (use-package tern
              :delight
              :config
              (tern-mode t)
              (unbind-key "C-c C-r" tern-mode-keymap))

            (use-package company-tern
              :after company
              :config
              (add-to-list 'company-backends 'company-tern))

            (use-package js-doc
              :after yasnippet
              :bind (:map js-mode-map
                          ("C-c d m" . js-doc-insert-file-doc)
                          ("C-c d f" . js-doc-insert-function-doc-snippet)))

            (use-package add-node-modules-path
              :config
              (add-node-modules-path))

            (use-package import-js
              :config
              (run-import-js)
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
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (js2-imenu-extras-mode t)

              (bind-keys
               :map js2-mode-map
               ("C-k" . sp-kill-whole-line))

              (use-package js2-refactor
                :delight
                :config
                (js2-refactor-mode t)
                (js2r-add-keybindings-with-prefix "C-c r")))))

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
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-setup)
              (tide-hl-identifier-mode t)))

  (add-hook 'before-save-hook 'tide-format-before-save)

  :bind (:map typescript-mode-map
              ("C-c f" . tide-format)
              ("C-c n" . tide-rename-symbol)
              ("M-1"   . tide-fix)
              ("M-?"   . tide-references)
              ("C-h o" . tide-documentation-at-point)))

;; Python
(use-package pyenv-mode
  :bind (:map pyenv-mode-map
              ("C-c C-s" . nil)
              ("C-c C-u" . nil)))

(add-hook 'python-mode-hook
          (lambda ()
            (use-package pipenv
              :delight
              :after projectile
              :config
              (pipenv-mode t)
              (pipenv-activate))

            (use-package anaconda-mode
              :delight
              :after company
              :config
              (anaconda-mode t)
              (anaconda-eldoc-mode t)
              (add-to-list 'company-backends '(company-anaconda :with company-capf))
              :bind (:map anaconda-mode-map
                          ("M-r"   . nil)
                          ("M-\""  . anaconda-mode-find-assignments)
                          ("M-?"   . anaconda-mode-find-references)
                          ("M-,"   . anaconda-mode-go-back)
                          ("C-h o" . anaconda-mode-show-doc)))

            (use-package py-autopep8
              :config (py-autopep8-enable-on-save))

            (use-package python-docstring
              :delight
              :config (python-docstring-mode t))

            (use-package importmagic
              :delight
              :config
              (importmagic-mode t)
              :bind (:map importmagic-mode-map
                          ("M-1" . importmagic-fix-imports)))

            (use-package py-isort
              :bind (:map python-mode-map
                          ("C-c s" . py-isort-buffer)))))

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

;; HTML templates and CSS
(use-package scss-mode
  :mode "\\.scss\\'")

(use-package rainbow-mode
  :delight
  :after scss-mode
  :hook (css-mode scss-mode))

(use-package web-mode
  :functions web-mode-language-at-pos
  :mode ("\\.vue\\'"
         "\\.tsx\\'"
         "\\.handlebars\\'"
         "\\.underscore\\'"
         "\\.html?\\'"
         ;; "\\.s?css\\'"
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
                            '(company-tern company-yasnippet company-files))

                (advice-add 'company-tern :before
                            (lambda (&rest _)
                              (if (eq major-mode 'web-mode)
                                  (let ((web-mode-cur-language
                                         (web-mode-language-at-pos)))
                                    (if (or (string= web-mode-cur-language "javascript")
                                            (string= web-mode-cur-language "jsx"))
                                        (unless tern-mode (tern-mode))
                                      (if tern-mode (tern-mode -t))))))))

              (use-package company-web-html
                :after company
                :config
                (setq-local company-backends
                            '(company-web-html company-tern company-yasnippet company-files)))

              (when (and (string-equal "tsx" (file-name-extension buffer-file-name))
                         (featurep 'tide))
                (flycheck-add-mode 'typescript-tslint 'web-mode)
                (tide-setup)
                (tide-hl-identifier-mode t))

              (when (string-equal "css" (file-name-extension buffer-file-name))
                (flycheck-add-mode 'css-stylelint 'web-mode))

              (when (string-equal "scss" (file-name-extension buffer-file-name))
                (flycheck-add-mode 'scss-stylelint 'web-mode)))))

(use-package emmet-mode
  :delight
  :after (:any web-mode js2-mode rjsx-mode)
  :hook (sgml-mode nxml-mode web-mode js-jsx-mode js2-jsx-mode rjsx-mode)
  :config
  (add-hook 'emmet-mode-hook
            (lambda ()
              (when (or (member major-mode '(js-jsx-mode js2-jsx-mode rjsx-mode))
                        (and (eq major-mode 'web-mode)
                             (string= (web-mode-language-at-pos) "jsx")))
                (setq-local emmet-expand-jsx-className? t)))))

;; Project management, version control, file search and browser
(use-package projectile
  :after pyenv-mode
  :preface
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))
  :config
  (projectile-mode t)
  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set))

(use-package go-projectile
  :after go-mode projectile)

;; Second fastest but the best find grep in terms of tooling support
(use-package ag
  :bind (("M-s a" . ag)))

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
  :bind (("M-s r" . rg)
         :map projectile-command-map
         ("s r" . rg-project)))

(use-package dumb-jump)

(use-package dumb-diff)

;; Git
(use-package magit
  :bind (("C-x v C-g" . magit-status)))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))

;; Hg
(use-package monky
  :bind (("C-x v C-h" . monky-status)))

;; Saner window management
(use-package imenu-list
  :quelpa (imenu-list :fetcher github :repo "wyuenho/imenu-list" :branch "clear-buffer"))

(use-package window-purpose
  :preface
  (defun purpose-after-init ()
    (when (file-exists-p purpose-default-layout-file)
      (purpose-load-window-layout-file))
    (select-window (get-largest-window))
    (purpose-x-code1-update-changed)
    (remove-hook 'after-init-hook 'purpose-after-init))
  :quelpa (window-purpose
           :fetcher github
           :repo "wyuenho/emacs-purpose"
           :branch "improve-code1"
           :files (:defaults "layouts"))
  :after magit undo-tree imenu-list
  :config

  ;; Pending https://github.com/bmag/emacs-purpose/pull/116
  (advice-add 'undo-tree-visualize :after
              (lambda (&rest _)
                (let ((window (get-buffer-window undo-tree-visualizer-buffer-name)))
                  (set-window-dedicated-p window 'soft))))

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

  (add-hook 'after-init-hook 'purpose-after-init t))

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
  (set-face-attribute 'header-line nil :box nil))
