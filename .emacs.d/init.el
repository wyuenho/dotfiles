;; -*- lexical-binding: t -*-

;; Stop asking me if a theme is safe. The entirety of Emacs is built around
;; evaling arbitrary code...
(advice-add 'load-theme :around #'(lambda (old-load-theme &rest r)
                                    (apply old-load-theme (car r) t (cddr r))))

;; Emacs loads init file first and the packages last normally. Forcing the
;; packages to load first makes conifguring them in the init file possible.
(package-initialize)

;; Tell Custom to write and find the custom settings elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Maximize frame on startup and set up default font
(when (window-system)
  (set-frame-font "DejaVu Sans Mono")
  (set-frame-parameter nil 'fullscreen 'maximized))

(prefer-coding-system 'utf-8)

;; Enable initially disabled keys
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook #'(lambda () (auto-fill-mode t)))

;; Turn on linum mode for all prog and text modes
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'(lambda () (linum-mode t))))
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

;; Kill all running subprocesses automatically when emacs is killed
(add-hook 'kill-emacs-query-functions
          #'(lambda ()
              (dolist (process (process-list))
                (set-process-query-on-exit-flag process nil))
              t))

;; More sensible comment-dwim
(defun comment-dwim-line-or-region (&optional arg)
  "Replacement for the `comment-dwim' command.

If no region is selected then comment the current line from the
front, otherwise the commentable lines in the region. Replaces
the default behaviour of `comment-dwim', where it inserts comment
at the end of the line.

Optional argument ARG same as `comment-dwim''s."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (use-region-p)))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(bind-keys
 ("C-c a"       . align)
 ("M-;"         . comment-dwim-line-or-region)
 ;; Rebind windmove keys
 ("C-c <left>"  . windmove-left)
 ("C-c <right>" . windmove-right)
 ("C-c <up>"    . windmove-up)
 ("C-c <down>"  . windmove-down)
 ;; Replace default buffer menu with ibuffer
 ("C-x C-b"     . ibuffer)
 ;; Rebind undo to undo-tree visualizer
 ("C-x u"       . undo-tree-visualize)
 ("C-c e f"     . byte-compile-file))

(bind-keys :map emacs-lisp-mode-map
           ("C-c e e" . eval-last-sexp)
           ("C-c e r" . eval-region)
           ("C-c e b" . eval-buffer)
           ("C-c e c" . emacs-lisp-byte-compile)
           ("C-c e l" . emacs-lisp-byte-compile-and-load))

;; Replace the major mode name with its icon and move the buffer name from the
;; mode line to the header line
(use-package all-the-icons
  :if (window-system)
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
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Turn on keyboard shortcut remainder
(use-package which-key
  :config
  (which-key-mode t)
  (bind-keys ("C-h b" . which-key-show-top-level)))

;; Unmap extraneous undo-tree mode keys
(assq-delete-all 'undo-tree-mode minor-mode-map-alist)

;; Unbind hide/show mode's ridiculous keybindings
(assq-delete-all 'hs-minor-mode minor-mode-map-alist)

;; Modern code folding
(use-package origami
  :config (bind-keys ("M-0"   . origami-open-all-nodes)
                     ("M-9"   . origami-close-all-nodes)
                     ("C-M-/" . origami-recursively-toggle-node)))

;; Enhances ido and isearch's fuzzy search
(use-package flx-ido
  :config
  (setq flx-ido-use-faces nil)
  (flx-ido-mode t))

(use-package flx-isearch
  :config
  (flx-isearch-mode t)
  (bind-keys ("C-M-s" . flx-isearch-forward)
             ("C-M-r" . flx-isearch-backward)))

;; Use ido with M-x
(use-package smex
  :config
  (smex-initialize)
  (bind-keys ("M-x" . smex)
             ("M-X" . smex-major-mode-commands)))

;; Use ido for even more things than ido-everywhere
(use-package ido-ubiquitous)
(use-package ido-vertical-mode)

;; Mark and edit multiple things at once
(use-package multiple-cursors
  :config
  (bind-keys ("C-c C-e" . mc/edit-lines)
             ("C->"     . mc/mark-next-like-this)
             ("C-<"     . mc/mark-previous-like-this)
             ("C-M->"   . mc/skip-to-next-like-this)
             ("C-M-<"   . mc/skip-to-previous-like-this)
             ("C-c C->" . mc/mark-all-dwim)))

;; Construct regexp and search visually and incrementally
(use-package visual-regexp-steroids
  :after multiple-cursors
  :config
  (bind-keys ("M-%"     . vr/replace)
             ("C-M-%"   . vr/query-replace)
             ("C-c C-s" . vr/isearch-forward)
             ("C-c C-r" . vr/isearch-backward)
             ("C-c m"   . vr/mc-mark)))

;; Turn on subword mode for all prog modes
(use-package syntax-subword
  :config (add-hook 'prog-mode-hook #'(lambda () (syntax-subword-mode t))))

(use-package expand-region
  :config (bind-keys ("M-=" . er/expand-region)
                     ("M--" . er/contract-region)))

(use-package smartparens-config
  :config
  (sp-use-smartparens-bindings)
  (bind-keys :map smartparens-mode-map
             ("M-<delete>"    . nil)
             ("M-<backspace>" . nil)
             ("C-<delete>"    . sp-unwrap-sexp)
             ("C-<backspace>" . sp-backward-unwrap-sexp)))

;; Cycle thru most commonly programming identifier styles
(use-package string-inflection
  :init
  (defun inflect-string ()
    (interactive)
    (cond ((memq major-mode '(java-mode js-mode js-jsx-mode typescript-mode))
           (string-inflection-java-style-cycle))
          ((memq major-mode '(python-mode ruby-mode))
           (string-inflection-ruby-style-cycle))
          ((derived-mode-p major-mode 'prog-mode)
           (string-inflection-all-cycle))))
  :config (bind-keys ("C-c C-u" . inflect-string)))

;; Vim-like increment and decrement
(use-package evil-numbers
  :config (bind-keys ("C-c =" . evil-numbers/inc-at-pt)
                     ("C-c -" . evil-numbers/dec-at-pt)))

;; Git
(use-package magit
  :config (bind-keys ("C-c v g" . magit-status)))

;; Hg
(use-package monky
  :config (bind-keys ("C-c v h" . monky-status)))

;; Adjust frame-wide font size
(use-package zoom-frm
  :config (bind-keys ("C-x C-+" . zoom-in/out)
                     ("C-x C--" . zoom-in/out)
                     ("C-x C-=" . zoom-in/out)
                     ("C-x C-0" . zoom-in/out)))

;; Project management
(use-package projectile
  :after pyenv-mode
  :init (defun projectile-pyenv-mode-set ()
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

;; Window management
(use-package golden-ratio
  :defines golden-ratio-inhibit-functions
  :config
  (defvar ediff-on nil)
  (add-hook 'ediff-before-setup-hook #'(lambda () (setq ediff-on t)))
  (add-hook 'ediff-quit-hook #'(lambda () (setq ediff-on nil)) 'append)
  (add-hook 'ediff-suspend-hook #'(lambda () (setq ediff-on nil)) 'append)
  (push #'(lambda () ediff-on) golden-ratio-inhibit-functions))

(use-package centered-window-mode
  :config
  (centered-window-mode)
  (add-hook 'ediff-before-setup-hook 'centered-window-mode-toggle)
  (add-hook 'ediff-quit-hook 'centered-window-mode-toggle 'append)
  (add-hook 'ediff-suspend-hook 'centered-window-mode-toggle 'append))

(use-package popwin
  :config
  (popwin-mode t)
  (global-set-key (kbd "C-z") popwin:keymap))

;; Auto-completion
(use-package company)

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode t))

(use-package company-flx
  :after company
  :config (company-flx-mode t))

(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backend '(company-shell company-shell-env)))

;; Live Syntax checking
(use-package flycheck-yamllint
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)))
(use-package flycheck-demjsonlint)
(use-package flycheck-mypy)
(dolist (hook '(python-mode-hook web-mode-hook js-mode-hook sh-mode-hook json-mode-hook))
  (add-hook hook #'(lambda () (flycheck-mode t))))

;; Web stuff
(use-package web-mode
  :after tide
  :mode ("\\.jinja'"
         "\\.tsx\\'"
         "\\.css\\'"
         "\\.phtml\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"
         "\\.handlebars\\'")
  :config (add-hook 'web-mode-hook
                    #'(lambda ()
                        (when (string-equal "tsx" (file-name-extension buffer-file-name))
                          (tide-setup)
                          (tide-hl-identifier-mode t)
                          (flycheck-mode t)))))

(use-package emmet-mode
  :after web-mode
  :config (dolist (hook '(sgml-mode-hook web-mode-hook nxml-mode-hook))
            (add-hook hook 'emmet-mode)))

;; TypeScript
(use-package typescript-mode)
(use-package ts-comint)
(use-package tide
  :after typescript-mode ts-comint
  :config
  (add-hook 'typescript-mode-hook #'(lambda ()
                                      (tide-setup)
                                      (tide-hl-identifier-mode t)
                                      (flycheck-mode t)))
  (add-hook 'before-save-hook 'tide-format-before-save)
  (bind-keys :map typescript-mode-map
             ("C-x C-e" . ts-send-last-sexp)
             ("C-M-x"   . ts-send-last-sexp-and-go)
             ("C-c b"   . ts-send-buffer)
             ("C-c C-b" . ts-send-buffer-and-go)
             ("C-c l"   . ts-load-file-and-go)
             ("C-c C-f" . tide-format)
             ("C-c m"   . tide-rename-symbol)
             ("M-1"     . tide-fix)
             ("M-/"     . tide-references)
             ("M-d"     . tide-documentation-at-point)))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js-jsx-mode))
(add-hook 'js-mode-hook
          #'(lambda ()
              (eval-when-compile (require 'js))
              (use-package eslintd-fix
                :functions eslintd-fix
                :config
                (eslintd-fix-mode t)
                (bind-keys :map js-mode-map
                           ("C-c C-f" . eslintd-fix)))

              (use-package tern
                :after company
                :config (add-to-list 'company-backends 'company-tern))))

;; JSON
(use-package json-mode
  :after flycheck-demjsonlint)

;; Python
(use-package pyenv-mode)

(add-hook 'python-mode-hook
          #'(lambda ()
              (use-package py-autopep8
                :config (py-autopep8-enable-on-save))

              (use-package python-docstring
                :config (python-docstring-mode t))

              (use-package py-isort
                :config (bind-keys :map python-mode-map
                                   ("C-c s" . py-isort-buffer)))

              (use-package anaconda-mode
                :after company
                :config
                (anaconda-mode t)
                (anaconda-eldoc-mode t)
                (add-to-list 'company-backends '(company-anaconda :with company-capf)))))

;; Go
(use-package go-mode
  :config
  (add-hook 'go-mode-hook
            #'(lambda ()
                (use-package go-eldoc
                  :config
                  (go-eldoc-setup))

                (use-package company-go
                  :config
                  (set (make-local-variable 'company-backends) '(company-go))))))

;; yasnippet
(use-package yasnippet
  :config
  (dolist (hook '(prog-mode-hook text-mode))
    (add-hook hook 'yas-minor-mode))
  (eval-when-compile (require 'yasnippet))
  (add-hook 'yas-minor-mode-hook #'(lambda () (yas-reload-all)))
  (bind-keys :map yas-minor-mode-map
             ("TAB"   . nil)
             ("<tab>" . nil)
             ("C-c i" . yas-expand)))
