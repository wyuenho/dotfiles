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
  (set-frame-parameter nil 'fullscreen 'maximized)
  (set-mouse-color "white"))

(prefer-coding-system 'utf-8)

;; Enable initially disabled keys
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook #'(lambda () (auto-fill-mode 1)))

;; Turn on linum mode for all prog and text modes
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'(lambda () (linum-mode 1))))

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
front, otherwise the commentable lines in the region. Replaces
the default behaviour of `comment-dwim', where it inserts comment
at the end of the line.

Optional argument ARG same as `comment-dwim''s."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (use-region-p)))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; WTF still using ASCII inputs???
(define-key input-decode-map [?\C-m] [C-m])

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(bind-keys ("C-c a"                          . align)
           ("M-;"                            . comment-dwim-line-or-region)
           ;; Make sure C-a and C-e are always moving by logical line, and
           ;; `next-line' and `previous-line' are also moving by logical lines,
           ;; even when `visual-line-mode' is turned on
           ("C-a"                            . beginning-of-line)
           ("C-e"                            . end-of-line)
           ([remap next-line]                . next-logical-line)
           ([remap previous-line]            . previous-logical-line)
           ;; Rebind windmove keys
           ("C-c <left>"                     . windmove-left)
           ("C-c <right>"                    . windmove-right)
           ("C-c <up>"                       . windmove-up)
           ("C-c <down>"                     . windmove-down)
           ;; Replace default buffer menu with ibuffer
           ("C-x C-b"                        . ibuffer)
           ;;
           ("C-x u"                          . undo-tree-visualize)
           ("C-c e f"                        . byte-compile-file))

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
  (which-key-mode 1)
  (bind-keys ("C-h b" . which-key-show-top-level)))

;; Unmap extraneous undo-tree mode keys
(assq-delete-all 'undo-tree-mode minor-mode-map-alist)

;; Unbind hide/show mode's ridiculous keybindings
(assq-delete-all 'hs-minor-mode minor-mode-map-alist)

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
             ("C-c m"   . vr/mc-mark)))

;; Turn on subword mode for all prog modes
(use-package syntax-subword
  :config (add-hook 'prog-mode-hook #'(lambda () (syntax-subword-mode 1))))

(use-package expand-region
  :config
  (bind-keys ("M-=" . er/expand-region)
             ("M--" . er/contract-region)))

(use-package smartparens-config
  :init
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
    (cond ((memq major-mode '(java-mode js-mode js2-mode typescript-mode))
           (string-inflection-java-style-cycle))
          ((memq major-mode '(python-mode ruby-mode))
           (string-inflection-ruby-style-cycle))
          ((derived-mode-p major-mode 'prog-mode)
           (string-inflection-all-cycle))))
  :config (bind-keys ("C-c C-u" . inflect-string)))

;; Vim-like increment and decrement
(use-package evil-numbers
  :config
  (bind-keys ("C-c =" . evil-numbers/inc-at-pt)
             ("C-c -" . evil-numbers/dec-at-pt)))

;; Git
(use-package magit
  :config (bind-keys ("C-c v g" . magit-status)))

;; Hg
(use-package monky
  :config (bind-keys ("C-c v h" . monky-status)))

;; Adjust frame-wide font size
(use-package zoom-frm
  :config
  (bind-keys ("C-x C-+" . zoom-in/out)
             ("C-x C--" . zoom-in/out)
             ("C-x C-=" . zoom-in/out)
             ("C-x C-0" . zoom-in/out)))

;; Window management
(use-package golden-ratio
  :defines golden-ratio-inhibit-functions
  :config
  (defvar ediff-on nil)
  (add-hook 'ediff-before-setup-hook #'(lambda () (setq ediff-on 1)))
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
  (popwin-mode 1)
  (bind-key "C-z" popwin:keymap))

;; Quick Snippets
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

;; Auto-completion
(defun my-load-company ()
  (when (require 'company nil t)
    (bind-keys :map company-mode-map
               ("M-/" . company-complete)))

  (when (require 'company-statistics nil t)
    (company-statistics-mode 1))

  (when (require 'company-quickhelp nil t)
    (company-quickhelp-mode 1))

  (when (require 'company-flx nil t)
    (company-flx-mode 1)))

;; Automatical syntax checking
(defun my-load-flycheck ()
  (when (require 'flycheck nil t)
    (flycheck-mode 1)))

;; Plain-text RESTful service client
(use-package restclient
  :config
  (add-hook 'restclient-mode-hook
            #'(lambda ()
                (my-load-company)
                (use-package company-restclient
                  :config
                  (add-to-list 'company-backends 'company-restclient)))))

;; Much faster PDF viewing
(add-hook 'doc-view-mode-hook
          #'(lambda ()
              (when (fboundp 'pdf-tools-install)
                (pdf-tools-install))))

;; Lisp
(dolist (hook '(lisp-mode-hook))
  (add-hook hook #'my-load-flycheck))

(bind-keys :map emacs-lisp-mode-map
           ("C-c e c" . emacs-lisp-byte-compile)
           ("C-c e l" . emacs-lisp-byte-compile-and-load))

;; Shell mode
(add-hook 'sh-mode-hook
          #'(lambda ()
              (my-load-flycheck)
              (my-load-company)
              (use-package company-shell
                :config
                (add-to-list 'company-backend '(company-shell company-shell-env)))))

;; JSON mode
(use-package json-mode
  :config
  (add-hook 'json-mode-hook
            #'(lambda ()
                (my-load-flycheck)
                (use-package flycheck-demjsonlint))))

;; YAML mode
(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
            #'(lambda ()
                (my-load-flycheck)
                (use-package flycheck-yamllint
                  :config
                  (flycheck-yamllint-setup)))))

;; JavaScript
(use-package rjsx-mode
  :mode ("\\.js[x]?\\'")
  :config

  (add-hook 'js-mode-hook
            #'(lambda ()
                (my-load-flycheck)

                (use-package eslintd-fix
                  :functions eslintd-fix
                  :config
                  (eslintd-fix-mode 1)
                  (bind-keys :map rjsx-mode-map
                             ("C-c C-f" . eslintd-fix)))

                (use-package tern
                  :config
                  (tern-mode 1)
                  (unbind-key "C-c C-r" tern-mode-keymap))

                (my-load-company)
                (use-package company-tern
                  :config
                  (add-to-list 'company-backends 'company-tern))

                (define-key js-mode-map [menu-bar] nil)))

  (add-hook 'js2-mode-hook
            #'(lambda ()
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

;; TypeScript
(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'my-load-flycheck))

(use-package ts-comint
  :after typescript-mode
  :config
  (bind-keys :map typescript-mode-map
             ("C-x C-e" . ts-send-last-sexp)
             ("C-M-x"   . ts-send-last-sexp-and-go)
             ("C-c b"   . ts-send-buffer)
             ("C-c C-b" . ts-send-buffer-and-go)
             ("C-c l"   . ts-load-file-and-go)))

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
             ("C-c m"   . tide-rename-symbol)
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
              (my-load-flycheck)

              (my-load-company)
              (use-package anaconda-mode
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
                (my-load-flycheck)
                (my-load-company)
                (use-package company-go
                  :config
                  (set (make-local-variable 'company-backends) '(company-go)))))
  (add-hook 'go-mode-hook #'(lambda () (use-package go-eldoc :config (go-eldoc-setup)))))

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
                (my-load-flycheck)
                ;; TODO: deal with this
                (flycheck-add-mode 'typescript-tslint 'web-mode)

                (my-load-company)
                (when (and (require 'company-tern nil t)
                           (require 'company-web-html nil t))
                  (set (make-local-variable 'company-backends)
                       '(company-tern company-web-html company-yasnippet company-files))

                  (advice-add 'company-tern :before
                              #'(lambda (&rest _)
                                  (if (equal major-mode 'web-mode)
                                      (let ((web-mode-cur-language
                                             (web-mode-language-at-pos)))
                                        (if (or (string= web-mode-cur-language "javascript")
                                                (string= web-mode-cur-language "jsx"))
                                            (unless tern-mode (tern-mode))
                                          (if tern-mode (tern-mode -1))))))))

                (when (and (string-equal "tsx" (file-name-extension buffer-file-name))
                           (featurep 'tide))
                  (tide-setup)
                  (tide-hl-identifier-mode 1)))))

;; deal with tsx and jsx classnames
(use-package emmet-mode
  :after web-mode
  :config
  (dolist (hook '(sgml-mode-hook
                  nxml-mode-hook
                  web-mode-hook
                  js-jsx-mode-hook
                  js2-jsx-mode
                  rjsx-mode-hook))
    (add-hook hook 'emmet-mode)))

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
  (projectile-mode 1)
  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set))

(use-package go-projectile
  :after go-mode projectile)
