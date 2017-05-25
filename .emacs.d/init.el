;; Stop asking me if a theme is safe. The entirety of Emacs is built around
;; evaling arbitrary code...
(advice-add 'load-theme :around #'(lambda (old-load-theme &rest r)
                                    (apply old-load-theme (append r '(t)))))

;; Emacs loads init file first and the packages last normally. Forcing the
;; packages to load first makes conifguring them in the init file possible.
(package-initialize)

;; Tell Custom to write and find the custom settings elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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

;; More sensible comment-dwim
(defun comment-dwim-line-or-region (&optional arg)
  "Replacement for the ‘comment-dwim' command.

If no region is selected then comment the current line from the
front, otherwise the commentable lines in the region. Replaces
the default behaviour of ‘comment-dwim’, where it inserts comment
at the end of the line.

Optional argument ARG same as `comment-dwim''s."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (use-region-p)))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))


(use-package bind-key
  :bind (
         ("M-;" . comment-dwim-line-or-region)
         ;; Rebind windmove keys
         ("C-c <left>"  . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>"    . windmove-up)
         ("C-c <down>"  . windmove-down)
         ;; Replace default buffer menu with ibuffer
         ("C-x C-b"     . ibuffer)
         ;; Rebind undo to undo-tree visualizer
         ("C-x u"       . undo-tree-visualize)
         ("C-c e f"     . byte-compile-file)
         :emacs-lisp-mode-map
         ("C-c e e"     . eval-last-sexp)
         ("C-c e e"     . eval-last-sexp)
         ("C-c e r"     . eval-region)
         ("C-c e b"     . eval-buffer)
         ("C-c e c"     . emacs-lisp-byte-compile)
         ("C-c e l"     . emacs-lisp-byte-compile-and-load)
         ("C-c e d"     . byte-recompile-directory)
         ("C-c e g"     . find-function-at-point)
         ("C-c e v"     . find-variable-at-point))
  :config (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

;; Unmap extraneous undo-tree mode keys
(assq-delete-all 'undo-tree-mode minor-mode-map-alist)

;; Unbind hide/show mode's ridiculous keybindings
(assq-delete-all 'hs-minor-mode minor-mode-map-alist)

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
              (eval-when-compile
                (require 'ediff-util)
                (ediff-janitor nil nil)))
          'append)

(use-package all-the-icons-dired
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Enhances ido and isearch's fuzzy search
(use-package flx-ido
  :config (progn
            (setq flx-ido-use-faces nil)
            (flx-ido-mode t)))

(use-package flx-isearch
  :bind (("C-M-s" . flx-isearch-forward)
         ("C-M-r" . flx-isearch-backward))
  :config (flx-isearch-mode t))

;; Use ido with M-x
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

;; Use ido for even more things than ido-everywhere
(use-package ido-ubiquitous)
(use-package ido-match-modes)
(use-package ido-vertical-mode)

;; Git
(use-package magit
  :bind (("C-c v g" . magit-status)))

;; Hg
(use-package monky
  :bind (("C-c v h" . monky-status)))

;; Modern code folding
(use-package origami
  :bind (("M-0"   . origami-open-all-nodes)
         ("M-9"   . origami-close-all-nodes)
         ("C-M-/" . origami-recursively-toggle-node)))

;; Turn on subword mode for all prog modes
(use-package syntax-subword
  :config (add-hook 'prog-mode-hook #'(lambda () (syntax-subword-mode t))))

(use-package expand-region
  :bind (("M-=" . er/expand-region)
         ("M--" . er/contract-region)))

(use-package smartparens-config
  :bind (("M-A"       . sp-beginning-of-sexp)
         ("M-E"       . sp-end-of-sexp)
         ("M-_"       . sp-unwrap-sexp)
         ("C-<left>"  . sp-backward-slurp-sexp)
         ("M-<left>"  . sp-backward-barf-sexp)
         ("C-<right>" . sp-forward-slurp-sexp)
         ("M-<right>" . sp-forward-barf-sexp)))

;; Cycle thru most commonly programming identifier styles
(use-package string-inflection
  :init (defun inflect-string ()
          (interactive)
          (cond ((memq major-mode '(java-mode js-mode js-jsx-mode typescript-mode))
                 (string-inflection-java-style-cycle))
                ((memq major-mode '(python-mode ruby-mode))
                 (string-inflection-ruby-style-cycle))
                ((derived-mode-p major-mode 'prog-mode)
                 (string-inflection-all-cycle))))
  :bind (("C-c C-u" . inflect-string)))

;; Vim-like increment and decrement
(use-package evil-numbers
  :bind (("C-c =" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

;; Adjust frame-wide font size
(use-package zoom-frm
  :bind (("C-x C-+" . zoom-in/out)
         ("C-x C--" . zoom-in/out)
         ("C-x C-=" . zoom-in/out)
         ("C-x C-0" . zoom-in/out)))

;; Turn on keyboard shortcut remainder
(use-package which-key
  :bind (("C-h b" . which-key-show-top-level))
  :config (which-key-mode t))

;; Auto-completion
(eval-after-load 'company
  ;; Bring help popup back to company
  (use-package company-quickhelp
    :config (company-quickhelp-mode t)))

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
                          (flycheck-mode t)
                          (eldoc-mode t)))))

(use-package emmet-mode
  :after web-mode
  :config (dolist (hook '(sgml-mode-hook web-mode-hook nxml-mode-hook))
            (add-hook hook 'emmet-mode)))

;; TypeScript
(use-package typescript-mode)
(use-package tide
  :after typescript-mode
  :bind (:map typescript-mode-map
              ("M-F" . tide-format)
              ("M-r" . tide-rename-symbol)
              ("M-1" . tide-fix)
              ("M-G" . tide-references)
              ("M-d" . tide-documentation-at-point))
  :config (progn
            (add-hook 'typescript-mode-hook
                      #'(lambda ()
                          (tide-setup)
                          (tide-hl-identifier-mode t)
                          (flycheck-mode t)
                          (eldoc-mode t)))
            (add-hook 'before-save-hook 'tide-format-before-save)))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'\\|\\.json\\'" . js-jsx-mode))
(add-hook 'js-jsx-mode-hook
          #'(lambda ()
              (use-package prettier-js
                :init (eval-when-compile (require 'js))
                :bind (:map js-mode-map
                            ("M-F" . prettier))
                :config (prettier-mode))

              (use-package eslintd-fix
                :config (eslintd-fix-mode t))

              (use-package tern
                :config (eval-after-load 'company
                          '(add-to-list 'company-backends 'company-tern)))))

;; FlyCheck
(dolist (hook '(python-mode-hook web-mode-hook js-mode-hook))
  (add-hook hook #'(lambda ()
                     (use-package flycheck-mypy
                       :config (flycheck-mode t)))))

;; Python stuff
(add-hook 'python-mode-hook
          #'(lambda ()
              (use-package py-autopep8
                :config (py-autopep8-enable-on-save))

              (use-package python-docstring
                :config (python-docstring-mode))

              (use-package py-isort
                :bind (:map python-mode-map
                            ("C-c s" . py-isort-buffer)))

              (use-package pyenv-mode-auto)

              (use-package anaconda-mode
                :config (progn
                          (anaconda-mode t)
                          (anaconda-eldoc-mode t)
                          (eval-after-load 'company
                            '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))))

(add-hook 'inferior-python-mode-hook
          #'(lambda ()
              (dolist (process (process-list))
                (when (string-prefix-p "Python" (process-name process))
                  (set-process-query-on-exit-flag process nil)))))

;; yasnippet
(use-package yasnippet
  :bind (:map yas-minor-mode-map
              ("TAB"   . nil)
              ("<tab>" . nil)
              ("C-c i" . yas-expand))
  :config (dolist (hook '(prog-mode-hook text-mode))
            (add-hook hook 'yas-minor-mode)
            (yas-reload-all)))

;; Window management
(use-package golden-ratio
  :config (progn
            (defvar ediff-on nil)
            (add-hook 'ediff-before-setup-hook #'(lambda () (setq ediff-on t)))
            (add-hook 'ediff-quit-hook #'(lambda () (setq ediff-on nil)) 'append)
            (add-hook 'ediff-suspend-hook #'(lambda () (setq ediff-on nil)) 'append)
            (push #'(lambda () ediff-on) golden-ratio-inhibit-functions)))

(use-package centered-window-mode
  :config (progn
            (centered-window-mode)
            (add-hook 'ediff-before-setup-hook 'centered-window-mode-toggle)
            (add-hook 'ediff-quit-hook 'centered-window-mode-toggle 'append)
            (add-hook 'ediff-suspend-hook 'centered-window-mode-toggle 'append)))

;; Modern fancy mode line
(use-package spaceline-all-the-icons
  :functions spaceline-all-the-icons-theme
  spaceline-all-the-icons--setup-package-updates
  spaceline-all-the-icons--setup-git-ahead
  spaceline-all-the-icons--setup-neotree
  :config (progn
            (spaceline-all-the-icons-theme)
            (spaceline-all-the-icons--setup-package-updates)
            (spaceline-all-the-icons--setup-git-ahead)))