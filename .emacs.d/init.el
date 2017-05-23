;; Stop asking me if a theme is safe. The entirety of Emacs is built around
;; evaling arbituary code...
(advice-add 'load-theme :around (lambda (old-load-theme &rest r)
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

(global-set-key (kbd "M-;") 'comment-dwim-line-or-region)

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook (lambda () (auto-fill-mode t)))

(global-set-key (kbd "C-c e f") 'byte-compile-file)

;; Turn on linum mode for all prog and text modes
(dolist (hook (list 'prog-mode-hook 'text-mode-hook))
  (add-hook hook (lambda () (linum-mode t))))
;; Renumber the current buffer after reverting the buffer
(add-hook 'after-revert-hook 'linum-update-current)

;; lisp keybindings
(define-key emacs-lisp-mode-map (kbd "C-c e e") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c e r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c e c") 'emacs-lisp-byte-compile)
(define-key emacs-lisp-mode-map (kbd "C-c e l") 'emacs-lisp-byte-compile-and-load)
(define-key emacs-lisp-mode-map (kbd "C-c e d") 'byte-recompile-directory)
(define-key emacs-lisp-mode-map (kbd "C-c e g") 'find-function-at-point)
(define-key emacs-lisp-mode-map (kbd "C-c e v") 'find-variable-at-point)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; rebind windmove keys
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; unmap undo-tree mode
(assq-delete-all 'undo-tree-mode minor-mode-map-alist)
(global-set-key (kbd "C-x u") 'undo-tree-visualize)

;; Unbind hide/show mode's ridiculous keybindings
(assq-delete-all 'hs-minor-mode minor-mode-map-alist)

;; Turn on iMenu for code outlines for all prog and text modes, if possible
(dolist (hook (list 'prog-mode-hook 'text-mode-hook))
  (add-hook hook (lambda () (ignore-errors (imenu-add-menubar-index)))))

;; Replace ido and isearch
(use-package ivy
  :after magit
  :config (ivy-mode t))

(use-package ivy-rich
  :after ivy
  :config (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

(use-package counsel
  :after swiper
  :bind (("C-x C-f" . counsel-find-file)
         :map read-expression-map
              ("C-r" . counsel-expression-history)))

;; Replace isearch query replace and query-replace functions with Anzu's equivalents
(use-package anzu
  :config (progn ()
            (defalias 'query-replace 'anzu-query-replace)
            (defalias 'query-replace-regexp 'anzu-query-replace-regexp)
            (defalias 'isearch-query-replace 'anzu-isearch-query-replace)
            (defalias 'isearch-query-replace-regexp 'anzu-isearch-query-replace-regexp)))

;; Git
(use-package magit
  :bind (("C-c v g" . magit-status)))

;; Hg
(use-package monky
  :bind (("C-c v h" . monky-status)))

;; Modern code folding
(use-package origami
  :bind (("M-0" . origami-open-all-nodes)
         ("M-9" . origami-close-all-nodes)
         ("C-M-/" . origami-recursively-toggle-node)))

;; Turn on subword mode for all prog modes
(use-package syntax-subword
  :config (add-hook 'prog-mode-hook (lambda () (syntax-subword-mode t))))

(use-package expand-region
  :bind (("M-=" . er/expand-region)
         ("M--" . er/contract-region)))

(use-package smartparens-config
  :bind (("M-A" . sp-beginning-of-sexp)
         ("M-E" . sp-end-of-sexp)
         ("M-_" . sp-unwrap-sexp)
         ("C-<left>" . sp-backward-slurp-sexp)
         ("M-<left>" . sp-backward-barf-sexp)
         ("C-<right>" . sp-forward-slurp-sexp)
         ("M-<right>" . sp-forward-barf-sexp)))

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
  :mode ("\\.css\\'"
         "\\.phtml\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"
         "\\.handlebars\\'"))

;; (when (require 'php-mode nil t)
;;   (dolist (hook (list
;;                  'php-mode-pear-hook
;;                  'php-mode-psr2-hook
;;                  'php-mode-drupal-hook
;;                  'php-mode-symfony2-hook
;;                  'php-mode-wordpress-hook))
;;     (add-hook hook (lambda ()
;;                      (setq c-basic-offset 4)
;;                      (setq tab-width 4)))))

(use-package emmet-mode
  :after web-mode
  :config (dolist (hook (list 'sgml-mode-hook 'web-mode-hook 'nxml-mode-hook))
            (add-hook hook 'emmet-mode)))

;; js-mode
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'\\|\\.json\\'" . js-jsx-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (use-package eslintd-fix
              :config (eslintd-fix-mode t))

            (use-package tern
              :config (eval-after-load 'company
                        '(add-to-list 'company-backends 'company-tern)))))

;; FlyCheck
(dolist (hook (list 'python-mode-hook 'web-mode-hook 'js-mode-hook))
  (add-hook hook (lambda ()
                   (use-package flycheck-mypy
                     :config (flycheck-mode t)))))

;; Python stuff
(add-hook 'python-mode-hook
          (lambda ()
            (use-package py-autopep8
              :config (py-autopep8-enable-on-save))

            (use-package python-docstring
              :config (python-docstring-mode))

            (use-package py-isort
              :config (define-key python-mode-map (kbd "C-c s") 'py-isort-buffer))

            (use-package pyenv-mode-auto)

            (use-package anaconda-mode
              :config (progn
                        (anaconda-mode t)
                        (anaconda-eldoc-mode t)
                        (eval-after-load 'company
                          '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (dolist (process (process-list))
              (when (string-prefix-p "Python" (process-name process))
                (set-process-query-on-exit-flag process nil)))))

;; yasnippet
(use-package yasnippet
  :config (progn
            (add-hook 'yas-minor-mode-hook
                      (lambda ()
                        (define-key yas-minor-mode-map (kbd "TAB") nil)
                        (define-key yas-minor-mode-map (kbd "<tab>") nil)
                        (define-key yas-minor-mode-map (kbd "C-c i") 'yas-expand)))
            ;; Turn on yasnippet for all prog and text modes
            (dolist (hook (list 'prog-mode-hook 'text-mode))
              (add-hook hook 'yas-minor-mode)
              (yas-reload-all))))

;; Project and window management
(use-package golden-ratio
  :config (golden-ratio-mode t))

(use-package counsel-projectile
  :config (counsel-projectile-on))

;; Modern fancy mode line
;; (defvar use-icon t)

(use-package spaceline
  :after projectile)

;; (use-package spaceline-config
;;   :after spaceline
;;   :functions spaceline-toggle-buffer-id-off spaceline-spacemacs-theme
;;   :config (progn
;;             (dolist (hook (list
;;                            'window-configuration-change-hook
;;                            'after-change-major-mode-hook))
;;               (add-hook hook
;;                         (lambda ()
;;                           (when (window-header-line-height)
;;                             (setq header-line-format 'mode-line-buffer-identification)))))

;;             ;; The buffer ID is removed from the mode line in customize.el, this sexp
;;             ;; replace it with the icon
;;             (add-hook 'after-change-major-mode-hook
;;                       (lambda ()
;;                         (when (and (require 'all-the-icons nil t) (window-system))
;;                           (let ((icon (all-the-icons-icon-for-mode major-mode)))
;;                             (when (and icon (not (string= major-mode icon)))
;;                               (setq mode-name icon))))))

;;             (spaceline-toggle-minor-modes-off)
;;             (spaceline-toggle-buffer-id-off)
;;             (spaceline-spacemacs-theme)))

(use-package spaceline-all-the-icons
  :after spaceline
  :functions spaceline-all-the-icons-theme
  spaceline-all-the-icons--setup-package-updates
  spaceline-all-the-icons--setup-git-ahead
  spaceline-all-the-icons--setup-neotree
  :config (progn
            (spaceline-all-the-icons-theme)
            (spaceline-all-the-icons--setup-package-updates)
            (spaceline-all-the-icons--setup-git-ahead)
            (spaceline-all-the-icons--setup-neotree)))
