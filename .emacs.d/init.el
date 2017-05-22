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
(define-key emacs-lisp-mode-map (kbd "C-c f") 'find-function-at-point)
(define-key emacs-lisp-mode-map (kbd "C-c v") 'find-variable-at-point)
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


;; Turn on subword mode for all prog modes
(use-package syntax-subword
  :config (add-hook 'prog-mode-hook (lambda () (syntax-subword-mode t))))

;; Replace isearch and query-replace functions with Anzu's equivalents
;; (use-package anzu
;;   :config (lambda ()
;;             (defalias 'query-replace 'anzu-query-replace)
;;             (defalias 'query-replace-regexp 'anzu-query-replace-regexp)
;;             (defalias 'isearch-query-replace 'anzu-isearch-query-replace)
;;             (defalias 'isearch-query-replace-regexp 'anzu-isearch-query-replace-regexp)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package smartparens-config)

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
  :config (which-key-mode))

;; Auto-completion
(eval-after-load 'company
  ;; Bring help popup back to company
  (when (require 'company-quickhelp nil t)
    (company-quickhelp-mode)))

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
            (when (require 'eslintd-fix nil t)
              (eslintd-fix-mode))
            (when (require 'tern nil t)
              (eval-after-load 'company
                '(add-to-list 'company-backends 'company-tern)))))

;; FlyCheck
(dolist (hook (list 'python-mode-hook 'web-mode-hook 'js-mode-hook))
  (add-hook hook
            (lambda ()
              (when (or (require 'flycheck-mypy nil t)
                        (require 'flycheck nil t))
                (flycheck-mode)))))

;; Python stuff
(add-hook 'python-mode-hook
          (lambda ()
            (when (require 'py-autopep8 nil t)
              (py-autopep8-enable-on-save))

            (when (require 'python-docstring nil t)
              (python-docstring-mode))

            (when (require 'py-isort nil t)
              (eval-when-compile (require 'python))
              (define-key python-mode-map (kbd "C-c s") 'py-isort-buffer))

            (when (require 'anaconda-mode nil t)
              (anaconda-mode)
              (anaconda-eldoc-mode))

            (eval-after-load 'company
              '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

            (require 'pyenv-mode-auto nil t)

            (hs-minor-mode)))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (dolist (process (process-list))
              (when (string-prefix-p "Python" (process-name process))
                (set-process-query-on-exit-flag process nil)))))

;; yasnippet
(use-package yasnippet
  :config (lambda ()
            (add-hook 'yas-minor-mode-hook
                      (lambda ()
                        (define-key yas-minor-mode-map (kbd "TAB") nil)
                        (define-key yas-minor-mode-map (kbd "<tab>") nil)
                        (define-key yas-minor-mode-map (kbd "C-c i") 'yas-expand)))
            ;; Turn on yasnippet for all prog and text modes
            (dolist (hook (list 'prog-mode-hook 'text-mode))
              (add-hook hook 'yas-minor-mode)
              (yas-reload-all))))

;; Modern fancy mode line
(defun set-buffer-id-to-header-line ()
  (when (window-header-line-height)
    (setq header-line-format 'mode-line-buffer-identification)))

(defun replace-mode-with-icon ()
  (when (and (require 'all-the-icons nil t) (window-system))
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
      (when (and icon (not (string= major-mode icon)))
        (setq mode-name icon)))))

(when (require 'spaceline-config nil t)
  (cond ((require 'spaceline-all-the-icons nil t)
         (spaceline-all-the-icons-theme)
         (spaceline-all-the-icons--setup-package-updates)
         (spaceline-all-the-icons--setup-git-ahead)
         (spaceline-all-the-icons--setup-neotree))
        (t
         (dolist (hook (list
                        'window-configuration-change-hook
                        'after-change-major-mode-hook))
           (add-hook hook 'set-buffer-id-to-header-line))
         (set-buffer-id-to-header-line)

         ;; The buffer ID is removed from the mode line in customize.el, this sexp
         ;; replace it with the icon
         (add-hook 'after-change-major-mode-hook 'replace-mode-with-icon)
         (replace-mode-with-icon)
         (spaceline-toggle-projectile-root-off)
         (spaceline-toggle-buffer-id-off)
         (spaceline-spacemacs-theme))))
