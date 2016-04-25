;; Emacs loads init file first and the packages last normally. Forcing the
;; packages to load first makes conifguring them in the init file possible.
(package-initialize)
(setq x-select-enable-clipboard t)
(setq ring-bell-function 'ignore)
(prefer-coding-system 'utf-8)

;; Enable initially disabled keys
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

;; header-line with file name
(add-hook 'window-configuration-change-hook
          (lambda ()
            (if (window-header-line-height)
                (setq header-line-format (abbreviate-file-name (or buffer-file-name ""))))))

;; Remove buffer ID from mode-line
(add-hook 'find-file-hook
          (lambda ()
            (setq mode-line-format
                  (delq 'mode-line-buffer-identification mode-line-format))))

;; Maximize frame on startup and set up default font
(when (window-system)
  (set-frame-font "DejaVu Sans Mono")
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (use-region-p)))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'comment-dwim-line)

;; emacs-lisp-mode
(define-key emacs-lisp-mode-map (kbd "\C-cee") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "\C-cer") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "\C-ceb") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "\C-cec") 'emacs-lisp-byte-compile)
(define-key emacs-lisp-mode-map (kbd "\C-cel") 'emacs-lisp-byte-compile-and-load)
(define-key emacs-lisp-mode-map (kbd "\C-ced") 'byte-recompile-directory)
(define-key emacs-lisp-mode-map (kbd "\C-cg") 'find-function-at-point)
(global-set-key (kbd "\C-cef") 'byte-compile-file)
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode t)))

;; Tell Custom to write and find the custom settings elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Vim-like increment and decrement
(when (require 'evil-numbers nil t)
  (global-set-key (kbd "C-c =") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; Turn on iMenu for code outlines
(dolist (hook (list
               'lisp-mode-hook
               'emacs-lisp-mode-hook
               'c-mode-common-hook
               'sh-mode-hook
               'sgml-mode-hook
               'python-mode-hook
               'ruby-mode-hook
               'perl-mode-hook
               'conf-mode-hook
               'rst-mode-hook
               'markdown-mode-hook))
  (add-hook hook (lambda()
                   (condition-case nil
                       (imenu-add-menubar-index)
                     (error nil)))))

;; Turn on subword mode for these prog modes
(dolist (hook (list
               'c-mode-common-hook
               'js-mode-hook))
  (add-hook hook (lambda () (subword-mode t))))

;; multiple cursors
(when (require 'multiple-cursors nil t)
  (global-set-key (kbd "C-,") 'mc/mark-all-dwim))

;; web-mode
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
                             (imenu-add-menubar-index))))


;; php-mode
(when (require 'php-mode nil t)
  (dolist (hook (list
                 'php-mode-pear-hook
                 'php-mode-psr2-hook
                 'php-mode-drupal-hook
                 'php-mode-symfony2-hook
                 'php-mode-wordpress-hook))
    (add-hook hook (lambda ()
                     (setq c-basic-offset 4)
                     (setq tab-width 4)))))

;; emmet-mode
(when (require 'emmet-mode nil t)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode))

;; js-mode
(add-hook 'js-mode-hook (lambda()
                          (hs-minor-mode t)
                          (tern-mode t)
                          (tern-ac-setup)))

;; FlyCheck
(when (require 'flycheck nil t)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'html-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode))

;; Markdown mode
(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode)))

;; Python-mode
(when (not (require 'pungi nil t))
  (when (require 'jedi nil t)
    (add-hook 'python-mode-hook 'jedi:setup)))

(when (require 'pyvenv nil t)
  (add-hook 'python-mode-hook (lambda () (pyvenv-mode t))))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (dolist (process (process-list))
              (when (string-prefix-p "Python" (process-name process))
                (set-process-query-on-exit-flag process nil)))))

;; reStructuredText mode
(when (require 'rst nil t)
  (add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
  (add-to-list 'auto-mode-alist '("\\.rest$" . rst-mode))
  (add-hook 'rst-adjust-hook 'rst-toc-update))

;; yasnippet
(when (require 'yasnippet nil t)
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (define-key yas-minor-mode-map (kbd "TAB") nil)
              (define-key yas-minor-mode-map (kbd "<tab>") nil)
              (define-key yas-minor-mode-map [(tab)] nil)
              (define-key yas-minor-mode-map "\C-ci" 'yas-expand)))
  ;; Turn on yasnippet for these prog modes
  (dolist (hook (list
                 'lisp-mode-hook
                 'emacs-lisp-mode-hook
                 'c-mode-common-hook
                 'js-mode-hook
                 'sh-mode-hook
                 'sgml-mode-hook
                 'nxml-mode-hook
                 'web-mode-hook
                 'python-mode-hook
                 'ruby-mode-hook
                 'perl-mode-hook
                 'conf-mode-hook
                 'rst-mode-hook
                 'markdown-mode-hook))
    (add-hook hook (lambda ()
                     (yas-minor-mode t))))
  (yas-reload-all))

;; Auto-complete
(when (require 'auto-complete-config nil t)
  (ac-config-default))
