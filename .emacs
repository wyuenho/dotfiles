;; Emacs loads init file first and the packages last normally. Forcing the
;; packages to load first makes conifguring them in the init file possible.
(package-initialize)
(setq select-enable-clipboard t)
(setq ring-bell-function 'ignore)
(prefer-coding-system 'utf-8)

;; Enable initially disabled keys
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

;; header-line with file name
(dolist (hook (list
               'window-configuration-change-hook
               'after-change-major-mode-hook))
  (add-hook hook
            (lambda ()
              (if (window-header-line-height)
                  (setq header-line-format (abbreviate-file-name (or buffer-file-name "")))))))

;; Remove buffer ID from mode-line
(add-hook 'find-file-hook
          (lambda ()
            (setq mode-line-format
                  (delq 'mode-line-buffer-identification mode-line-format))))

;; Maximize frame on startup and set up default font
(when (window-system)
  (set-frame-font "DejaVu Sans Mono"))
  ;; (set-frame-parameter nil 'fullscreen 'fullboth))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (use-region-p)))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") 'comment-dwim-line)

;; emacs-lisp-mode
(define-key emacs-lisp-mode-map (kbd "C-c e e") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c e r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c e c") 'emacs-lisp-byte-compile)
(define-key emacs-lisp-mode-map (kbd "C-c e l") 'emacs-lisp-byte-compile-and-load)
(define-key emacs-lisp-mode-map (kbd "C-c e d") 'byte-recompile-directory)
(define-key emacs-lisp-mode-map (kbd "C-c g") 'find-function-at-point)
(global-set-key (kbd "C-c e f") 'byte-compile-file)
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode t)))

;; Tell Custom to write and find the custom settings elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Renumber the current buffer after reverting the buffer
(add-hook 'after-revert-hook 'linum-update-current)

;; Vim-like increment and decrement
(when (require 'evil-numbers nil t)
  (global-set-key (kbd "C-c =") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; Adjust frame-wide font size
(when (require 'zoom-frm nil t)
  (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
  (define-key ctl-x-map [(control ?0)] 'zoom-in/out))

;; Rebind hide/show mode's ridiculous keybindings
(add-hook 'hs-minor-mode-hook
          (lambda ()
            (define-key hs-minor-mode-map (kbd "C-c @") nil)
            (define-key hs-minor-mode-map (kbd "C-c @ ESC") nil)
            (define-key hs-minor-mode-map (kbd "C-c @ C-h") nil)
            (define-key hs-minor-mode-map (kbd "C-c @ C-s")	nil)
            (define-key hs-minor-mode-map (kbd "C-c @ C-M-h") nil)
            (define-key hs-minor-mode-map (kbd "C-c @ C-M-s") nil)
            (define-key hs-minor-mode-map (kbd "C-c @ C-l") nil)
            (define-key hs-minor-mode-map (kbd "C-c @ C-c") nil)

            (define-key hs-minor-mode-map (kbd "C-x C-/") 'hs-toggle-hiding)
            (define-key hs-minor-mode-map (kbd "C-x C-<") 'hs-hide-all)
            (define-key hs-minor-mode-map (kbd "C-x C->") 'hs-show-all)
            (define-key hs-minor-mode-map (kbd "C-x C-,") 'hs-hide-block)
            (define-key hs-minor-mode-map (kbd "C-x C-.") 'hs-show-block)))

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
               'web-mode-hook
               'js-mode-hook
               'js2-mode-hook))
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

;; emmet-mode
(when (require 'emmet-mode nil t)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode))

;; js-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))
(add-hook 'js-mode-hook
          (lambda()
            (when (require 'tern-mode nil t)
              (tern-mode t)
              (eval-after-load "company"
                '(add-to-list 'company-backends 'company-tern)))))

;; FlyCheck
(when (or (require 'flycheck-mypy nil t)
          (require 'flycheck nil t))
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'html-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook 'flycheck-mode))

;; Markdown mode
(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode)))

;; Python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (when (require 'yapfify nil t)
              (yapf-mode))
            (when (require 'python-docstring nil t)
              (python-docstring-mode t))
            (when (require 'py-isort nil t)
              (define-key python-mode-map (kbd "C-c s") 'py-isort-buffer))
            (hs-minor-mode t)))

(require 'pyenv-mode-auto nil t)

(when (require 'anaconda-mode nil t)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook (lambda () (anaconda-eldoc-mode t)))
  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda :with company-capf))))

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
              (define-key yas-minor-mode-map (kbd "C-c i") 'yas-expand)))
  ;; Turn on yasnippet for these prog modes
  (dolist (hook (list
                 'lisp-mode-hook
                 'emacs-lisp-mode-hook
                 'c-mode-common-hook
                 'js-mode-hook
                 'js2-mode-hook
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
