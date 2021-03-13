;; -*- lexical-binding: t -*-
(toggle-debug-on-error)

(require 'map)
(require 'seq)
(require 'cl-lib)

(set-locale-environment "UTF-8")

;; Tell Custom to write and find the custom settings elsewhere
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Install selected by missing packages
(let ((missing (cl-set-difference package-selected-packages package-activated-list)))
  (when missing
    (with-demoted-errors "%s"
      (package-refresh-contents))
    (dolist (package missing)
      (with-demoted-errors "%s"
        (package-install package t)
        (package-activate package))))
  (require 'quelpa)
  (when (quelpa-read-cache)
    (quelpa-upgrade-all-maybe)
    (dolist (cache quelpa-cache)
      (let ((package (car cache)))
        (package-activate package))))
  (require 'quelpa-use-package)
  (setq use-package-compute-statistics t)
  (quelpa-use-package-activate-advice))

;; Remap Unicode charactors to their appropriate fonts
;; (use-package unicode-fonts
;;   :config
;;   (unicode-fonts-setup))

;; Turn off useless mode lighters
(use-package delight
  :config
  (delight '((rainbow-mode)
             (lsp-mode)
             (whitespace-cleanup-mode)
             (tree-sitter-mode          nil tree-sitter)
             (tree-sitter-hl-mode       nil tree-sitter-hl)
             (python-black-on-save-mode nil python-black)
             (auto-fill-function        nil t)
             (isearch-mode              nil isearch)
             (abbrev-mode               nil abbrev)
             (purpose-mode              nil window-purpose)
             (eldoc-mode                nil eldoc)
             (move-dup-mode             nil move-dup)
             (smartparens-mode          nil smartparens)
             (which-key-mode            nil which-key)
             (auto-revert-mode          nil autorevert)
             (visual-line-mode          nil simple)
             (subword-mode              nil subword))))

;; Theme
(use-package solarized-theme
  :if (display-graphic-p)
  :config
  (load-theme 'solarized-dark t)

  ;; (pcase-dolist (`(,face . ,spec)
  ;;                '((region . ((((type ns))
  ;;                              (:background "selectedTextBackgroundColor" :foreground "selectedTextColor"))))))
  ;;   (put face 'theme-face nil)
  ;;   (face-spec-set face spec))

  (pcase-dolist (`(,face . ,alias)
                 '((all-the-icons-dired-dir-face . dired-directory)
                   (icomplete-first-match        . ido-first-match)
                   (completions-common-part      . flx-highlight-face)))
    (put face 'theme-face nil)
    (put face 'face-alias alias))

  (let ((line (face-attribute 'mode-line :underline)))
    (if window-divider-mode
        (progn
          (set-face-attribute 'window-divider nil :foreground line)
          (set-face-attribute 'mode-line nil :overline line :underline nil :box nil)
          (set-face-attribute 'mode-line-inactive nil :overline line :underline nil :box nil))
      (set-face-attribute 'mode-line nil :overline line :box nil)
      (set-face-attribute 'mode-line-inactive nil :overline line :underline line :box nil)))

  (with-eval-after-load 'dired
    (set-face-attribute 'dired-header nil :underline t :foreground nil :background nil)))

;; Fancy mode line
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-toggle-buffer-encoding-abbrev-off))

;; Replace the major mode name with its icon
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (with-eval-after-load 'powerline
    (defun powerline-major-mode-advice (fn &rest args)
      (let* ((major-mode-segment (apply fn args))
             (props (text-properties-at 0 major-mode-segment))
             (icon (all-the-icons-icon-for-mode major-mode))
             (face-prop (and (stringp icon) (get-text-property 0 'face icon))))
        (if face-prop
            (apply 'propertize icon 'face face-prop props)
          major-mode-segment)))
    (advice-add 'powerline-major-mode :around 'powerline-major-mode-advice)))

;; Sets $MANPATH, $PATH and exec-path from your shell, but only on OS X. This
;; should be done ASAP on init.
(use-package exec-path-from-shell
  :if (memq (window-system) '(mac ns))
  :config (exec-path-from-shell-initialize))

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Turn on line wrapping for programming, text and message buffers
(dolist (hook '(prog-mode-hook text-mode-hook messages-buffer-mode-hook Custom-mode-hook))
  (add-hook hook 'visual-line-mode))

;; Turn on subword mode and linum mode for all prog and text modes
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda ()
                   (subword-mode 1)
                   (if (fboundp 'display-line-numbers-mode)
                       (display-line-numbers-mode 1)
                     (linum-mode 1)
                     ;; Renumber the current buffer after reverting the buffer
                     (add-hook 'after-revert-hook 'linum-update-current)))))

;; More sensible comment-dwim
(with-eval-after-load 'newcomment
  (defun comment-dwim-advice (comment-dwim &rest args)
    "Replacement for the `comment-dwim' command.

If no region is selected and point is not at the end of the line,
comment the current line from the front, otherwise the
commentable lines in the region.  Replaces the default behaviour
of `comment-dwim', where it inserts comment at the end of the
line if no region is defined.

Optional argument ARG same as `comment-dwim''s."
    (interactive "*P")
    (if (and (not (use-region-p)))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (apply comment-dwim args)))
  (advice-add 'comment-dwim :around 'comment-dwim-advice))

;; I hate X mouse bindings
(when (and (display-graphic-p)
           (not (memq (window-system) '(x))))
  (global-set-key (kbd "<mouse-3>") 'mouse-buffer-menu))

;; tab-bar mode can't be used on macOS, so rebinding it to something useful.
(global-set-key (kbd "C-x t") 'display-time-world)

;; Completely unbind annoying abbrev, dabbrev, expand, hippie-expand. These
;; ancient completion commands are just too stupid for this day and age
(global-unset-key (kbd "M-'"))
(global-unset-key (kbd "M-/"))
(global-unset-key (kbd "C-x '"))
;; Always use M-g prefix to jump between errors
(global-unset-key (kbd "C-x `"))
;; Must there be 4 bindings to undo?
(global-unset-key (kbd "C-x u"))
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "s-z"))

;; Bind useful things to keys
(pcase-dolist (`(,key . ,command)
               '(("<backtab>" . align)
                 ("C-x f"     . follow-mode)
                 ;; Replace default buffer menu with ibuffer
                 ("C-x C-b"   . ibuffer)
                 ("C-t"       . transpose-sexps)
                 ("M-:"       . ielm)
                 ("C-x ^"     . nil)
                 ("C-{"       . enlarge-window)
                 ("C-}"       . shrink-window)
                 ("C-c l"     . browse-url-at-point)))
  (global-set-key (kbd key) command))

;; Replace zap-to-char with the hidden zap-up-to-char
(autoload 'zap-up-to-char "misc")
(fset 'zap-to-char 'zap-up-to-char)

;; Not that I use occur very often, but when I do, I'd like its keybindings the
;; same as grep mode's
(with-eval-after-load 'replace
  (pcase-dolist (`(,key . ,command)
                 '(("M-n" . nil)
                   ("M-p" . nil)
                   ("n"   . occur-next)
                   ("p"   . occur-prev)))
    (define-key occur-mode-map (kbd key) command)))

(with-eval-after-load 'compile
  (pcase-dolist (`(,key . ,command)
                 '(("M-{" . nil)
                   ("M-}" . nil)
                   ("M-n" . nil)
                   ("M-p" . nil)
                   ("{"   . compilation-previous-file)
                   ("}"   . compilation-next-file)
                   ("p"   . compilation-previous-error)
                   ("n"   . compilation-next-error)))
    (define-key compilation-minor-mode-map (kbd key) command)
    (define-key compilation-mode-map (kbd key) command)))

;; Persistent history for all the inferior modes
(add-hook 'comint-mode-hook
          (lambda ()
            (let ((proc (get-buffer-process (current-buffer))))
              (when proc
                (setq-local comint-input-ring-file-name
                            (expand-file-name
                             (format ".%s-history" major-mode)
                             user-emacs-directory))
                (when (file-exists-p comint-input-ring-file-name)
                  (let ((comint-input-ring-separator "\n"))
                    (comint-read-input-ring)))
                (set-process-sentinel
                 proc
                 (lambda (&rest _)
                   (let ((comint-input-ring-separator "\n"))
                     (comint-write-input-ring))
                   (quit-window 'kill)))))))

(use-package osx-trash
  :if (and (eq system-type 'darwin)
           (not (fboundp 'system-move-file-to-trash)))
  :config (osx-trash-setup))

;; Turn on keyboard shortcut remainder
(use-package which-key
  :delight
  :bind (("C-h b" . which-key-show-top-level)
         ("C-h m" . which-key-show-major-mode)))

;; Visual alignment
(use-package ialign
  :bind ("<A-tab>" . ialign))

;; Other missing essentials that I don't want to write
(use-package crux
  :bind (("C-x C-u" . crux-upcase-region)
         ("C-x C-l" . crux-downcase-region)
         ("C-x M-c" . crux-capitalize-region)
         ("C-c d"   . crux-delete-file-and-buffer)
         ("C-c m"   . crux-rename-file-and-buffer)
         ("C-c c"   . crux-copy-file-preserve-attributes)
         ("C-c u"   . crux-find-user-init-file)
         ("C-c ,"   . crux-find-user-custom-file)
         ("C-c s"   . crux-find-shell-init-file)
         ("C-c C-u" . crux-sudo-edit)
         ("C-c M-o" . crux-open-with)
         :map emacs-lisp-mode-map
         ("C-c e e" . crux-eval-and-replace))
  :config
  (setq crux-shell shell-file-name))

;; Saner undo/redo
(use-package undo-fu
  :bind (("M-z"   . undo-fu-only-undo)
         ("C-M-z" . undo-fu-only-redo)))

;; So I can see past kills that I can yank
(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

;; Sane keyboard scrolling
(use-package pager-default-keybindings)

;; Adjust frame-wide font size
(use-package default-text-scale
  :bind (("C-x C-=" . default-text-scale-increase)
         ("C-x C--" . default-text-scale-decrease)
         ("C-x C-0" . default-text-scale-reset)))

;; Enhances ido and isearch's fuzzy search
(use-package flx-ido
  :config (flx-ido-mode 1))

(use-package flx-isearch
  :bind (("C-M-s" . flx-isearch-forward)
         ("C-M-r" . flx-isearch-backward))
  :config (flx-isearch-mode 1))

;; Use ido with M-x
(use-package amx
  :bind (("M-X" . amx-major-mode-commands)))

;; Use ido for even more things than ido-everywhere
(use-package crm-custom
  :config
  (crm-custom-mode 1))

(use-package ido-completing-read+)

(use-package ido-vertical-mode
  :demand
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n"    . icomplete-forward-completions)
              ("<up>"   . icomplete-backward-completions)
              ("C-p"    . icomplete-backward-completions)
              ("C-v"    . icomplete-vertical-toggle)))

(use-package icomplete-vertical)

;; Convenient iMenu entry search
(use-package imenu-anywhere
  :bind (("C-\\" . imenu-anywhere)))

;; Turn on iMenu for code outlines for all prog and text modes, if possible
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (ignore-errors (imenu-add-menubar-index)))))

;; Enable `xref-pop-marker-stack' to go back to where I was before clicking on
;; an imenu item
(add-hook 'imenu-after-jump-hook
          (lambda () (when (mark) (xref-push-marker-stack (mark-marker)))))

;; More sensible begin and end in certain modes
(use-package beginend
  :after (delight)
  :hook ((dired-mode          . beginend-dired-mode)
         (magit-status-mode   . beginend-magit-status-mode)
         (message-mode        . beginend-message-mode)
         (prog-mode           . beginend-prog-mode)
         (occur-mode          . beginend-occur-mode)
         (ibuffer-mode        . beginend-ibuffer-mode)
         (vc-dir-mode         . beginend-vc-dir-mode)
         (recentf-dialog-mode . beginend-recentf-dialog-mode)
         (compilation-mode    . beginend-compilation-mode)
         (rg-mode             . beginend-rg-mode))
  :config
  (map-values-apply (lambda (mode) (delight mode nil t)) beginend-modes))

;; Mark and edit multiple things at once
(use-package multiple-cursors
  :bind (("M-s M-e" . mc/edit-lines)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-M->"   . mc/skip-to-next-like-this)
         ("C-M-<"   . mc/skip-to-previous-like-this)
         ("M-s C->" . mc/mark-all-dwim)))

;; Mark and edit occurrences
(use-package iedit)

;; Construct regexp and search visually and incrementally
(use-package visual-regexp-steroids
  :bind (("M-%"   . vr/replace)
         ("C-M-%" . vr/query-replace)
         ("C-s"   . vr/isearch-forward)
         ("C-r"   . vr/isearch-backward)
         ("M-s m" . vr/mc-mark)))

;; More convenient region selection
(use-package expand-region
  :commands (er/contract-region)
  :bind (("M-=" . er/expand-region)
         ("M--" . er/contract-region)
         ([remap kill-ring-save] . expand-region-or-kill-ring-save))
  :preface
  (defun expand-region-or-kill-ring-save (beg end &optional arg)
    "Select things at point before saving the region to the kill ring.

Save region to kill ring if there is one, otherwise expand the
region at point, optionally ARG number of times before saving the
region."
    (interactive "r\np")
    (if (not (use-region-p))
        (progn
          (er/expand-region arg)
          (read-only-mode 1)
          (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "=") 'er/expand-region)
            (define-key keymap (kbd "-") 'er/contract-region)
            (define-key keymap (kbd "RET") 'kill-ring-save)
            (set-transient-map keymap t (lambda () (read-only-mode 0)))))
      (kill-ring-save beg end)))

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

;; Navigate source code by syntax
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

                          ("M-(" . sp-wrap-round)
                          ("M-[" . sp-wrap-square)
                          ("M-{" . sp-wrap-curly)
                          ("M-}" . sp-unwrap-sexp)
                          ("M-]" . sp-backward-unwrap-sexp)))))

;; Cross-machine fomatting
(use-package editorconfig
  :delight
  :config
  (add-to-list 'editorconfig-indentation-alist
               '(rjsx-mode js2-basic-offset sgml-basic-offset)))

;; Performance enhancement for files with really long lines
(use-package so-long)

;; Prettier form-feeds
(use-package form-feed
  :delight
  :hook (((prog-mode text-mode) . form-feed-mode)))

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
              ("C-c i" . yas-expand-from-trigger-key)))

(use-package yasnippet-snippets)

;; No sane person will program from right to left, so turn this major perf
;; bottleneck off
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local bidi-paragraph-direction 'left-to-right
                        bidi-inhibit-bpa nil)))

;; Turn on background color for HEX for specific modes
(use-package rainbow-mode
  :hook (emacs-lisp-mode web-mode js-mode typescript-mode sh-mode))

;; Cycle through most common programming identifier styles
(use-package string-inflection
  :preface
  (defun inflect-string ()
    (interactive)
    (cond ((derived-mode-p 'scala-mode 'java-mode 'js-mode 'typescript-mode 'go-mode)
           (string-inflection-java-style-cycle))
          ((derived-mode-p 'python-mode 'ruby-mode 'c-mode 'rust-mode)
           (string-inflection-python-style-cycle))
          ((derived-mode-p 'prog-mode)
           (string-inflection-all-cycle))))
  :config
  (setq string-inflection-skip-backward-when-done t)
  :bind (("C-x C-y" . inflect-string)))

(use-package smart-semicolon
  :delight
  :hook ((c-mode-common js-mode java-mode scala-mode css-mode scss-mode) . smart-semicolon-mode))

;; Cycle between quotes
(use-package cycle-quotes
  :bind (("C-x C-'" . cycle-quotes)))

;; Increment and decrement
(use-package shift-number
  :bind (("C-x =" . shift-number-up)
         ("C-x -" . shift-number-down)))

;; Modern code folding
(use-package origami
  :config
  (with-eval-after-load 'hideshow
    ;; Unloading is unsafe, so this the best I can do to pretend `hideshow'
    ;; never existed.
    (setq minor-mode-map-alist
          (assq-delete-all 'hs-minor-mode minor-mode-map-alist)
          minor-mode-alist
          (assq-delete-all 'hs-minor-mode minor-mode-alist)
          minor-mode-list
          (delq 'hs-minor-mode minor-mode-list)))
  :bind (:map origami-mode-map
              ("M-0"   . origami-open-all-nodes)
              ("M-9"   . origami-close-all-nodes)
              ("C-M-/" . origami-recursively-toggle-node)))

;; Minimum Distraction
(use-package olivetti
  :delight
  :bind ("C-c o" . olivetti-mode))

;; Much faster PDF viewing
(add-hook 'doc-view-mode-hook
          (lambda ()
            (use-package pdf-tools
              :config (pdf-tools-install))))

;; Modern tree-based syntax-highlighting
(use-package tree-sitter-langs
  :config
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

;; Static Analysis
(use-package lsp-mode
  :after (which-key)
  :hook (((c-mode-common
           caml-mode
           css-mode
           enh-ruby-mode
           go-mode
           js-mode
           python-mode
           reason-mode
           rust-mode
           sh-mode
           swift-mode
           typescript-mode
           web-mode)
          . (lambda ()
              (when (not (derived-mode-p 'json-mode))
                (lsp-deferred))))
         (lsp-managed-mode . lsp-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq read-process-output-max (* 1024 1024 10)))

(use-package lsp-jedi
  :after lsp)

(use-package lsp-sourcekit
  :after (lsp-mode))

(use-package lsp-origami
  :hook (lsp-after-open . lsp-origami-try-enable))

;; LSP debugging support
(use-package dap-mode
  :after (lsp-mode)
  :config
  (add-hook 'dap-stopped-hook
            (lambda (_) (call-interactively #'dap-hydra)))

  (setq dap-utils-extension-path (expand-file-name "~/.vscode/extensions"))

  (add-hook 'js-mode-hook
            (lambda ()
              (unless (derived-mode-p 'json-mode)
                (setq dap-chrome-debug-path
                      (car (last (file-expand-wildcards (concat dap-utils-extension-path "/msjsdiag.debugger-for-chrome-*")))))
                (setq dap-chrome-debug-program
                      (concat "node " dap-chrome-debug-path "/out/src/chromeDebug.js"))
                (require 'dap-chrome)

                (setq dap-firefox-debug-path
                      (car (last (file-expand-wildcards
                                  (concat dap-utils-extension-path "/firefox-devtools.vscode-firefox-debug-*")))))
                (setq dap-firefox-debug-program
                      (concat "node " dap-firefox-debug-path "/dist/adaptor.bundle.js"))
                (require 'dap-firefox)

                (setq dap-node-debug-path
                      "/Applications/Visual\ Studio\ Code.app/Contents/Resources/app/extensions/ms-vscode.node-debug2")
                (setq dap-node-debug-program
                      (concat "node " dap-node-debug-path "/out/src/nodeDebug.js"))
                (require 'dap-node))))

  (add-hook 'python-mode-hook
            (lambda ()
              (setq dap-python-debugger 'debugpy)
              (require 'dap-python)))

  (add-hook 'go-mode-hook
            (lambda ()
              (setq dap-go-debug-path
                    (car (last (file-expand-wildcards
                                (concat dap-utils-extension-path "/golang.go-*")))))
              (setq dap-go-debug-program
                    (concat "node" dap-go-debug-path "/dist/debugAdaptor2.js"))
              (require 'dap-go)))

  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq dap-lldb-debug-path
                    (car (last (file-expand-wildcards (concat dap-utils-extension-path "/lanza.lldb-vscode-*")))))
              (setq dap-lldb-debug-program
                    (concat dap-lldb-debug-path (concat "/bin/" (symbol-name system-type) "/bin/lldb-vscode")))
              (require 'dap-lldb)

              (setq dap-cpptools-debug-path
                    (car (last (file-expand-wildcards
                                (concat dap-utils-extension-path "/ms-vscode.cpptools-*")))))
              (setq dap-cpptools-debug-program
                    (concat dap-cpptools-debug-path "/bin/cpptools"))
              (require 'dap-cpptools))))

;; Auto-completion
(use-package company
  :delight
  :preface
  (defun company-complete-common-or-cycle-next ()
    (interactive)
    (company-complete-common-or-cycle 1))
  (defun company-complete-common-or-cycle-previous ()
    (interactive)
    (company-complete-common-or-cycle -1))
  :bind (:map company-mode-map
              ("M-/" . company-manual-begin)
              :map company-active-map
              ("M-n" . company-complete-common-or-cycle-next)
              ("M-p" . company-complete-common-or-cycle-previous)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-backends
        `(company-bbdb
          (company-semantic company-clang)
          company-cmake
          company-files
          (company-capf :with company-yasnippet)
          (company-dabbrev-code
           company-gtags
           company-etags
           company-keywords))))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-box
  :delight
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (add-hook 'company-prescient-mode-hook
            (lambda ()
              ;; Make sure `company-sort-prefer-same-case-prefix' is always at the back
              (delq 'company-sort-prefer-same-case-prefix company-transformers)
              (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix t))))

(use-package orderless
  :custom (completion-styles '(orderless))
  :config
  (with-eval-after-load 'company
    (defun company-capf--candidates-advice (fn &rest args)
      (let ((orderless-match-faces [completions-common-part]))
        (apply fn args)))
    (advice-add 'company-capf--candidates :around #'company-capf--candidates-advice))

  (with-eval-after-load 'icomplete
    (add-hook 'minibuffer-setup-hook
              (lambda ()
                (when fido-mode
                  (setq-local completion-styles '(orderless))))
              t)))

;; Linting
(use-package flycheck
  :delight
  :config
  (global-flycheck-mode 1))

(use-package flycheck-pos-tip
  :after (flycheck)
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-pyre
  :after (flycheck)
  :hook (flycheck-mode . flycheck-pyre-setup))

;; REST API
(use-package restclient
  :commands restclient-mode
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :config
  (add-hook 'restclient-mode-hook
            (lambda ()
              (use-package company-restclient
                :after (company)
                :config
                (setq-local company-backends `(company-restclient))))))

;; Term and shell
(with-eval-after-load 'shell
  (use-package native-complete
    :config (native-complete-setup-bash)))

(add-hook 'sh-mode-hook
          (lambda ()
            (use-package company-native-complete
              :after (company)
              :config
              (setq-local company-backends
                          '(company-native-complete company-files company-capf)))))

(setq eshell-directory-name (expand-file-name ".eshell/" user-emacs-directory))

(use-package vterm
  :preface
  (defun vterm--sentinel-advice (process event)
    "Sentinel of vterm PROCESS.
Argument EVENT process event.

Runs `vterm-exit-functions' before exiting.

If `vterm-kill-buffer-on-exit' is t, kills the buffer and
optionally the window if possible."
    (let* ((buffer (process-buffer process))
           (window (get-buffer-window buffer)))
      (when (string-match "\\(finished\\|exited\\)" event)
        (run-hook-with-args 'vterm-exit-functions
                            (if (buffer-live-p buffer) buffer nil)
                            event)
        (when (and vterm-kill-buffer-on-exit (buffer-live-p buffer))
          (kill-buffer buffer)
          (let ((next-vterm-buffer (seq-find (lambda (buffer)
                                               (with-current-buffer buffer
                                                 (and (derived-mode-p 'vterm-mode)
                                                      (buffer-live-p buffer))))
                                             (buffer-list))))
            (when (and (window-live-p window)
                       (not (window-dedicated-p window)))
              (if (not next-vterm-buffer)
                  (ignore-errors (delete-window window))
                (switch-to-buffer next-vterm-buffer))))))))
  :bind (("M-T" . vterm)
         :map vterm-mode-map
         ([remap backward-kill-word] . vterm--self-insert)
         ([remap sp-kill-hybrid-sexp] . vterm--self-insert))
  :config
  (advice-add 'vterm--sentinel :override 'vterm--sentinel-advice))

(use-package eterm-256color
  :config
  (with-eval-after-load 'term
    (add-hook 'term-mode 'eterm-256color-mode)))

;; Markup and config languages
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package dotenv-mode
  :mode "\\.env\\..*\\'")

(use-package flycheck-yamllint
  :after (flycheck yaml-mode)
  :config (flycheck-yamllint-setup))

;; Emacs Lisp
(pcase-dolist (`(,key . ,command)
               '(("C-c e f" . byte-compile-file)
                 ("C-c e c" . emacs-lisp-byte-compile)
                 ("C-c e l" . emacs-lisp-byte-compile-and-load)
                 ("C-c e b" . eval-buffer)
                 ("C-c e r" . eval-region)))
  (define-key emacs-lisp-mode-map (kbd key) command))

(add-hook 'ielm-mode-hook (lambda () (ielm-change-working-buffer (window-buffer (selected-window)))))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e x" . macrostep-expand)))

(use-package cl-lib-highlight
  :config
  (cl-lib-highlight-initialize)
  (cl-lib-highlight-warn-cl-initialize))

(use-package elisp-def
  :delight
  :hook (emacs-lisp-mode . elisp-def-mode))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)
         ("C-h p" . helpful-at-point)))

;; C/C++/Objective-C
(use-package flycheck-objc-clang
  :after (flycheck)
  :config (flycheck-objc-clang-setup))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

;; Formatting
(use-package reformatter
  :quelpa (reformatter :fetcher github :repo "wyuenho/reformatter.el" :branch "post-processor")
  :config
  (reformatter-define yarn-eslint-format
    :program "yarn"
    :args `("--silent"
            "eslint"
            "--fix-dry-run"
            "--format"
            "json"
            "--stdin"
            "--stdin-filename"
            ,buffer-file-name
            "--ext"
            ".json,.js,.jsx,.mjs,.mjsx,.cjs,.cjsx,.ts,.tsx")
    :output-processor (lambda (output-file result-callback)
                        (let* ((data (ignore-error 'json-end-of-file (json-read-file output-file)))
                               (output (and data (arrayp data) (alist-get 'output (aref data 0)))))
                          (funcall result-callback output)))
    :exit-code-success-p integerp)
  (reformatter-define eslint-format
    :program "eslint_d"
    :args `("--fix-to-stdout"
            "--stdin"
            "--stdin-filename"
            ,buffer-file-name
            "--ext"
            ".json,.js,.jsx,.mjs,.mjsx,.cjs,.cjsx,.ts,.tsx")))

(dolist (mode '(css-mode js-mode markdown-mode scss-mode typescript-mode web-mode yaml-mode))
  (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook mode-hook
              (lambda ()
                (let ((formatter
                       (when-let* ((package-json-dir
                                    (locate-dominating-file default-directory "package.json"))

                                   (package-json
                                    (if package-json-dir
                                        (json-read-file (concat
                                                         (expand-file-name package-json-dir)
                                                         "package.json"))
                                      nil))

                                   (devDependencies
                                    (if package-json
                                        (alist-get 'devDependencies package-json)
                                      nil))

                                   (formatter-styles
                                    '((prettier prettier-eslint prettier)
                                      (eslint eslint-plugin-prettier eslint))))

                         (car (seq-filter 'identity
                                          (map-apply (lambda (command packages)
                                                       (and
                                                        (seq-some
                                                         (lambda (package) (map-contains-key devDependencies package)) packages)
                                                        command))
                                                     formatter-styles)))))
                      (yarn-pnp-p (seq-some 'file-exists-p
                                            (list (concat default-directory ".pnp.js")
                                                  (concat default-directory ".pnp.cjs")))))
                  (unless yarn-pnp-p
                    (use-package add-node-modules-path
                      :config (add-node-modules-path)))
                  (cond
                   ((and (eq formatter 'eslint)
                         (or (not (featurep 'lsp-mode))
                             (with-eval-after-load 'lsp-mode
                               (or (not lsp-mode)
                                   (and lsp-mode
                                        (member 'eslint
                                                (lsp-foreach-workspace
                                                 (lsp--workspace-server-id lsp--cur-workspace))))))))
                    (if (or yarn-pnp-p (executable-find "yarn"))
                        (progn
                          (define-key (derived-mode-map-name mode) (kbd "C-c f") 'yarn-eslint-format-buffer)
                          (yarn-eslint-format-on-save-mode))
                      (progn
                        (define-key (derived-mode-map-name mode) (kbd "C-c f") 'eslint-format-buffer)
                        (eslint-format-on-save-mode))))
                   ((eq formatter 'prettier)
                    (use-package prettier
                      :delight
                      :config
                      (prettier-mode)
                      (define-key (derived-mode-map-name mode) (kbd "C-c f") 'prettier-prettify)))))))))

;; Node
(add-hook 'js-mode-hook
          (lambda ()
            (use-package import-js
              :bind (:map js-mode-map
                          ("C-c t i"   . import-js-import)
                          ("C-c t f"   . import-js-fix)
                          ("C-c t M-." . import-js-goto))
              :config (run-import-js))

            (use-package js-doc
              :bind (:map js-mode-map
                          ("C-c C-d m" . js-doc-insert-file-doc)
                          ("C-c C-d f" . js-doc-insert-function-doc-snippet)))

            (use-package nodejs-repl
              :bind(:map js-mode-map
                         ("C-c e e" . nodejs-repl-send-last-expression)
                         ("C-c e r" . nodejs-repl-send-region)
                         ("C-c e b" . nodejs-repl-send-buffer)
                         ("C-c e l" . nodejs-repl-load-file)
                         ("C-c M-:" . nodejs-repl-switch-to-repl)))

            (define-key js-mode-map [menu-bar] nil)
            (define-key js-mode-map (kbd "M-.") nil)))

(use-package js2-mode
  :interpreter ("node" . js2-mode)
  :config
  (when (fboundp 'sp-kill-whole-line)
    (define-key js2-mode-map (kbd "C-k") 'sp-kill-whole-line)))

(use-package rjsx-mode
  :mode ("\\.jsx?\\'" "\\.mjs\\'"))

(use-package json-mode
  :config
  (define-key json-mode-map (kbd "C-c C-f") nil)
  (add-hook 'json-mode-hook
            (lambda ()
              (when (not (key-binding (kbd "C-c f")))
                (define-key json-mode-map (kbd "C-c f") 'json-pretty-print-buffer)))))

(use-package polymode
  ;; :after rjsx-mode
  ;; :config
  ;; (define-hostmode poly-rjsx-hostmode nil
  ;;   "RJSX hostmode."
  ;;   :mode 'rjsx-mode)
  ;; (define-innermode poly-rjsx-graphql-innermode nil
  ;;   :mode 'graphql-mode
  ;;   :head-matcher "graphql\`"
  ;;   :tail-matcher "\`"
  ;;   :head-mode 'host
  ;;   :tail-mode 'host)
  ;; (define-polymode poly-rjsx-mode
  ;;   :hostmode 'poly-rjsx-hostmode
  ;;   :innermodes '(poly-rjsx-graphql-innermode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . poly-rjsx-mode))
  )

(use-package poly-markdown)

;; TypeScript
(use-package typescript-mode
  :mode ("\\.ts\\'"))

(use-package ts-comint
  :after (typescript-mode)
  :bind (:map typescript-mode-map
              ("C-x C-e" . ts-send-last-sexp)
              ("C-c e e" . ts-send-last-sexp-and-go)
              ("C-c e r" . ts-send-region-and-go)
              ("C-c e b" . ts-send-buffer-and-go)
              ("C-c e l" . ts-load-file-and-go)
              ("C-c M-:" . switch-to-ts)))

;; Python
(add-to-list 'auto-mode-alist '("\\.pythonrc\\'" . python-mode))
(add-hook 'python-mode-hook
          (lambda ()
            (use-package python-docstring
              :delight
              :config (python-docstring-mode))

            (use-package poetry)

            (when-let* ((requirements
                         (ignore-errors (process-lines "pip" "list" "--format=freeze")))
                        (requirements
                         (mapcar (lambda (req) (car (split-string req "=="))) requirements)))

              (use-package python-black
                :config
                (python-black-on-save-mode (if (member "black" requirements) 1 0)))

              (use-package importmagic
                :delight
                :config
                (importmagic-mode (if (member "importmagic" requirements) 1 0))
                (bind-key "C-c f i" 'importmagic-fix-import python-mode-map)
                (unbind-key "C-c C-l" 'importmagic-mode-map))

              ;; Don't add `py-isort-before-save' to `before-save-hook' or the
              ;; undo history will be very messed up
              (use-package py-isort
                :config
                (when (member "isort" requirements)
                  (bind-key "C-c f s" 'py-isort-buffer python-mode-map))))))

;; Ruby
(use-package yard-mode)
(use-package enh-ruby-mode
  :mode "\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\|irbrc\\)\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Puppet\\|Berks\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :interpreter ("ruby1.8" "ruby1.9" "jruby" "rbx" "ruby")
  :config (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
  :hook yard-mode)

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save nil 'local)
  (add-hook 'go-mode-hook
            (lambda ()
              (with-eval-after-load 'dap-mode
                (require 'dap-go)))))

(use-package flycheck-golangci-lint
  :after (go-mode)
  :config (flycheck-golangci-lint-setup))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (use-package cargo
                :delight
                :config (cargo-minor-mode)))))

;; Swift
(use-package swift-mode
  :mode "\\.swift\\'")

;; Scala
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; API Blueprints
(use-package apib-mode
  :mode "\\.apib\\'")

;; Web
(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package web-mode
  :functions web-mode-language-at-pos
  :mode ("\\.vue\\'"
         "\\.tsx\\'"
         "\\.handlebars\\'"
         "\\.underscore\\'"
         "\\.html?\\'"
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
              (use-package company-web-html
                :after (company)
                :config
                (setq-local company-backends
                            `(company-web-html
                              company-yasnippet
                              company-files)))

              ;; Setup flycheck
              (let ((ext (file-name-extension buffer-file-name)))
                (when (fboundp 'flycheck-add-mode)
                  (cond ((string-equal "tsx" ext)
                         (flycheck-add-mode 'typescript-tslint 'web-mode))
                        ((string-equal "css" ext)
                         (flycheck-add-mode 'css-stylelint 'web-mode))
                        ((string-equal "scss" ext)
                         (flycheck-add-mode 'scss-stylelint 'web-mode))
                        (t nil)))))))

(use-package emmet-mode
  :delight
  :after (:any web-mode js2-mode rjsx-mode)
  :hook (sgml-mode nxml-mode web-mode js-jsx-mode js2-jsx-mode rjsx-mode)
  :config
  (define-key emmet-mode-keymap (kbd "C-c C-c w") nil)
  (define-key emmet-mode-keymap (kbd "C-c C-m w") 'emmet-wrap-with-markup)
  (add-hook 'emmet-mode-hook
            (lambda ()
              (when (or (member major-mode '(js-jsx-mode js2-jsx-mode rjsx-mode))
                        (and (eq major-mode 'web-mode)
                             (member (web-mode-language-at-pos) '("jsx" "tsx"))))
                (setq-local emmet-expand-jsx-className? t)))))

;; Project management
(use-package projectile
  :preface
  ;; Bridge projectile and project together so packages that depend on project
  ;; like eglot work
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))

  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function)))

(use-package projectile-rails
  :after projectile
  :delight
  :config
  (projectile-rails-global-mode)
  (add-hook 'projectile-rails-mode-hook
            (lambda ()
              (setq projectile-rails-mode-map (make-sparse-keymap))
              (define-key projectile-command-map (kbd "C-r") 'projectile-rails-command-map))))

;; Search
(use-package ag
  :bind (("M-s a" . ag)))

(use-package wgrep-ag
  :after (ag))

(use-package rg
  :config
  (rg-enable-default-bindings)
  (with-eval-after-load 'projectile
    (define-key projectile-command-map (kbd "s r") 'rg-project))
  (add-hook 'rg-mode-hook 'wgrep-rg-setup))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; Version Control

;; Save window config before ediff starts and restores it and cleans up when it quits, sanity!
(with-eval-after-load 'ediff
  (defvar ediff-saved-window-configuration)

  (add-hook 'ediff-before-setup-hook
            (lambda ()
              (setq ediff-saved-window-configuration (current-window-configuration))))

  (let ((restore-window-configuration
         (lambda ()
           (set-window-configuration ediff-saved-window-configuration))))
    (add-hook 'ediff-quit-hook restore-window-configuration 'append)
    (add-hook 'ediff-suspend-hook restore-window-configuration 'append))

  (add-hook 'ediff-cleanup-hook
            (lambda ()
              (eval-and-compile (require 'ediff-util))
              (ediff-janitor nil nil)) 'append))

(use-package diff-hl
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  (diff-hl-flydiff-mode 1)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))
  (with-eval-after-load 'magit
    (add-hook 'magit-post-commit-hook 'diff-hl-update)
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package magit
  :config
  (add-hook 'magit-post-refresh-hook 'vc-refresh-state))

(use-package forge
  :after (magit)
  :config
  (add-hook 'forge-post-mode-hook
            (lambda ()
              (when (forge-github-repository-p
                     (forge-get-repository forge--buffer-post-object))
                (auto-fill-mode -1))))
  (remove-hook 'forge-post-mode-hook 'turn-on-flyspell))

(use-package magit-todos
  :quelpa (magit-todos :fetcher github :repo "wyuenho/magit-todos" :branch "fix-95")
  :after (magit)
  :config (magit-todos-mode 1))

(use-package magit-lfs
  :after (magit))

(use-package abridge-diff
  :after (magit)
  :delight)

(use-package monky
  :bind (("C-x v M-m" . monky-status)))

;; File management
(defun add-to-invisibility-spec-override-advice (element)
  "Add ELEMENT to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added.

If `buffer-invisibility-spec' isn't a list before calling this
function, `buffer-invisibility-spec' will afterwards be a list
with the value `(t ELEMENT)'.  This means that if text exists
that invisibility values that aren't either `t' or ELEMENT, that
text will become visible.

ELEMENT is only added once."
  (if (eq buffer-invisibility-spec t)
      (setq buffer-invisibility-spec (list t)))
  (setq buffer-invisibility-spec
        (delete-dups (cons element buffer-invisibility-spec))))
(advice-add 'add-to-invisibility-spec :override 'add-to-invisibility-spec-override-advice)

(with-eval-after-load 'dired
  ;; Font lock symlinks
  (defface dired-executable
    '((t (:inherit font-lock-warning-face :weight normal)))
    "Face used for symbolic links."
    :group 'dired-faces)

  (defvar dired-executable-face 'dired-executable)

  (defun dired-executable-matcher (end)
    (let ((file (dired-file-name-at-point)))
      (when (and (file-executable-p file)
                 (not (file-symlink-p file)))
        (re-search-forward ".+" end t))))

  (font-lock-add-keywords
   'dired-mode
   (list
    (list dired-re-exe
          (list 'dired-executable-matcher
                '(dired-move-to-filename)
                nil
                '(0 dired-executable-face)))))

  ;; Don't save point position in dired buffers
  (defun save-place--setup-hooks-after-advice (&rest _)
    (remove-hook 'dired-initial-position-hook 'save-place-dired-hook))
  (advice-add 'save-place--setup-hooks :after 'save-place--setup-hooks-after-advice)

  (add-hook 'dired-mode-hook
            (lambda ()
              ;; So desktop mode will save this buffer local to the session
              (add-minor-mode 'dired-hide-details-mode "")
              ;; Add binding to open file in native app
              (when (memq (window-system) '(mac ns))
                (define-key dired-mode-map (kbd "z")
                  (lambda ()
                    (interactive)
                    (let ((file-name (dired-get-file-for-visit)))
                      (start-process "default-app" nil "open" file-name))))))))

(use-package shrink-path
  :config
  (add-hook 'dired-after-readin-hook
            (lambda ()
              (save-excursion
                (goto-char (point-min))
                (when (looking-at dired-subdir-regexp)
                  (let* ((inhibit-read-only t)
                         (beg (match-beginning 1))
                         (end (match-end 1))
                         (subdir (buffer-substring beg end))
                         (shrunken-subdir (shrink-path-dirs subdir)))
                    (make-button beg end 'display shrunken-subdir 'help-echo subdir)))))))

(use-package all-the-icons-dired
  :after (all-the-icons dired-collapse)
  :quelpa (all-the-icons-dired :fetcher github :repo "wyuenho/all-the-icons-dired" :branch "master")
  :if (display-graphic-p)
  :hook (dired-collapse-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :demand
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode)))

(use-package dired-single
  :after (dired-hide-dotfiles)
  :bind (:map dired-mode-map
              ("^"         . dired-single-up-directory)
              ("<mouse-1>" . dired-single-buffer-mouse)
              ("\C-m"      . dired-single-buffer))
  :config
  (with-eval-after-load 'window-purpose-x
    (setq dired-single-magic-buffer-name purpose-x-code1-dired-buffer-name))
  ;; Make sure dired-hide-details-mode is preserved when reusing the dired
  ;; window
  (defun find-alternate-file-advice (fn &rest args)
    "Preserve inherited parent dired buffer state if invoked from a dired buffer."
    (let ((is-dired (derived-mode-p 'dired-mode))
          (hide-dotfiles (and (boundp 'dired-hide-dotfiles-mode) dired-hide-dotfiles-mode))
          (hide-details dired-hide-details-mode)
          (hide-information-lines dired-hide-details-hide-information-lines)
          (hide-symlink-targets dired-hide-details-hide-symlink-targets)
          (tl truncate-lines)
          (ww word-wrap)
          (vlm visual-line-mode)
          (mlf mode-line-format)
          (arv auto-revert-verbose)
          (result (apply fn args)))
      (when is-dired
        (when hide-dotfiles (dired-hide-dotfiles-mode))
        (setq truncate-lines tl
              word-wrap ww
              visual-line-mode vlm
              mode-line-format mlf
              auto-revert-verbose arv)
        (setq-local dired-hide-details-hide-information-lines hide-information-lines)
        (setq-local dired-hide-details-hide-symlink-targets hide-symlink-targets)
        (when hide-details (dired-hide-details-mode)))
      result))
  (advice-add 'find-alternate-file :around 'find-alternate-file-advice))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

;; Window management
(with-eval-after-load 'recentf
  (define-key recentf-dialog-mode-map [remap recentf-cancel-dialog] 'quit-window))

(with-eval-after-load 'wid-browse
  (define-key widget-browse-mode-map [remap bury-buffer] 'quit-window))

(with-eval-after-load 'frameset
  ;; Let the themes deal with these things
  (dolist (param '(background-mode tty-color-mode screen-gamma
                   alpha font foreground-color background-color
                   mouse-color cursor-color border-color
                   scroll-bar-foreground scroll-bar-background))
    (if (assq param frameset-filter-alist)
        (setf (alist-get param frameset-filter-alist) :never)
      (push `(,param . :never) frameset-filter-alist)))

  (defun frameset--remove-unrestorable-file-buffers (window-tree)
    "Remove un-restorable buffers from window state."
    (let ((head (car window-tree))
          (tail (cdr window-tree)))
      (cond ((memq head '(vc hc))
             `(,head ,@(frameset--remove-unrestorable-file-buffers tail)))
            ((eq head 'leaf)
             (let* ((buffer (alist-get 'buffer tail))
                    (buffer-name (car buffer))
                    (filter (lambda (buffer) (string= buffer-name (car buffer))))
                    (next-buffers (alist-get 'next-buffers tail))
                    (prev-buffers (alist-get 'prev-buffers tail)))
               (if (get-buffer buffer-name)
                   window-tree
                 (cond ((and next-buffers (> (length next-buffers) 1))
                        (setf (alist-get 'next-buffers window-tree)
                              (seq-remove filter next-buffers)))
                       ((and prev-buffers (> (length prev-buffers) 1))
                        (setf (alist-get 'prev-buffers window-tree)
                              (seq-remove filter prev-buffers))))
                 (when prev-buffers
                   (setf (alist-get 'buffer tail) `(,(caar (alist-get 'prev-buffers window-tree)) ,@(cdr buffer))))
                 `(,head ,@tail))))
            ((null head) nil)
            (t (cons (if (and (listp head) (listp (cdr head)))
                         (frameset--remove-unrestorable-file-buffers head)
                       head)
                     (if (and (listp tail) (listp (cdr tail)))
                         (frameset--remove-unrestorable-file-buffers tail)
                       tail))))))

  (defun frameset--restore-frame-advice (fn &rest args)
    (let ((window-state (cadr args)))
      (apply fn
             (car args)
             (frameset--remove-unrestorable-file-buffers window-state)
             (cddr args))))
  (advice-add 'frameset--restore-frame :around 'frameset--restore-frame-advice))

;; Move around windows with shifted arrow keys
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package buffer-move
  :bind (("C-c b <left>"  . buf-move-left)
         ("C-c b <right>" . buf-move-right)
         ("C-c b <up>"    . buf-move-up)
         ("C-c b <down>"  . buf-move-down)))

(use-package imenu-list
  :quelpa (imenu-list :fetcher github :repo "wyuenho/imenu-list" :branch "code1"))

(use-package window-purpose
  :quelpa (window-purpose :fetcher github :repo "wyuenho/emacs-purpose" :files (:defaults "layouts")
                          :branch "improve-code1")
  :config
  (define-key purpose-mode-map (kbd "C-c ,") nil)
  (define-key purpose-mode-map (kbd "C-c w") purpose-mode-prefix-map)

  (purpose-add-user-purposes
   :modes '((message-mode . edit)
            (ag-mode      . search)
            (rg-mode      . search)
            (vterm-mode   . terminal)))

  (with-eval-after-load 'window-purpose-x
    (add-to-list 'purpose-x-popwin-buffer-names "*Messages*")
    (purpose-x-popwin-update-conf)

    (with-eval-after-load 'ido
      (add-to-list 'purpose-x-popwin-buffer-names ido-completion-buffer)
      (purpose-x-popwin-update-conf))

    (with-eval-after-load 'ispell
      (add-to-list 'purpose-x-popwin-buffer-names ispell-choices-buffer)
      (purpose-x-popwin-update-conf))

    (with-eval-after-load 'minibuffer
      (add-to-list 'purpose-x-popwin-buffer-names "*Completions*")
      (purpose-x-popwin-update-conf))

    (with-eval-after-load 'whitespace
      (add-to-list 'purpose-x-popwin-buffer-name-regexps
                   (concat (regexp-quote whitespace-report-buffer-name) "\\(<[[:digit:]]+>\\)*"))
      (add-to-list 'purpose-x-popwin-buffer-name-regexps
                   (concat (regexp-quote whitespace-help-buffer-name) "\\(<[[:digit:]]+>\\)*"))
      (purpose-x-popwin-update-conf)))

  (purpose-x-code1-setup)
  (purpose-x-popwin-setup)
  (purpose-x-kill-setup)
  (purpose-x-magit-single-on)

  (purpose-mode 1)

  (add-hook 'after-init-hook
            (lambda ()
              (when (file-exists-p purpose-default-layout-file)
                (purpose-load-window-layout-file))))

  ;; Bury all special buffers after setting up dummy buffers and restoring
  ;; session buffers
  (with-eval-after-load 'desktop
    (add-hook 'desktop-after-read-hook
              (lambda ()
                (dolist (buf (seq-filter
                              (lambda (buf)
                                (string-match-p "^ \\|\\*" (buffer-name buf)))
                              (buffer-list)))
                  (bury-buffer buf))))))
