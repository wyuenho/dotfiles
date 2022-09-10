;; -*- lexical-binding: t -*-
(toggle-debug-on-error)

(require 'cl-lib)
(require 'filenotify)
(require 'let-alist)
(require 'map)
(require 'pcase)
(require 'rx)
(require 'seq)
(require 'subr-x)

(set-locale-environment "UTF-8")
(custom-autoload 'package-selected-packages "package")
(custom-autoload 'package-activated-list "package")

;; Tell Custom to write and find the custom settings elsewhere
(defun load-custom-file ()
  "Load custom file.

Sets `custom-file' by searching for a file named `custom.el'
under `user-emacs-directory'.  If it exists, loaded it."
  (interactive)
  (when-let ((custom-file-path
              (locate-file "custom.el" (list user-emacs-directory))))
    (setf custom-file custom-file-path)
    (load custom-file)))
(load-custom-file)

;; Install selected by missing packages
(let ((missing (cl-set-difference
                package-selected-packages
                package-activated-list)))
  (when missing
    (with-demoted-errors "%s"
      (package-refresh-contents))
    (dolist (package missing)
      (with-demoted-errors "%s"
        (package-install package t)
        (package-activate package)))
    (add-hook 'window-setup-hook 'load-custom-file))
  (require 'quelpa-use-package)
  (setf use-package-compute-statistics t)
  (quelpa-use-package-activate-advice))

;; Turn off useless mode lighters
(use-package delight
  :config
  (delight '((whitespace-cleanup-mode)
             (aggressive-indent-mode nil aggressive-indent)
             (auto-fill-function     nil t)
             (isearch-mode           nil isearch)
             (abbrev-mode            nil abbrev)
             (purpose-mode           nil window-purpose)
             (eldoc-mode             nil eldoc)
             (move-dup-mode          nil move-dup)
             (smartparens-mode       nil smartparens)
             (which-key-mode         nil which-key)
             (auto-revert-mode       nil autorevert)
             (visual-line-mode       nil simple)
             (subword-mode           nil subword))))

;; Theme
(use-package solarized-theme
  :if (display-graphic-p)
  :config
  (load-theme 'solarized-dark t)

  (pcase-dolist (`(,face . ,alias)
                 '((all-the-icons-dired-dir-face      . dired-directory)
                   (icomplete-first-match             . ido-first-match)
                   (completions-common-part           . flx-highlight-face)
                   (company-tooltip-selection         . company-tooltip-mouse)
                   (tooltip                           . company-tooltip)
                   (lsp-ui-doc-background             . company-tooltip)))
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

(use-package direnv
  :quelpa (direnv :fetcher github :repo "wyuenho/emacs-direnv" :branch "early-reliable-update-env")
  :after (exec-path-from-shell))

(setf backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook (lambda ()
                            (unless (member major-mode '(yaml-mode markdown-mode))
                              (auto-fill-mode 1))))

;; Make sure xwidget buffers are killed when quitting window
(with-eval-after-load 'xwidget
  (when (functionp 'xwidget-webkit-browse-url)
    (setf browse-url-browser-function 'xwidget-webkit-browse-url)
    (function-put 'xwidget-webkit-browse-url 'browse-url-browser-kind 'internal))

  (when (boundp 'xwidget-webkit-mode-map)
    (define-key xwidget-webkit-mode-map (kbd "q")
      (lambda ()
        (interactive)
        (quit-window t))))

  (defun xwidget-kill-buffer-query-function-advice ()
    "Kill xwidget buffer without asking."
    (let ((xwidgets (get-buffer-xwidgets (current-buffer))))
      (or (not xwidgets)
          (not (memq t (mapcar #'xwidget-query-on-exit-flag xwidgets)))
          t)))

  (advice-add 'xwidget-kill-buffer-query-function :override 'xwidget-kill-buffer-query-function-advice))

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
                   (quit-window 'kill))))))
          t)

(use-package osx-trash
  :if (and (eq system-type 'darwin)
           (not (fboundp 'system-move-file-to-trash)))
  :config (osx-trash-setup))

;; Turn on keyboard shortcut remainder
(use-package which-key
  :delight
  :bind (("C-h b" . which-key-show-top-level)
         ("C-h m" . which-key-show-major-mode))
  :config
  (which-key-add-key-based-replacements "C-x 4" "window")
  (which-key-add-key-based-replacements "C-x 5" "frame")
  (which-key-add-keymap-based-replacements ctl-x-map
    "RET" "mule"
    "8" "unicode"
    "@" "event-apply"
    "X" "edebug"
    "x" "buffer"
    "a" "abbrev"
    "n" "narrow"
    "r" "register"
    "C-a" "gud")
  (with-eval-after-load 'vc
    (which-key-add-key-based-replacements "C-x v M" "vc-merge")))

;; More featureful package menu
(use-package paradox
  :quelpa (paradox :fetcher github :repo "wyuenho/paradox")
  :config
  (paradox-enable)
  (add-hook 'paradox-menu-mode-hook
            (lambda ()
              (defun paradox--key-descriptors-advice (fn &rest args)
                (let ((result (apply fn args)))
                  (cl-loop for i in result
                           collect (cl-loop for j in i
                                            if (and (stringp j) (string-prefix-p "filter" j))
                                            collect "/-filter"
                                            else
                                            collect j))))
              (advice-add 'paradox--key-descriptors :around 'paradox--key-descriptors-advice)

              (pcase-dolist (`(,key . ,command)
                             '(("F" . nil)
                               ("f" . nil)
                               ("s" . nil)
                               ("l" . nil)))
                (define-key paradox-menu-mode-map (kbd key) command))

              (define-key package-menu-mode-map (kbd "/ /") nil)

              (if paradox-menu-hail-hydra
                  (define-key paradox-menu-mode-map (kbd "/") 'hydra-paradox-filter/body)
                (pcase-dolist (`(,key . ,command)
                               '(("s" . nil)
                                 ("k" . nil)))
                  (define-key paradox-menu-filter-prefix-map (kbd key) command))
                (define-key paradox-menu-mode-map (kbd "f") nil)
                (define-key paradox-menu-mode-map (kbd "/") paradox-menu-filter-prefix-map))

              (with-eval-after-load 'which-key
                (which-key-add-keymap-based-replacements package-menu-mode-map
                  "/" "search"
                  "S" "sort")))))

;; Visual alignment
(use-package ialign
  :bind ("<A-tab>" . ialign))

;; Other missing essentials that I don't want to write
(use-package crux
  :quelpa (crux :fetcher github :repo "wyuenho/crux" :branch "fix-rename-file-and-buffer")
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
  (setf crux-shell shell-file-name))

;; Saner undo/redo
(use-package vundo
  :bind (("M-z" . vundo))
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'vundo-post-exit-hook
              (lambda ()
                (when flycheck-mode
                  (flycheck-buffer))))))

;; So I can see past kills that I can yank
(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

;; Sane keyboard scrolling
(use-package pager-default-keybindings)

;; Scroll when jumping medium distances
(use-package scroll-on-jump
  :config
  (scroll-on-jump-advice-add set-mark-command)
  (with-eval-after-load 'diff-hl
    (scroll-on-jump-advice-add diff-hl-next-hunk)))

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
  :init (require 'icomplete)
  :demand
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n"    . icomplete-forward-completions)
              ("<up>"   . icomplete-backward-completions)
              ("C-p"    . icomplete-backward-completions)
              ("C-v"    . icomplete-vertical-toggle)))

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
         (magit-revision-mode . beginend-magit-revision-mode)
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

(use-package pcre2el
  :config
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-c /" "PCRE")))

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
                          ("C-t"     . sp-transpose-sexp)
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
                          ("M-]" . sp-backward-unwrap-sexp))
              :config
              (require 'smartparens-config)

              (add-hook 'eval-expression-minibuffer-setup-hook
                        (lambda ()
                          (smartparens-mode 1))))))

;; Cross-machine fomatting
(use-package editorconfig
  :delight)

;; Performance enhancement for files with really long lines
(use-package so-long)

;; Prettier form feeds
(use-package page-break-lines-mode
  :hook (((prog-mode text-mode) . page-break-lines-mode)))

;; Quick Snippets
(use-package yasnippet
  :delight yas-minor-mode
  :commands yas-minor-mode
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("TAB"   . nil)
              ("<tab>" . nil)
              ("C-c i" . yas-expand-from-trigger-key))
  :config
  (yas-reload-all)
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-c &" "yasnippet")))

(use-package yasnippet-snippets)

;; No sane person will program from right to left, so turn this major perf
;; bottleneck off
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local bidi-paragraph-direction 'left-to-right
                        bidi-inhibit-bpa nil)))

;; Turn on background color for HEX for specific modes
(use-package rainbow-mode
  :delight
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
  (setf string-inflection-skip-backward-when-done t)
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
    (setf minor-mode-map-alist
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
(use-package pdf-tools
  :config
  (defun pdf-view-goto-page-advice (fn &rest args)
    "Ignore `pdf-view-goto-page' error when scrolling."
    (ignore-errors (apply fn args)))
  (advice-add 'pdf-view-goto-page :around 'pdf-view-goto-page-advice)
  (pdf-tools-install t)
  (pdf-loader-install t))

;; Modern tree-based syntax-highlighting
(use-package tree-sitter-langs
  :delight (tree-sitter-mode tree-sitter-hl-mode)
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

;; Static Analysis
(use-package lsp-mode
  :delight (lsp-mode) (lsp-lens-mode)
  :hook (((c-mode-common
           enh-ruby-mode
           go-mode
           groovy-mode
           js-mode
           python-mode
           reason-mode
           rust-mode
           scala-mode
           swift-mode
           tuareg-mode)
          . lsp-deferred)
         (lsp-managed-mode . lsp-mode)
         (lsp-mode . (lambda ()
                       (with-eval-after-load 'which-key
                         (lsp-enable-which-key-integration)))))
  :config
  (setf read-process-output-max (* 1024 1024 10))

  (with-eval-after-load 'flycheck
    (defvar-local lsp-flycheck-checkers nil)

    (defun lsp-flycheck-checker-get-advice (fn checker property)
      "Get checker property from buffer-local variable `lsp-flycheck-checkers'.

Result checker CHECKER property PROPERTY from buffer-local
`lsp-flycheck-checkers', if any. Othewise, from the global
checker symbol."
      (or (alist-get property (alist-get checker lsp-flycheck-checkers))
          (funcall fn checker property)))

    (advice-add 'flycheck-checker-get :around 'lsp-flycheck-checker-get-advice)

    (add-hook 'lsp-managed-mode-hook
              (lambda ()
                (let ((web-mode-checkers
                       (cond ((derived-mode-p 'web-mode)
                              (let ((ext (file-name-extension buffer-file-name)))
                                (cond ((string-equal "css" ext)
                                       '(css-stylelint . ((modes . (web-mode)))))
                                      ((string-equal "less" ext)
                                       '(less-stylelint . ((modes . (web-mode)))))
                                      ((string-equal "scss" ext)
                                       '(scss-stylelint . ((modes . (web-mode)))))
                                      ((>= (string-match-p "htm" ext) 0)
                                       '(html-tidy . ((modes . (web-mode))))))))))
                      (lsp-next-checkers
                       (cond ((derived-mode-p 'css-mode)
                              (cond ((eq major-mode 'scss-mode)
                                     '((warning . scss-stylelint)))
                                    ((eq major-mode 'less-mode)
                                     '((warning . less-stylelint)))
                                    (t '((warning . css-stylelint)))))
                             ((derived-mode-p 'web-mode)
                              (let ((ext (file-name-extension buffer-file-name)))
                                (cond ((string-equal "css" ext)
                                       '((warning . css-stylelint)))
                                      ((string-equal "less" ext)
                                       '((warning . less-stylelint)))
                                      ((string-equal "scss" ext)
                                       '((warning . scss-stylelint)))
                                      ((>= (string-match-p "htm" ext) 0)
                                       '((warning . html-tidy))))))
                             ((derived-mode-p 'js-mode)
                              '((warning . javascript-eslint)))
                             ((derived-mode-p 'typescript-mode)
                              '((warning . javascript-eslint)))
                             ((derived-mode-p 'python-mode)
                              '((warning . python-flake8)
                                (warning . python-mypy)
                                (warning . python-pylint)))
                             ((derived-mode-p 'enh-ruby-mode)
                              '((warning . ruby-rubocop)))
                             ((derived-mode-p 'go-mode)
                              '((warning . golangci-lint))))))
                  (when lsp-next-checkers
                    (push `(lsp . ((next-checkers . ,lsp-next-checkers))) lsp-flycheck-checkers))
                  (when web-mode-checkers
                    (push web-mode-checkers lsp-flycheck-checkers)))))))

(use-package lsp-ui
  :after (lsp-mode)
  :config
  (setf lsp-ui-doc-border (face-foreground 'window-divider))
  (add-hook 'lsp-ui-doc-frame-hook
            (lambda (_ window)
              (let* ((frame (window-frame window)))
                (set-frame-font
                 (font-spec :family "Menlo" :size 11)
                 nil
                 (list frame))))))

(use-package lsp-origami
  :hook (lsp-after-open . lsp-origami-try-enable))

;; LSP debugging support
(use-package dap-mode
  :after (lsp-mode)
  :config
  (setf dap-utils-extension-path (expand-file-name "~/.vscode/extensions"))

  (add-hook 'js-mode-hook
            (lambda ()
              (use-package dap-firefox
                :custom
                (dap-firefox-debug-path
                 (car
                  (last
                   (file-expand-wildcards
                    (concat
                     dap-utils-extension-path
                     "/firefox-devtools.vscode-firefox-debug-*")))))
                (dap-firefox-debug-program
                 (concat "node " dap-firefox-debug-path "/dist/adaptor.bundle.js")))

              ;; https://github.com/emacs-lsp/dap-mode/issues/369
              ;; (use-package dap-node
              ;;   :custom
              ;;   (dap-node-debug-path
              ;;    "/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/ms-vscode.js-debug")
              ;;   (dap-node-debug-program
              ;;    (concat "node " dap-node-debug-path "/src/extension.js")))
              ))

  (add-hook 'python-mode-hook (lambda () (use-package dap-python)))

  (add-hook 'go-mode-hook
            (lambda ()
              (use-package dap-dlv-go)))

  (dolist (mode '(c-mode-common rust-mode))
    (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook mode-hook
                (lambda ()
                  (use-package dap-gdb-lldb
                    :custom
                    (dap-gdb-lldb-path
                     (car
                      (last
                       (file-expand-wildcards
                        (concat
                         dap-utils-extension-path
                         "/webfreak.debug-*")))))
                    (dap-gdb-lldb-debug-program
                     `("node" ,(concat dap-gdb-lldb-path "/out/src/gdb.js"))))

                  ;; (use-package dap-codelldb
                  ;;   :custom
                  ;;   (dap-codelldb-debug-path
                  ;;    (car
                  ;;     (last
                  ;;      (file-expand-wildcards
                  ;;       (concats
                  ;;        dap-utils-extension-path
                  ;;        "/vadimcn.vscode-lldb-*")))))
                  ;;   (dap-codelldb-debug-program
                  ;;    (concat dap-codelldb-debug-path "/adapter/codelldb")))

                  ;; (use-package dap-cpptools
                  ;;   :custom
                  ;;   (dap-cpptools-debug-path
                  ;;    (car
                  ;;     (last
                  ;;      (file-expand-wildcards
                  ;;       (concat
                  ;;        dap-utils-extension-path
                  ;;        "/ms-vscode.cpptools-*")))))
                  ;;   (dap-cpptools-debug-program
                  ;;    (concat dap-cpptools-debug-path "/debugAdapters/bin/OpenDebugAD7")))
                  )))))

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
  (setf company-backends
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

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (add-hook 'company-prescient-mode-hook
            (lambda ()
              ;; Make sure `company-sort-prefer-same-case-prefix' is always at the back
              (delq 'company-sort-prefer-same-case-prefix company-transformers)
              (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix t))))

;; Linting
(defun find-file-from-project-root (file-name)
  (when-let ((dir (locate-dominating-file
                   (or (and (functionp 'projectile-project-root)
                            (projectile-project-root))
                       default-directory)
                   file-name)))
    (expand-file-name (concat (file-name-as-directory dir) file-name))))

(use-package spinner)

(use-package flycheck
  :quelpa (flycheck :fetcher github :repo "wyuenho/flycheck" :branch "my-fixes")
  :delight
  :config
  (defun flycheck-show-help-function (msg)
    (when flycheck-mode
      (if (null msg)
          (flycheck-clear-displayed-errors)
        (pcase-let* ((`(,frame ,x . ,y) (mouse-position))
                     (win (window-at x y frame))
                     (`(,body-left ,body-top ,@_) (window-body-edges win))
                     (col (max 1 (- x body-left (or display-line-numbers-width 0))))
                     (row (- y body-top)))
          (with-current-buffer (window-buffer win)
            (save-excursion
              (goto-char (point-min))
              (forward-line (1- (+ (line-number-at-pos (window-start win)) row)))
              (move-to-column (1- col))
              (when-let (errors (flycheck-overlay-errors-at (point)))
                (flycheck-display-errors errors))))))))

  (add-hook 'flycheck-mode-hook
            (lambda ()
              (when (display-mouse-p)
                (if flycheck-mode
                    (setq-local show-help-function 'flycheck-show-help-function)
                  (kill-local-variable 'show-help-function)))))

  (add-hook 'flycheck-status-changed-functions
            (lambda (status)
              (when (fboundp 'spinner-start)
                (pcase status
                  (`running (spinner-start 'minibox))
                  (- (spinner-stop)))))))

(use-package quick-peek)

(use-package flycheck-inline
  :quelpa (flycheck-inline :fetcher github :repo "wyuenho/flycheck-inline" :branch "flycheck-clear-displayed-errors-function")
  :init
  (defun flycheck-inline-quick-peek (msg pos err)
    (when (or (not company-mode)
              (not (company--active-p)))
      (pcase-let* ((`(,_ ,body-top ,_ ,body-bottom) (window-body-edges))
                   (line-offset (- (line-number-at-pos (point))
                                   (line-number-at-pos (window-start))))
                   (quick-peek-position
                    (if (< (- body-bottom body-top line-offset) 10) 'above 'below))
                   (ov (quick-peek-overlay-ensure-at pos))
                   (contents (quick-peek-overlay-contents ov)))
        (setf (quick-peek-overlay-contents ov)
              (concat contents (when contents "\n") (string-trim msg)))
        (quick-peek-update ov 1 4))))
  :after (flycheck quick-peek company)
  :hook (flycheck-mode . flycheck-inline-mode)
  :custom
  (flycheck-inline-display-function #'flycheck-inline-quick-peek)
  (flycheck-inline-clear-function #'quick-peek-hide))

;; REST API
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; API Blueprints
(use-package apib-mode
  :mode "\\.apib\\'")

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

(setf eshell-directory-name (expand-file-name ".eshell/" user-emacs-directory))

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
         ([remap backward-kill-word]  . vterm--self-insert)
         ([remap sp-kill-hybrid-sexp] . vterm--self-insert))
  :config
  (advice-add 'vterm--sentinel :override 'vterm--sentinel-advice))

(use-package eterm-256color
  :config
  (with-eval-after-load 'term
    (add-hook 'term-mode-hook 'eterm-256color-mode)))

;; Markup and config languages
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)))

(use-package dotenv-mode
  :mode "\\.env\\..*\\'")

;; Emacs Lisp
(add-to-list 'lisp-imenu-generic-expression
             (list nil (concat
                        "^\\s-*("
                        (eval-when-compile
                          (regexp-opt '("iter-defun" "aio-defun") t))
                        "\\s-+\\(" lisp-mode-symbol-regexp "\\)")
                   2))

(defun calculate-lisp-indent-advice (fn &rest args)
  "Don't indent vectors in `emacs-lisp-mode' like lists."
  (if (save-excursion
        (beginning-of-line)
        (let ((start (point)))
          (save-excursion
            (beginning-of-defun)
            (let ((containing-sexp (elt (parse-partial-sexp (point) start) 1)))
              (when containing-sexp
                (setf (point) containing-sexp)
                (looking-at "\\["))))))
      (let ((lisp-indent-offset 1))
        (apply fn args))
    (apply fn args)))
(advice-add 'calculate-lisp-indent :around 'calculate-lisp-indent-advice)

(defun elisp--local-variables-advice (fn &rest args)
  "Prevents errors in `elisp--local-variables' from causing completion functions to throw."
  (ignore-errors (apply fn nil args)))

(advice-add 'elisp--local-variables :around 'elisp--local-variables-advice)

(pcase-dolist (`(,key . ,command)
               '(("C-c e f" . byte-compile-file)
                 ("C-c e c" . emacs-lisp-byte-compile)
                 ("C-c e l" . emacs-lisp-byte-compile-and-load)
                 ("C-c e b" . eval-buffer)
                 ("C-c e r" . eval-region)))
  (define-key emacs-lisp-mode-map (kbd key) command))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c e" "elisp"))

(add-hook 'ielm-mode-hook
          (lambda ()
            (ielm-change-working-buffer (window-buffer (selected-window)))))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e x" . macrostep-expand)))

(use-package elisp-def
  :delight
  :hook (emacs-lisp-mode . elisp-def-mode))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)
         ("C-h p" . helpful-at-point)))

(defun eieio-browse-advice (fn &rest args)
  "Put EIEIO object browser buffer in special mode."
  (apply fn args)
  (with-current-buffer (get-buffer "*EIEIO OBJECT BROWSE*")
    (special-mode)
    (select-window (get-buffer-window (current-buffer)))))
(advice-add 'eieio-browse :around 'eieio-browse-advice)

(use-package package-lint
  :quelpa (package-lint :fetcher github
                        :repo "wyuenho/package-lint"
                        :branch "fix-installable-package-detection"
                        :files (:defaults "data" (:exclude "*flymake.el"))))

(use-package flycheck-package
  :after (flycheck package-lint)
  :config
  (flycheck-package-setup)
  (plist-put
   (symbol-plist 'emacs-lisp-checkdoc)
   'flycheck-predicate
   #'package-lint-looks-like-a-package-p))

(use-package flycheck-cask
  :after (flycheck)
  :hook (flycheck-mode . flycheck-cask-setup))

;; C/C++/Objective-C
(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

;; Formatting
(use-package reformatter
  :quelpa (reformatter :fetcher github :repo "wyuenho/reformatter.el" :branch "post-processor")
  :config
  (reformatter-define goimports-format
    :program "goimports")

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
            ".json,.js,.jsx,.mjs,.cjs,.ts,.tsx")
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
            ".json,.js,.jsx,.mjs,.cjs,.ts,.tsx")))

(dolist (mode '(css-mode js-mode markdown-mode scss-mode typescript-mode web-mode yaml-mode))
  (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook mode-hook
              (lambda ()
                (let ((formatter
                       (when-let* ((package-json-file
                                    (find-file-from-project-root "package.json"))
                                   (package-json
                                    (and package-json-file
                                         (json-read-file package-json-file)))
                                   (devDependencies
                                    (and package-json
                                         (alist-get 'devDependencies package-json)))
                                   (formatter-styles
                                    '((prettier prettier-eslint prettier)
                                      (eslint eslint-plugin-prettier eslint))))
                         (car (seq-filter 'identity
                                          (map-apply
                                           (lambda (command packages)
                                             (and (seq-some
                                                   (lambda (package)
                                                     (map-contains-key devDependencies package))
                                                   packages)
                                                  command))
                                           formatter-styles)))))
                      (yarn-pnp-p (find-file-from-project-root ".pnp.js")))
                  (unless (and yarn-pnp-p (functionp 'add-node-modules-path))
                    (add-node-modules-path))
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
                          (define-key (symbol-value (derived-mode-map-name mode)) (kbd "C-c f") 'yarn-eslint-format-buffer)
                          (yarn-eslint-format-on-save-mode))
                      (define-key (symbol-value (derived-mode-map-name mode)) (kbd "C-c f") 'eslint-format-buffer)
                      (eslint-format-on-save-mode)))
                   ((eq formatter 'prettier)
                    (use-package prettier
                      :delight
                      :config
                      (prettier-mode)
                      (define-key (symbol-value (derived-mode-map-name mode)) (kbd "C-c f") 'prettier-prettify)))))))))

;; Java
(use-package lsp-java
  :after (lsp)
  :custom
  (lsp-java-server-install-dir (car (file-expand-wildcards "~/.vscode/extensions/redhat.java-*/server"))))

;; Javascript
(add-hook 'js-mode-hook
          (lambda ()
            (pcase-dolist (`(,key . ,command)
                           '(("M-."     . nil)
                             ("C-c M-:" . nil)
                             ("C-c C-j" . nil)
                             ("C-M-x"   . nil)
                             ("<menu-bar>" . nil)))
              (define-key js-mode-map (kbd key) command))))

(use-package import-js
  :after (js)
  :bind (:map js-mode-map
              ("C-c t i"   . import-js-import)
              ("C-c t f"   . import-js-fix)
              ("C-c t M-." . import-js-goto))
  :config (run-import-js))

(use-package js-doc
  :after (js)
  :bind (:map js-mode-map
              ("C-c C-d m" . js-doc-insert-file-doc)
              ("C-c C-d f" . js-doc-insert-function-doc-snippet)))

(use-package nodejs-repl
  :after (js)
  :bind(:map js-mode-map
             ("C-c e e" . nodejs-repl-send-last-expression)
             ("C-c e r" . nodejs-repl-send-region)
             ("C-c e b" . nodejs-repl-send-buffer)
             ("C-c e l" . nodejs-repl-load-file)
             ("C-c M-:" . nodejs-repl-switch-to-repl)))

(use-package js2-mode
  :interpreter ("node" . js2-mode)
  :config
  (when (fboundp 'sp-kill-whole-line)
    (define-key js2-mode-map (kbd "C-k") 'sp-kill-whole-line)))

(use-package rjsx-mode
  :mode "\\.\\(?:cjs\\|jsx?\\|mjs\\)\\'")

(use-package jsonian
  :after so-long
  :mode ("\\.jsonc?\\'" . jsonian-c-mode)
  :config
  (jsonian-no-so-long-mode)
  (with-eval-after-load 'flycheck
    (jsonian-enable-flycheck)))

(use-package jq-format
  :delight jq-format-json-on-save-mode
  :config
  (add-hook 'jsonian-mode-hook
            (lambda ()
              (define-key jsonian-mode-map (kbd "C-c f") 'jq-format-json-buffer)
              (jq-format-json-on-save-mode 1))))

;; TypeScript
(use-package typescript-mode
  :config
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX"
    "Major mode for editing TSX files.

Refer to Typescript documentation for syntactic differences between normal and TSX
variants of Typescript.")

  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))

  (with-eval-after-load 'smartparens
    (add-to-list 'sp--html-modes 'typescript-tsx-mode)))

(use-package ts-comint
  :after (typescript-mode)
  :bind (:map typescript-mode-map
              ("C-x C-e" . ts-send-last-sexp)
              ("C-c e e" . ts-send-last-sexp-and-go)
              ("C-c e r" . ts-send-region-and-go)
              ("C-c e b" . ts-send-buffer-and-go)
              ("C-c e l" . ts-load-file-and-go)
              ("C-c M-:" . switch-to-ts)))

(use-package tide
  :delight tide-mode
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode     . tide-setup)
         (typescript-tsx-mode . tide-setup))
  :config
  (setq-local help-at-pt-display-when-idle '(tide-documentation-at-point)))

;; Python
(add-to-list 'auto-mode-alist '("\\.pythonrc\\'"   . python-mode))
(add-to-list 'auto-mode-alist '("\\.pylintrc\\'"   . conf-mode))
(add-to-list 'auto-mode-alist '("\\.flake8\\'"     . conf-mode))
(add-to-list 'auto-mode-alist '("/poetry.lock\\'"  . conf-toml-mode))
(add-to-list 'auto-mode-alist '("/Pipfile.lock\\'" . conf-toml-mode))

(use-package highlight-indent-guides
  :delight
  :hook (python-mode . highlight-indent-guides-mode))

(use-package python-docstring
  :delight
  :hook (python-mode . python-docstring-mode))

(use-package python-insert-docstring
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key
               (kbd "C-c I")
               'python-insert-docstring-with-google-style-at-point))))

(use-package sphinx-doc
  :delight
  :hook (python-mode . sphinx-doc-mode))

(use-package python-black
  :delight python-black-on-save-mode
  ;; :quelpa (python-black :fetcher github :repo "wyuenho/emacs-python-black" :branch "blackd")
  )

(use-package python-isort
  :delight python-isort-on-save-mode)

(use-package python-pytest)

(defun python-get-black-d-request-headers ()
  (let-alist (python-pyproject)
    `(,@(if .tool.black.line-length
            `(,(cons "X-Line-Length" (format "%s" .tool.black.line-length))))
      ,@(if (and .tool.black.skip-string-normalization
                 (not (eq .tool.black.skip.string-normalization :false)))
            `(,(cons "X-Skip-String-Normalization" "1")))
      ,@(if (or .tool.black.fast .tool.black.safe)
            `(,(cons "X-Fast-Or-Safe" (if .tool.black.fast "fast" "safe"))))
      ,@(if (or .tool.black.pyi .tool.black.target-version)
            `(,(cons "X-Python-Variant"
                     (if .tool.black.pyi
                         "pyi"
                       (funcall 'string-join
                                (append .tool.black.target-version) ","))))))))

(use-package lsp-jedi
  :after (lsp-mode))

(use-package lsp-pyright
  :after (lsp-mode pet)
  :config
  (pet-def-config-accessor pyrightconfig
                           :file-name "\\`pyrightconfig.json\\'"
                           :parser pet-parse-config-file)
  (let ((client (gethash 'pyright lsp-clients)))
    (setf (lsp--client-major-modes client) nil)
    (setf (lsp--client-activation-fn client)
          (lambda (file-name mode)
            (and (or (string-match-p "py[iw]?" (or (file-name-extension file-name) ""))
                     (eq mode 'python-mode))
                 (or (pet-pyrightconfig)
                     (let-alist (pet-pyproject)
                       .tool.pyright)))))))

(use-package pet
  :delight
  :config (add-hook 'python-mode-hook 'pet-mode -10))

;; Ruby
(use-package yard-mode)
(use-package enh-ruby-mode
  :mode (rx (or ".rb"
                ".rbw"
                ".ru"
                ".rake"
                ".thor"
                ".jbuilder"
                ".rabl"
                ".gemspec"
                ".podspec"
                ".irbrc"
                "/Gemfile"
                "/Rakefile"
                "/Capfile"
                "/Thorfile"
                "/Puppetfile"
                "/Berksfile"
                "/Vagrantfile"
                "/Guardfile"
                "/Prodfile")
            string-end)
  :interpreter ("ruby1.8" "ruby1.9" "jruby" "rbx" "ruby")
  :config (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
  :hook yard-mode)

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (defun lsp-go-format-buffer ()
    (condition-case err
        (lsp-format-buffer)
      (error (minibuffer-message (error-message-string err)))))
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'goimports-format-buffer -100 t)
              (add-hook 'before-save-hook 'gofmt-before-save nil t)
              (with-eval-after-load 'lsp-mode
                (add-hook 'lsp-managed-mode-hook
                          (lambda ()
                            (if lsp-managed-mode
                                (progn
                                  (setq-local lsp-enable-indentation t
                                              lsp-enable-on-type-formatting t)
                                  (remove-hook 'before-save-hook 'gofmt-before-save t)
                                  (add-hook 'before-save-hook 'lsp-go-format-buffer nil t))
                              (kill-local-variable 'lsp-enable-indentation)
                              (kill-local-variable 'lsp-enable-on-type-formatting)
                              (remove-hook 'before-save-hook 'lsp-go-format-buffer t)
                              (add-hook 'before-save-hook 'gofmt-before-save nil t)))
                          nil t)))))

(use-package flycheck-golangci-lint
  :after (go-mode)
  :config (flycheck-golangci-lint-setup))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'")

(use-package cargo
  :delight cargo-minor-mode
  :config
  (cargo-minor-mode))

(use-package ron-mode
  :mode "\\.ron'")

;; Swift
(use-package swift-mode
  :mode "\\.swift\\'")

(use-package lsp-sourcekit
  :after (lsp-mode))

;; Scala
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package lsp-metals
  :after (lsp-mode))

;; Web
(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package web-mode
  :functions web-mode-language-at-pos
  :mode ((rx (or ".vue"
                 ".eex"
                 ".svelte"
                 ".handlebars"
                 ".underscore"
                 ".mustache"
                 ".hbs"
                 ".htm"
                 ".html"
                 ".jinja"
                 ".jinja2"
                 ".j2"
                 ".mako"
                 ".dtl"
                 ".jsp"
                 ".erb"))))

(use-package emmet-mode
  :delight
  :after (:any web-mode
               js-mode
               js2-mode
               rjsx-mode
               typescript-tsx-mode)
  :hook (sgml-mode
         nxml-mode
         web-mode
         js-jsx-mode
         js2-jsx-mode
         rjsx-mode
         typescript-tsx-mode)
  :config
  (define-key emmet-mode-keymap (kbd "C-c C-c w") nil)
  (define-key emmet-mode-keymap (kbd "C-c C-m w") 'emmet-wrap-with-markup)
  (add-hook 'emmet-mode-hook
            (lambda ()
              (when (member major-mode
                            '(js-jsx-mode
                              js2-jsx-mode
                              rjsx-mode
                              typesript-tsx-mode))
                (setq-local emmet-expand-jsx-className? t)))))

;; Project management
(use-package projectile
  :delight projectile-mode
  :config
  (projectile-register-project-type 'go
                                    '("go.mod")
                                    :project-file "go.mod"
                                    :compile "go build"
                                    :test "go test ./..."
                                    :test-suffix "_test")
  (global-set-key (kbd "C-x p") projectile-command-map)
  (projectile-mode 1)
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-x p" "projectile")
    (which-key-add-keymap-based-replacements projectile-command-map
      "4" "projectile-other-window"
      "5" "projectile-other-frame"
      "x" "projectile-run"
      "s" "projectile-search")))

(use-package projectile-rails
  :after projectile
  :delight
  :config
  (projectile-rails-global-mode)
  (add-hook 'projectile-rails-mode-hook
            (lambda ()
              (setf projectile-rails-mode-map (make-sparse-keymap))
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
              (setf ediff-saved-window-configuration (current-window-configuration))))

  (let ((restore-window-configuration
         (lambda ()
           (set-window-configuration ediff-saved-window-configuration))))
    (add-hook 'ediff-quit-hook restore-window-configuration 'append)
    (add-hook 'ediff-suspend-hook restore-window-configuration 'append))

  (add-hook 'ediff-cleanup-hook
            (lambda ()
              (eval-and-compile (require 'ediff-util))
              (ediff-janitor nil nil))
            'append))

(use-package diff-hl
  :config
  (if (display-graphic-p)
      (diff-hl-flydiff-mode 1)
    (diff-hl-margin-mode 1))
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook (lambda () (diff-hl-dired-mode 1)))))

(use-package magit
  :config
  (with-eval-after-load 'git-rebase
    ;; Vanilla undo has been completely unbound, this reenable undo in
    ;; `git-rebase-mode'
    (define-key git-rebase-mode-map (kbd "M-z") 'git-rebase-undo)))

;; Refresh VC related states reliably across packages.
(when file-notify--library
  (defvar watched-git-heads nil
    "A list of .git/logs/HEAD file to watcher mappings.")

  (defun refresh-git-backed-buffer ()
    "Refresh the current buffer base on the current git state."
    (if (and (boundp 'magit-mode) (derived-mode-p 'magit-mode))
        (magit-refresh-buffer)
      (unless revert-buffer-in-progress-p
        (cond
         ((or (bound-and-true-p diff-hl-flydiff-mode)
              (bound-and-true-p diff-hl-mode))
          (diff-hl-update))
         ((bound-and-true-p diff-hl-dired-mode)
          (diff-hl-dired-update))
         ((bound-and-true-p vc-dir-mode)
          (vc-dir-refresh)
          (when (bound-and-true-p diff-hl-dir-mode)
            (diff-hl-dir-update)))
         ((vc-backend (buffer-file-name))
          (vc-refresh-state))))))

  (defun handle-git-state-change (event)
    "Callback to `file-notify-add-watch' to handle git state change.

EVENT is a file system notification event.  If EVENT is a change
event, find all the visible buffers belonging to the same git
repo and refresh all of the version control minor mode states.
If EVENT is a deleted or renamed event, remove the file system
notification watcher.

See `file-notify-add-watch' for more details."
    (pcase-let ((`(,desc ,action ,file ,@_) event))
      (pcase action
        ((or 'deleted 'renamed)
         (file-notify-rm-watch desc))
        ('changed
         (let ((vc-root (apply 'file-name-concat "/" (seq-subseq (split-string file "/") 0 -3))))
           (dolist (frame (frame-list))
             (when (not (frame-parent frame))
               (dolist (buf (buffer-list frame))
                 (when (not (string-prefix-p " " (buffer-name buf)))
                   (with-current-buffer buf
                     (when (string-prefix-p vc-root (expand-file-name default-directory))
                       (refresh-git-backed-buffer))))))))))))

  (defun watch-git-head ()
    (let ((file (buffer-file-name)))
      (when (eq 'Git (vc-backend file))
        (let* ((root (expand-file-name (vc-git-root file)))
               (head (file-name-concat root ".git" "logs" "HEAD")))
          (unless (assoc-default head watched-git-heads)
            (ignore-errors
              (setf (alist-get head watched-git-heads nil nil 'equal)
                    (file-notify-add-watch head '(change) 'handle-git-state-change))))))))

  (defun reap-git-head-watchers ()
    (when-let* ((buffer-default-dirs-sans-current
                 (seq-uniq
                  (mapcar
                   (lambda (buf) (with-current-buffer buf (expand-file-name default-directory)))
                   (seq-filter
                    (lambda (buf)
                      (not (or (string-prefix-p " " (buffer-name buf))
                               (equal buf (current-buffer)))))
                    (apply 'append
                           (mapcar
                            'buffer-list
                            (seq-filter
                             (lambda (frame) (not (frame-parent frame)))
                             (frame-list))))))))
                (git-heads-to-delete
                 (seq-filter
                  (lambda (git-head)
                    (let ((vc-root (apply 'file-name-concat "/" (seq-subseq (split-string git-head "/") 0 -3))))
                      (not (seq-some (apply-partially 'string-prefix-p vc-root) buffer-default-dirs-sans-current))))
                  (mapcar 'car watched-git-heads))))

      (dolist (git-head git-heads-to-delete)
        (let ((watcher (assoc-default git-head watched-git-heads)))
          (file-notify-rm-watch watcher)
          (setf (alist-get git-head watched-git-heads nil t 'equal) nil)))))

  (add-hook 'find-file-hook 'watch-git-head)
  (add-hook 'kill-buffer-hook 'reap-git-head-watchers))

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
  :after (magit)
  :config (magit-todos-mode 1))

(use-package magit-lfs
  :after (magit))

(use-package git-timemachine
  :config
  (defun git-timemachine-quit-advice ()
    "Quit the `git-timemachine' window and kill the buffer."
    (interactive) (quit-window t))
  (advice-add 'git-timemachine-quit :override 'git-timemachine-quit-advice))

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
      (setf buffer-invisibility-spec (list t)))
  (setf buffer-invisibility-spec
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
                (defun dired-ns-open-externally ()
                  "Open file using default system apps on macOS."
                  (interactive)
                  (let ((file-name (dired-get-file-for-visit)))
                    (start-process "default-app" nil "open" file-name)))
                (define-key dired-mode-map (kbd "z") 'dired-ns-open-externally)))))

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
    (setf dired-single-magic-buffer-name purpose-x-code1-dired-buffer-name))
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
        (setf truncate-lines tl
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
                   scroll-bar-foreground scroll-bar-background
                   ;; make sure frame restoration is alway maximized
                   left top height width fullscreen
                   GUI:font GUI:height GUI:width GUI:fullscreen))
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
                    (buffer-params (cdr buffer))
                    (same-buffer-p (lambda (buffer)
                                     (string= buffer-name (car buffer))))
                    (live-buffer-p (lambda (buffer)
                                     (buffer-live-p (get-buffer (car buffer)))))
                    (next-buffers (alist-get 'next-buffers tail))
                    (prev-buffers (alist-get 'prev-buffers tail)))
               (if (get-buffer buffer-name)
                   window-tree
                 (when (> (length next-buffers) 1)
                   (setf (alist-get 'next-buffers window-tree)
                         (seq-filter live-buffer-p (seq-remove same-buffer-p next-buffers))))
                 (when (> (length prev-buffers) 1)
                   (setf prev-buffers
                         (seq-filter live-buffer-p (seq-remove same-buffer-p prev-buffers))
                         (alist-get 'prev-buffers window-tree) prev-buffers)
                   (when-let* ((replacement-buffer (car prev-buffers))
                               (replacement-buffer-name (car replacement-buffer))
                               (start (nth 1 replacement-buffer))
                               (point (nth 2 replacement-buffer)))
                     (setf (alist-get 'point buffer-params) point
                           (alist-get 'start buffer-params) start
                           (alist-get 'buffer tail)
                           `(,replacement-buffer-name ,@buffer-params))))
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
  :bind (("C-c b" . buf-move)))

(use-package window-purpose
  :quelpa (window-purpose :fetcher github :repo "wyuenho/emacs-purpose" :files (:defaults "layouts")
                          :branch "improve-code1")
  :config
  (define-key purpose-mode-map (kbd "C-c ,") nil)
  (define-key purpose-mode-map (kbd "C-c w") purpose-mode-prefix-map)

  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-c w" "window-purpose")
    (which-key-add-keymap-based-replacements purpose-mode-prefix-map
      "4" "purpose-other-window"
      "5" "purpose-other-frame"))

  (purpose-add-user-purposes
   :modes '((message-mode . edit)
            (ag-mode      . search)
            (rg-mode      . search)))

  (with-eval-after-load 'message
    (defun message-send-and-exit-advice (fn &rest args)
      "Quit the window after killing the message buffer after sending."
      (apply fn args)
      (quit-window))
    (advice-add 'message-send-and-exit :around 'message-send-and-exit-advice)
    (defun message-kill-buffer-advice (fn &rest args)
      "Quit the window after killing the message buffer."
      (apply fn args)
      (quit-window))
    (advice-add 'message-kill-buffer :around 'message-kill-buffer-advice))

  (with-eval-after-load 'window-purpose-x
    (add-to-list 'purpose-x-popwin-buffer-names "*Messages*")
    (add-to-list 'purpose-x-popwin-buffer-names "*Warnings*")
    (purpose-x-popwin-update-conf)

    (with-eval-after-load 'pet
      (add-to-list 'purpose-x-popwin-buffer-names "*pet info*")
      (purpose-x-popwin-update-conf))

    (with-eval-after-load 'browse-kill-ring
      (add-to-list 'purpose-x-popwin-major-modes 'browse-kill-ring-mode)
      (purpose-x-popwin-update-conf))

    (with-eval-after-load 'vterm
      (add-to-list 'purpose-x-popwin-major-modes 'vterm-mode)
      (add-to-list 'purpose-x-popwin-buffer-names vterm-buffer-name)
      (purpose-x-popwin-update-conf))

    (with-eval-after-load 'ido
      (add-to-list 'purpose-x-popwin-buffer-names ido-completion-buffer)
      (add-to-list 'purpose-x-popwin-buffer-names " *IDO Trace*")
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

  (add-hook 'window-setup-hook
            (lambda ()
              (let ((win (selected-window)))
                (purpose-mode 1)
                (select-window win))))

  ;; (with-eval-after-load 'debug
  ;;   (defun purpose--debug (fn &rest args)
  ;;     "Ignore `pop-to-buffer' display actions given by `debug'."
  ;;     (advice-remove 'purpose-pop-to-buffer-advice 'pop-to-buffer)
  ;;     (unwind-protect
  ;;         (progn
  ;;           (let* ((pop-to-buffer-definition (symbol-function 'pop-to-buffer))
  ;;                  (purpose-debug-pop-to-buffer
  ;;                   (lambda (buffer &optional _action record)
  ;;                     (if purpose--active-p
  ;;                         (purpose-pop-buffer buffer record)
  ;;                       (funcall pop-to-buffer-definition buffer nil record)))))
  ;;             ;; (cl-letf (((symbol-function 'pop-to-buffer) purpose-debug-pop-to-buffer))
  ;;             ;;   (apply fn args))
  ;;             (cl-flet ((pop-to-buffer
  ;;                         (buffer &optional _action record)
  ;;                         (funcall purpose-debug-pop-to-buffer _action record)))
  ;;               (apply fn args))
  ;;             ))
  ;;       (advice-add 'purpose-pop-to-buffer-advice :around 'pop-to-buffer)))

  ;;   (advice-add 'debug :around 'purpose--debug))

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
