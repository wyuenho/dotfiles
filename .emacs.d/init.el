;; -*- lexical-binding: t -*-
(toggle-debug-on-error)

(require 'cl-lib)
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
under `user-emacs-directory'.  If it exists, load it."
  (interactive)
  (when-let ((custom-file-path
              (locate-file "custom.el" (list user-emacs-directory))))
    (setf custom-file custom-file-path)
    (load custom-file)))
(load-custom-file)

;; Install tree-sitter language grammars
(when (treesit-available-p)
  (setq treesit-language-source-alist
        `((cmake "https://github.com/uyha/tree-sitter-cmake")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
          ,@(mapcar
             (lambda (lang) (list lang (format "https://github.com/tree-sitter/tree-sitter-%s" lang)))
             '(bash c c-sharp cpp css go java javascript json python ruby rust))))

  (dolist (lang (mapcar 'car treesit-language-source-alist))
    (let* ((lib-url (car (alist-get lang treesit-language-source-alist)))
           (lib-name (concat
                      "lib"
                      (substring lib-url (1+ (string-match-p "/[^/]*$" lib-url)))
                      module-file-suffix))
           (lib-path (expand-file-name
                      (file-name-concat
                       user-emacs-directory "tree-sitter" lib-name))))
      (when (or (not (treesit-language-available-p lang))
                (not (file-exists-p lib-path))
                (let ((mod-time (float-time
                                 (file-attribute-modification-time
                                  (file-attributes
                                   lib-path)))))
                  (> (- (float-time) mod-time) (* 60 60 24 90))))
        (let ((noninteractive t))
          (cl-flet ((y-or-n-p (prompt) t))
            (treesit-install-language-grammar lang)))))))

;; Install selected but missing packages
(let ((missing (cl-set-difference
                package-selected-packages
                package-activated-list))
      (debug-on-error nil))
  (when missing
    (with-demoted-errors "%s"
      (package-refresh-contents t))
    (let ((noninteractive t))
      (dolist (package missing)
        (with-demoted-errors "%s"
          (package-install package t)
          (package-activate package))))
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
             (gcmh-mode              nil gcmh)
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

;; Replace the major mode name with its icon
(use-package all-the-icons
  :quelpa (all-the-icons :fetcher github :repo "domtronn/all-the-icons.el" :branch "svg" :files (:defaults "svg"))
  :if (display-graphic-p)
  :config
  (with-eval-after-load 'powerline
    (defun powerline-major-mode-advice (major-mode-segment)
      (let* ((props (text-properties-at 0 major-mode-segment))
             (icon (all-the-icons-icon-for-mode major-mode))
             (face-prop (and (stringp icon) (get-text-property 0 'face icon))))
        (if face-prop
            (apply 'propertize icon 'face face-prop props)
          major-mode-segment)))
    (advice-add 'powerline-major-mode :filter-return 'powerline-major-mode-advice))

  (defvar vscode-kind-icons
    `((text           . ,(all-the-icons-vscode-codicons "symbol-key" :padding '(2 . 1)))
      (method         . ,(all-the-icons-vscode-codicons "symbol-method" :face 'all-the-icons-purple :padding '(2 . 1)))
      (function       . ,(all-the-icons-vscode-codicons "symbol-method" :face 'all-the-icons-purple :padding '(2 . 1)))
      (constructor    . ,(all-the-icons-vscode-codicons "symbol-method" :face 'all-the-icons-purple :padding '(2 . 1)))
      (field          . ,(all-the-icons-vscode-codicons "symbol-field" :face 'all-the-icons-blue :padding '(2 . 1)))
      (variable       . ,(all-the-icons-vscode-codicons "symbol-variable" :face 'all-the-icons-blue :padding '(2 . 1)))
      (class          . ,(all-the-icons-vscode-codicons "symbol-class" :face 'all-the-icons-yellow :padding '(2 . 1)))
      (interface      . ,(all-the-icons-vscode-codicons "symbol-interface" :face 'all-the-icons-blue :padding '(2 . 1)))
      (module         . ,(all-the-icons-vscode-codicons "symbol-namespace" :padding '(2 . 1)))
      (property       . ,(all-the-icons-vscode-codicons "symbol-property" :padding '(2 . 1)))
      (unit           . ,(all-the-icons-vscode-codicons "symbol-ruler" :padding '(2 . 1)))
      (value          . ,(all-the-icons-vscode-codicons "symbol-enum-member" :face 'all-the-icons-yellow :padding '(2 . 1)))
      (enum           . ,(all-the-icons-vscode-codicons "symbol-enum" :face 'all-the-icons-yellow :padding '(2 . 1)))
      (keyword        . ,(all-the-icons-vscode-codicons "symbol-keyword" :padding '(2 . 1)))
      (snippet        . ,(all-the-icons-vscode-codicons "symbol-snippet" :padding '(2 . 1)))
      (color          . ,(all-the-icons-vscode-codicons "symbol-color" :padding '(2 . 1)))
      (file           . ,(all-the-icons-vscode-codicons "symbol-file" :padding '(2 . 1)))
      (reference      . ,(all-the-icons-vscode-codicons "references" :padding '(2 . 1)))
      (folder         . ,(all-the-icons-vscode-codicons "folder" :padding '(2 . 1)))
      (enum-member    . ,(all-the-icons-vscode-codicons "symbol-enum-member" :face 'all-the-icons-yellow :padding '(2 . 1)))
      (constant       . ,(all-the-icons-vscode-codicons "symbol-constant" :padding '(2 . 1)))
      (struct         . ,(all-the-icons-vscode-codicons "symbol-structure" :padding '(2 . 1)))
      (event          . ,(all-the-icons-vscode-codicons "symbol-event" :face 'all-the-icons-yellow :padding '(2 . 1)))
      (operator       . ,(all-the-icons-vscode-codicons "symbol-operator" :padding '(2 . 1)))
      (type-parameter . ,(all-the-icons-vscode-codicons "symbol-parameter" :padding '(2 . 1))))))

;; Theme
(defun custom-save-all-advice (fn &rest args)
  "Skip saving faces."
  (cl-letf (((symbol-function 'custom-save-faces) (symbol-function 'ignore)))
    (apply fn args)))
(advice-add 'custom-save-all :around 'custom-save-all-advice)

(use-package modus-themes
  :config
  (add-hook 'modus-themes-after-load-theme-hook
            (lambda ()
              (modus-themes-with-colors
                (custom-set-faces
                 `(bold ((,c :weight semibold)))
                 `(corfu-annotations ((((type ns))
                                       :family "SF Pro Text"
                                       :height 0.9
                                       :foreground ,bg-hl-line)))
                 `(corfu-current ((,c :distant-foreground ,docstring)))
                 `(corfu-popupinfo ((((type ns)) :family "SF Pro Text")))
                 `(lsp-headerline-breadcrumb-separator-face ((t :inherit shadow)))
                 `(lsp-ui-doc-background ((t :background ,bg-dim)))
                 `(quick-peek-background-face ((,c :background ,bg-main)))
                 `(quick-peek-border-face ((,c :strike-through t
                                               :foreground ,fg-main
                                               :background ,bg-main)))
                 `(quick-peek-padding-face ((,c :foreground ,border
                                                :background ,bg-main)))

                 `(line-number ((,c :inherit ,(if modus-themes-mixed-fonts
                                                  '(fixed-pitch default)
                                                'default)
                                    :background ,bg-line-number-inactive
                                    :foreground ,fg-line-number-inactive)))
                 `(line-number-current-line ((,c :inherit line-number
                                                 :background ,bg-line-number-active
                                                 :foreground ,fg-line-number-active)))
                 `(mode-line ((,c :inherit modus-themes-ui-variable-pitch
                                  :box unspecified
                                  :overline ,border-mode-line-active
                                  :background ,bg-mode-line-active
                                  :foreground ,fg-mode-line-active)))
                 `(mode-line-highlight ((,c :background ,bg-hover
                                            :foreground ,fg-main
                                            :box unspecified
                                            :overline ,border-mode-line-active)))
                 `(mode-line-inactive ((,c :inherit modus-themes-ui-variable-pitch
                                           :box unspecified
                                           :overline ,border-mode-line-inactive
                                           :background ,bg-mode-line-inactive
                                           :foreground ,fg-mode-line-inactive)))
                 `(tooltip ((,c :background ,bg-active
                                :foreground ,fg-main
                                :inherit modus-themes-ui-variable-pitch)))))))

  (modus-themes-load-theme 'modus-vivendi-tinted))

;; Fancy mode line
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-toggle-buffer-encoding-abbrev-off))

;; Sets $MANPATH, $PATH and exec-path from your shell, but only on OS X. This
;; should be done ASAP on init.
(use-package exec-path-from-shell
  :if (memq (window-system) '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package inheritenv)

(use-package envrc
  :config
  (add-hook 'change-major-mode-after-body-hook 'envrc-mode))

(setf backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Automatically wrap overly long lines for all text modes
(add-hook 'text-mode-hook (lambda ()
                            (unless (member major-mode '(yaml-mode yaml-ts-mode markdown-mode))
                              (auto-fill-mode))))

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
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook go-dot-mod-mode-hook))
  (add-hook hook (lambda ()
                   (subword-mode)
                   (if (fboundp 'display-line-numbers-mode)
                       (display-line-numbers-mode)
                     (linum-mode)
                     ;; Renumber the current buffer after reverting the buffer
                     (add-hook 'after-revert-hook 'linum-update-current)))))

;; No sane person will program from right to left, so turn this major perf
;; bottleneck off
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local bidi-paragraph-direction 'left-to-right
                        bidi-inhibit-bpa nil)))

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
;; Unbind useless help keys
(keymap-unset help-map "RET" t)
(keymap-unset help-map "F" t)
(keymap-unset help-map "K" t)
(keymap-unset help-map "g" t)
(keymap-set help-map "F" 'describe-face)

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

;; So I can see past kills that I can gyank
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

(use-package vertico
  :config
  (vertico-mode))

(use-package vertico-directory
  :after (vertico)
  :config
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (with-eval-after-load rfn-eshadow-overlay
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

(use-package vertico-mouse
  :after (vertico)
  :config (vertico-mouse-mode))

(use-package vertico-prescient
  :after (vertico)
  :config (vertico-prescient-mode))

(use-package marginalia
  :after (vertico)
  :config
  (marginalia-mode))

(use-package embark
  :after (vertico)
  :config
  (keymap-set vertico-map "C-," #'embark-dwim)
  (keymap-set vertico-map "C-." #'embark-act))

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
         ("C-M-s" . vr/isearch-forward)
         ("C-M-r" . vr/isearch-backward)
         ("M-s m" . vr/mc-mark)))

;; More convenient region selection
(use-package expreg
  :bind (("M-=" . expreg-expand)
         ("M--" . expreg-contract)))

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
              (add-hook 'eval-expression-minibuffer-setup-hook 'smartparens-mode))))

;; Guide bars for indentation-based languages
(use-package indent-bars
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))

;; Cross-machine fomatting
(use-package editorconfig
  :delight)

;; Performance enhancement for files with really long lines
(use-package so-long)

;; Prettier form feeds
(use-package page-break-lines
  :hook ((prog-mode
          text-mode
          compilation-mode
          special-mode
          comint-mode)
         . page-break-lines-mode))

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

;; Turn on background color for HEX for specific modes
(use-package rainbow-mode
  :delight
  :hook (emacs-lisp-mode js-base-mode sh-base-mode typescript-ts-base-mode web-mode))

;; Cycle through most common programming identifier styles
(use-package string-inflection
  :init
  (defun inflect-string ()
    (interactive)
    (cond ((derived-mode-p 'scala-mode 'java-mode 'java-ts-mode 'js-base-mode 'js-jsx-mode 'typescript-ts-base-mode 'go-mode 'go-ts-mode)
           (string-inflection-java-style-cycle))
          ((derived-mode-p 'python-base-mode 'ruby-base-mode 'enh-ruby-mode 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode 'rust-mode 'rust-ts-mode)
           (string-inflection-python-style-cycle))
          ((derived-mode-p 'prog-mode)
           (string-inflection-all-cycle))))
  :config
  (setf string-inflection-skip-backward-when-done t)
  :bind (("C-x C-y" . inflect-string)))

;; Insert semicolons smartly
(use-package smart-semicolon
  :delight
  :hook ((c-mode-common
          css-base-mode
          java-mode
          java-ts-mode
          js-base-mode
          rust-mode
          rust-ts-mode
          scala-mode
          typescript-ts-base-mode)
         . smart-semicolon-mode))

;; Cycle between quotes
(use-package cycle-quotes
  :bind (("C-x C-'" . cycle-quotes)))

;; Increment and decrement
(use-package shift-number
  :bind (("C-x =" . shift-number-up)
         ("C-x -" . shift-number-down)))

;; Modern code foldinq
(use-package treesit-fold
  :quelpa (treesit-fold :fetcher github :repo "emacs-tree-sitter/treesit-fold")
  :delight
  :config
  (keymap-set treesit-fold-mode-map "M-0" 'treesit-fold-open-all)
  (keymap-set treesit-fold-mode-map "M-9" 'treesit-fold-close-all)
  (keymap-set treesit-fold-mode-map "C-M-/" 'treesit-fold-toggle))

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
  (pdf-loader-install t))

;; Static Analysis
;; (use-package eglot
;;   :hook (((c-mode-common
;;            c-ts-base-mode
;;            cmake-ts-mode
;;            enh-ruby-mode
;;            go-mode
;;            go-ts-mode
;;            groovy-mode
;;            python-base-mode
;;            rust-mode
;;            rust-ts-mode
;;            scala-mode
;;            swift-mode
;;            tuareg-mode)
;;           . eglot-ensure))
;;   :config
;;   (add-hook 'eglot-managed-mode-hook
;;             (lambda ()
;;               (when (eglot-managed-p)
;;                 (flycheck-mode -1)))))

(defun find-file-from-project-root (&rest file-names)
  (when-let ((root
              (or (and (functionp 'projectile-project-root)
                       (projectile-project-root))
                  (and (functionp 'project-current)
                       (project-current)
                       (functionp 'project-root)
                       (expand-file-name (project-root (project-current)))))))
    (seq-some
     (lambda (file-name)
       (car (file-expand-wildcards (concat (file-name-as-directory root) file-name) t)))
     file-names)))

(use-package posframe)
(use-package lsp-mode
  :quelpa (lsp-mode :repo "wyuenho/lsp-mode" :fetcher github :files (:defaults "clients/*.el") :branch "fix-all-the-icons")
  :after (projectile posframe)
  :delight (lsp-mode) (lsp-lens-mode)
  :hook ((c-mode-common
          c-ts-base-mode
          cmake-ts-mode
          enh-ruby-mode
          go-mode
          go-ts-mode
          groovy-mode
          js-base-mode
          python-base-mode
          rust-mode
          rust-ts-mode
          scala-mode
          swift-mode
          typescript-ts-base-mode
          tuareg-mode)
         . lsp-deferred)
  :demand
  :config
  (add-hook 'lsp-mode-hook
            (lambda ()
              (keymap-set lsp-mode-map "C-;" 'lsp-rename)
              (with-eval-after-load 'which-key
                (lsp-enable-which-key-integration))))

  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
                    '(flex))))

  (setf read-process-output-max (* 1024 1024 10))

  ;; https://github.com/blahgeek/emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)
               (not (file-remote-p default-directory))
               lsp-use-plists
               (not (functionp 'json-rpc-connection))
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

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
                       (cond ((derived-mode-p 'css-base-mode)
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
                             ((derived-mode-p 'js-base-mode 'typescript-ts-base-mode)
                              '((warning . javascript-eslint)))
                             ((derived-mode-p 'python-base-mode)
                              '((warning . python-ruff)
                                (warning . python-flake8)
                                (warning . python-mypy)
                                (warning . python-pylint)))
                             ((derived-mode-p 'enh-ruby-mode)
                              '((warning . ruby-rubocop)))
                             ((derived-mode-p 'go-mode 'go-ts-mode)
                              '((warning . golangci-lint)))
                             ((derived-mode-p 'rust-mode 'rust-ts-mode)
                              '((warning . rust-cargo)
                                (warning . rust)
                                (warning . rust-clippy))))))
                  (when lsp-next-checkers
                    (push `(lsp . ((next-checkers . ,lsp-next-checkers))) lsp-flycheck-checkers))
                  (when web-mode-checkers
                    (push web-mode-checkers lsp-flycheck-checkers)))))))

(use-package lsp-ui
  :after (lsp-mode)
  :init
  (defun lsp-ui-doc-frame-set-font (frame _)
    (when (eq (window-system) 'ns)
      (set-frame-font
       (font-spec :family "SF Pro Text" :size 11)
       nil
       (list frame))))
  :config
  (setf lsp-ui-doc-border (face-foreground 'window-divider nil t))
  (add-hook 'lsp-ui-doc-frame-hook 'lsp-ui-doc-frame-set-font))

;; Auto-completion
(use-package corfu
  :init
  (defun vscode-kind-icons-corfu-margin-formatter (_)
    (when-let ((company-kind-func (plist-get completion-extra-properties :company-kind))
               (_ (display-graphic-p)))
      (lambda (item)
        (let* ((kind (funcall company-kind-func item))
               (icon (alist-get kind vscode-kind-icons (all-the-icons-vscode-codicons "symbol-misc" :padding '(2 . 1)))))
          icon))))

  :config
  (setq read-extended-command-predicate 'command-completion-default-include-p)

  (add-hook 'corfu-margin-formatters 'vscode-kind-icons-corfu-margin-formatter)

  (with-eval-after-load 'transient
    (setq read-extended-command-predicate
          (lambda (sym buf)
            (and
             (transient-command-completion-not-suffix-only-p sym buf)
             (command-completion-default-include-p sym buf)))))
  (global-corfu-mode 1))

(use-package corfu-popupinfo
  :after (corfu))

(use-package corfu-pixel-perfect
  :quelpa (corfu-pixel-perfect :fetcher github :repo "wyuenho/corfu-pixel-perfect")
  :after (corfu))

;; (use-package corfu-terminal
;;   ;; :quelpa (corfu-terminal :fetcher codeberg :repo "wyuenho/emacs-corfu-terminal" :branch "remove-gui-mode")
;;   :hook (corfu-mode . corfu-terminal-mode))

(use-package cape
  :config
  (dolist (capf (nreverse `(cape-file ,(cape-capf-inside-code 'cape-keyword))))
    (add-hook 'completion-at-point-functions capf))

  (let ((elisp-capf (cape-capf-super
                     (cape-capf-nonexclusive
                      (cape-capf-inside-code 'cape-elisp-symbol))
                     (cape-capf-nonexclusive
                      (cape-capf-inside-code 'cape-elisp-block)))))

    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (kill-local-variable 'completion-at-point-functions)
                (add-hook 'completion-at-point-functions elisp-capf nil t))))

  (dolist (mode '(web nxml sgml js-ts js-jsx tsx-ts))
    (add-hook (intern (format "%s-mode-hook" mode))
              (lambda ()
                (add-hook 'completion-at-point-functions 'cape-sgml 90 t))))

  (add-hook 'tex-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions 'cape-tex 90 t))))

(use-package corfu-prescient
  :after (corfu)
  :hook (corfu-mode . corfu-prescient-mode))

;; Linting
(use-package spinner)

(use-package flycheck
  :init
  ;; With `flycheck' and `purpose-x-code-1' turned on, under `emacs-lisp-mode',
  ;; `diff-hl-dired-mode' on the dired `*Files*' will create git index lock
  ;; contention during `vc-git-dir-status-update' because some checkers like
  ;; emacs-lisp keeps creating and deleting files in the directory in-place.
  (defun flycheck-checker-arguments-advice (fn &rest args)
    "Ensure all checkers using `source-inplace' use `source' instead.

Some checkers like pyright might refuse to work properly, but
that's probably a command definition error. In that case, fix it
and submit a PR.

FN is `flycheck-checker-arguments', ARGS is its arguments."
    (let* ((checker (car args))
           (result (apply fn args))
           (source-inplace-pos (seq-position result 'source-inplace)))
      (when source-inplace-pos
        (setf (elt result source-inplace-pos) 'source))
      result))
  :delight
  :config
  (advice-add 'flycheck-checker-arguments :around 'flycheck-checker-arguments-advice)

  (defun flycheck-show-help-function (msg)
    (when flycheck-mode
      (if (null msg)
          (flycheck-clear-displayed-errors)
        (pcase-let* ((`(,frame ,x . ,y) (mouse-position))
                     (win (window-at x y frame))
                     (`(,body-left ,body-top) (window-body-edges win))
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

(use-package quick-peek
  :config
  (defun quick-peek--insert-spacer-advice (pos str-before str-after)
    (save-excursion
      (goto-char pos)
      (insert (propertize "\n" 'face 'quick-peek-border-face))))
  (advice-add 'quick-peek--insert-spacer :override 'quick-peek--insert-spacer-advice))

(use-package flycheck-inline
  :quelpa (flycheck-inline :repo "wyuenho/flycheck-inline" :fetcher github)
  :init
  (defun flycheck-inline-quick-peek (msg pos err)
    (unless (or (and (featurep 'company)
                     company-mode
                     (company--active-p))
                completion-in-region-mode)
      (pcase-let* ((`(,_ ,body-top ,__ ,body-bottom) (window-body-edges))
                   (line-offset (- (line-number-at-pos (point))
                                   (line-number-at-pos (window-start))))
                   (quick-peek-position
                    (if (< (- body-bottom body-top line-offset) 10) 'above 'below))
                   (ov (quick-peek-overlay-ensure-at pos))
                   (contents (quick-peek-overlay-contents ov)))
        (setf (quick-peek-overlay-contents ov)
              (concat contents (when contents "\n") (string-trim msg)))
        (quick-peek-update ov 1 4))))
  :after (flycheck quick-peek)
  :hook (flycheck-mode . flycheck-inline-mode)
  :custom
  (flycheck-inline-display-function #'flycheck-inline-quick-peek)
  (flycheck-inline-clear-function #'quick-peek-hide))

;; REST API
(use-package verb
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

;; API Blueprints
(use-package apib-mode
  :mode "\\.apib\\'")

;; Term and shell
(use-package shfmt
  :delight shfmt-on-save-mode
  :hook (sh-base-mode . shfmt-on-save-mode))

(setf eshell-directory-name (expand-file-name ".eshell/" user-emacs-directory))

(use-package vterm
  :init
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

;; Markup and config languages
(add-to-list 'auto-mode-alist '("\\.nmconnection\\'" . conf-mode))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (with-eval-after-load 'prettier
                (when (executable-find "prettier")
                  (prettier-mode))))))

(use-package yaml-ts-mode
  :defer
  :config
  (add-hook 'yaml-ts-mode-hook
            (lambda ()
              (with-eval-after-load 'prettier
                (when (executable-find "prettier")
                  (prettier-mode))))))

(use-package toml-ts-mode
  :config
  (add-hook 'toml-ts-mode-hook
            (lambda ()
              (with-eval-after-load 'prettier
                (when (executable-find "prettier")
                  (prettier-mode))))))

(use-package markdown-mode
  :init
  (defun markdown-fontify-code-block-natively-advice (fn &rest args)
    (cl-letf (((symbol-function 'run-mode-hooks) (symbol-function 'ignore)))
      (apply fn args)))
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (advice-add 'markdown-fontify-code-block-natively :around 'markdown-fontify-code-block-natively-advice))

(use-package org-src
  :after (org)
  :init
  (defun org-src-font-lock-fontify-block-advice (fn &rest args)
    (cl-letf (((symbol-function 'run-mode-hooks) (symbol-function 'ignore)))
      (apply fn args)))
  :config
  (advice-add 'org-src-font-lock-fontify-block :around 'org-src-font-lock-fontify-block-advice))

(use-package dockerfile-ts-mode)

(use-package dotenv-mode
  :mode "\\.env\\..*\\'")

;; Emacs Lisp
(defun calculate-lisp-indent-advice (fn &rest args)
  "Don't indent vectors in `emacs-lisp-mode' like lists."
  (if (save-excursion
        (beginning-of-line)
        (let ((start (point)))
          (save-excursion
            (beginning-of-defun)
            (let ((containing-sexp (elt (parse-partial-sexp (point) start) 1)))
              (when containing-sexp
                (goto-char containing-sexp)
                (looking-at "\\["))))))
      (let ((lisp-indent-offset 1))
        (apply fn args))
    (apply fn args)))
(advice-add 'calculate-lisp-indent :around 'calculate-lisp-indent-advice)

;; auto-indent built-in libs
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-prefix-p lisp-directory (buffer-file-name))
                       buffer-read-only)
              (with-silent-modifications
                (indent-region (point-min) (point-max)))))
          90)

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
  :hook ((emacs-lisp-mode ielm-mode) . elisp-def-mode))

(use-package helpful
  :quelpa (helpful :fetcher github :repo "wyuenho/helpful" :branch "search-after-navigate")
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)
         ("C-h p" . helpful-at-point)))

(defun eieio-browse-advice (&rest args)
  "Put EIEIO object browser buffer in special mode."
  (with-current-buffer (get-buffer "*EIEIO OBJECT BROWSE*")
    (special-mode)
    (select-window (get-buffer-window (current-buffer)))))
(advice-add 'eieio-browse :after 'eieio-browse-advice)

(use-package package-lint
  :bind (:map emacs-lisp-mode-map
              ("C-c e p" . package-lint-current-buffer)
              ("C-c e s" . package-lint-describe-symbol-history)))

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
            ".json,.js,.jsx,.mjs,.cjs,.ts,.tsx"))

  (reformatter-define denofmt-format
    :program "deno"
    :args `("fmt" "-"))

  (reformatter-define clang-format
    :program "clang-format"
    :args `("--style" "GNU")))

(dolist (mode '(css-base-mode
                js-base-mode
                jsonian-mode
                typescript-ts-base-mode
                web-mode))
  (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook mode-hook
              (lambda ()
                (let* ((formatter-styles
                        '((prettier prettier-eslint prettier)
                          (eslint eslint-plugin-prettier eslint)))
                       (formatter
                        (or (when-let* ((package-json-file
                                         (find-file-from-project-root "package.json"))
                                        (package-json
                                         (and package-json-file
                                              (json-read-file package-json-file)))
                                        (devDependencies
                                         (and package-json
                                              (alist-get 'devDependencies package-json))))
                              (car (seq-filter 'identity
                                               (map-apply
                                                (lambda (command packages)
                                                  (and (seq-some
                                                        (lambda (package)
                                                          (map-contains-key devDependencies package))
                                                        packages)
                                                       command))
                                                formatter-styles))))))
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
                        (yarn-eslint-format-on-save-mode)
                      (eslint-format-on-save-mode)))
                   ((and (eq formatter 'prettier)
                         (featurep 'prettier)
                         (executable-find "prettier"))
                    (prettier-mode))))))))

;; Java
;; (use-package lsp-java
;;   :after (lsp-mode)
;;   :custom
;;   (lsp-java-server-install-dir (car (file-expand-wildcards "~/.vscode/extensions/redhat.java-*/server"))))

;; Javascript
(add-to-list 'auto-mode-alist '("\\(?:\\.\\(?:[cm]?js\\)\\)" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\(?:\\.jsx\\)" . js-jsx-mode))

(dolist (mode '(js-mode js-ts-mode js-jsx-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda ()
              (pcase-dolist (`(,key . ,command)
                             '(("M-."     . nil)
                               ("C-c M-:" . nil)
                               ("C-c C-j" . nil)
                               ("C-M-x"   . nil)
                               ("<menu-bar>" . nil)))
                (define-key (symbol-value (derived-mode-map-name mode)) (kbd key) command)))))

(use-package jsdoc
  :config
  (dolist (mode '(js-mode js-ts-mode js-jsx-mode typescript-ts-mode tsx-ts-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda ()
                (keymap-set (symbol-value (derived-mode-map-name mode)) "C-c d" 'jsdoc)))))

(use-package prettier
  :delight
  :config
  (let ((css-parser (alist-get 'css-mode prettier-major-mode-parsers))
        (json-parser (alist-get 'json-mode prettier-major-mode-parsers))
        (js-parser (alist-get 'js-mode prettier-major-mode-parsers))
        (typescript-parser (alist-get 'typescript-mode prettier-major-mode-parsers))
        (toml-parser (alist-get 'toml-mode prettier-major-mode-parsers))
        (yaml-parser (alist-get 'yaml-mode prettier-major-mode-parsers)))
    (add-to-list 'prettier-major-mode-parsers (cons 'css-ts-mode css-parser))
    (add-to-list 'prettier-major-mode-parsers (cons 'js-jsx-mode js-parser))
    (add-to-list 'prettier-major-mode-parsers (cons 'js-json-mode js-parser))
    (add-to-list 'prettier-major-mode-parsers (cons 'js-ts-mode js-parser))
    (add-to-list 'prettier-major-mode-parsers (cons 'json-ts-mode json-parser))
    (add-to-list 'prettier-major-mode-parsers (cons 'jsonian-mode json-parser))
    (add-to-list 'prettier-major-mode-parsers (cons 'toml-ts-mode toml-parser))
    (add-to-list 'prettier-major-mode-parsers (cons 'tsx-ts-mode typescript-parser))
    (add-to-list 'prettier-major-mode-parsers (cons 'typescript-ts-mode typescript-parser))
    (add-to-list 'prettier-major-mode-parsers (cons 'yaml-ts-mode yaml-parser))))

(use-package nodejs-repl
  :bind(:map js-base-mode-map
             ("C-c e e" . nodejs-repl-send-last-expression)
             ("C-c e r" . nodejs-repl-send-region)
             ("C-c e b" . nodejs-repl-send-buffer)
             ("C-c e l" . nodejs-repl-load-file)
             ("C-c M-:" . nodejs-repl-switch-to-repl)))

(use-package jsonian
  :after (so-long)
  :mode ("\\.json[c5]?\\'" . jsonian-c-mode)
  :config
  (jsonian-no-so-long-mode)
  (with-eval-after-load 'flycheck
    (jsonian-enable-flycheck)))

;; TypeScript
(use-package typescript-ts-mode
  :config
  (add-to-list 'typescript-ts-mode--keywords "satisfies")
  (add-hook 'typescript-ts-base-mode-hook
            (lambda ()
              (when (find-file-from-project-root "deno.jsonc?")
                (denofmt-format-on-save-mode)))))

(use-package ts-comint
  :after (inheritenv)
  :bind (:map typescript-ts-base-mode-map
              ("C-x C-e" . ts-send-last-sexp)
              ("C-c e e" . ts-send-last-sexp-and-go)
              ("C-c e r" . ts-send-region-and-go)
              ("C-c e b" . ts-send-buffer-and-go)
              ("C-c e l" . ts-load-file-and-go)
              ("C-c M-:" . switch-to-ts))
  :config
  (inheritenv-add-advice 'run-ts))

;; Python
(add-to-list 'auto-mode-alist '("\\.pythonrc\\'"   . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.pylintrc\\'"   . conf-mode))
(add-to-list 'auto-mode-alist '("\\.flake8\\'"     . conf-mode))
(add-to-list 'auto-mode-alist '("/poetry.lock\\'"  . conf-toml-mode))
(add-to-list 'auto-mode-alist '("/Pipfile.lock\\'" . conf-toml-mode))

;; compilation-shell-minor-mode-map clash with comint-mode-map due to early-init
(add-hook 'inferior-python-mode-hook
          (lambda () (compilation-shell-minor-mode -1)))

(use-package sphinx-doc
  :delight
  :hook (python-base-mode . sphinx-doc-mode)
  :config
  (keymap-unset sphinx-doc-mode-map "C-c M-d")
  (keymap-set sphinx-doc-mode-map "C-c d" 'sphinx-doc))

(use-package python-black
  :delight python-black-on-save-mode
  :config
  (with-eval-after-load 'pet
    (add-hook 'pet-mode-hook (lambda ()
                               (when python-black-command
                                 (python-black-on-save-mode))))))

(use-package python-isort
  :delight python-isort-on-save-mode
  :config
  (with-eval-after-load 'pet
    (add-hook 'pet-mode-hook (lambda ()
                               (when python-isort-command
                                 (python-isort-on-save-mode))))))

(use-package ruff-format
  :delight ruff-format-on-save-mode
  :config
  (with-eval-after-load 'pet
    (add-hook 'pet-mode-hook (lambda ()
                               (when ruff-format-command
                                 (ruff-format-on-save-mode))))))

(use-package python-pytest)

(use-package lsp-jedi
  :after (lsp-mode))

(use-package pet
  :delight
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package lsp-pyright
  :after (lsp-mode pet)
  :config
  (pet-def-config-accessor pyrightconfig
                           :file-name "pyrightconfig.json"
                           :parser pet-parse-config-file)
  (let ((client (gethash 'pyright lsp-clients)))
    (setf (lsp--client-major-modes client) nil)
    (setf (lsp--client-activation-fn client)
          (lambda (file-name &optional _)
            (and (derived-mode-p 'python-mode 'python-ts-mode)
                 (or (pet-pyrightconfig)
                     (let-alist (pet-pyproject)
                       .tool.pyright)))))))

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

;; C/C++/Objective-C
(use-package cmake-ts-mode)

;; Go
(use-package go-ts-mode
  :after (reformatter lsp-mode)
  :init
  (reformatter-define goimports-format
    :program "goimports")

  (reformatter-define gofmt-format
    :program "gofmt"
    :args `("-s"))

  (reformatter-define gofumpt-format
    :program "gofumpt")

  (defun lsp-go-format-buffer ()
    (condition-case err
        (progn
          (lsp-format-buffer)
          (lsp-organize-imports))
      (error (minibuffer-message (error-message-string err)))))

  (defun go-setup-format-buffer-on-save ()
    (if (executable-find "gofumpt")
        (gofumpt-format-on-save-mode 1)
      (gofmt-format-on-save-mode 1))

    (when (executable-find "goimports")
      (goimports-format-on-save-mode 1)))

  (defun go-teardown-format-buffer-on-save ()
    (if (executable-find "gofumpt")
        (gofumpt-format-on-save-mode 0)
      (gofmt-format-on-save-mode 0))

    (when (executable-find "goimports")
      (goimports-format-on-save-mode 0)))

  :config
  (add-hook 'go-ts-mode-hook 'go-setup-format-buffer-on-save)

  (add-hook 'go-ts-mode-hook
            (lambda ()
              (with-eval-after-load 'lsp-mode
                (add-hook 'lsp-managed-mode-hook
                          (lambda ()
                            (if lsp-managed-mode
                                (progn
                                  (remove-hook 'go-ts-mode-hook 'go-setup-format-buffer-on-save)
                                  (go-teardown-format-buffer-on-save)

                                  (setq-local lsp-enable-indentation t
                                              lsp-enable-on-type-formatting t)

                                  (add-hook 'before-save-hook 'lsp-go-format-buffer nil t))

                              (kill-local-variable 'lsp-enable-indentation)
                              (kill-local-variable 'lsp-enable-on-type-formatting)

                              (remove-hook 'before-save-hook 'lsp-go-format-buffer t)

                              (add-hook 'go-ts-mode-hook 'go-setup-format-buffer-on-save)
                              (go-setup-format-buffer-on-save)))
                          nil t))))

  (add-hook 'go-ts-mode-hook
            (lambda ()
              (when (boundp 'corfu-pixel-perfect-ellipsis)
                (setq-local corfu-pixel-perfect-ellipsis 'proportional)))))

(use-package flycheck-golangci-lint
  :after (flycheck)
  :config
  (flycheck-golangci-lint-setup)
  (cl-pushnew 'go-ts-mode (flycheck-checker-get 'golangci-lint 'modes)))

;; Rust
(use-package rust-ts-mode
  :after (reformatter)
  :init
  (reformatter-define rustfmt-format
    :program "rustfmt"
    :args `("--emit" "stdout"))
  :config
  (add-hook 'rust-ts-mode-hook 'rustfmt-format-on-save-mode))

(use-package flycheck-rust
  :after (rust-ts-mode flycheck)
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

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

;; (use-package lsp-metals
;;   :after (lsp-mode))

;; Web
(use-package sass-mode
  :mode "\\.sass\\'")

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
  :after (:any web-mode)
  :hook (sgml-mode
         nxml-mode
         web-mode
         js-base-mode
         typescript-ts-base-mode)
  :config
  (define-key emmet-mode-keymap (kbd "C-c C-c w") nil)
  (define-key emmet-mode-keymap (kbd "C-c C-m w") 'emmet-wrap-with-markup)
  (add-hook 'emmet-mode-hook
            (lambda ()
              (when (member major-mode
                            '(js-ts-mode
                              js-jsx-mode
                              tsx-ts-mode))
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
  (projectile-mode)
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-x p" "projectile")
    (which-key-add-keymap-based-replacements projectile-command-map
      "4" "projectile-other-window"
      "5" "projectile-other-frame"
      "x" "projectile-run"
      "s" "projectile-search")))

(use-package projectile-rails
  :after (projectile)
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
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-call-git-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package magit
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (with-eval-after-load 'git-rebase
    ;; Vanilla undo has been completely unbound, this reenable undo in
    ;; `git-rebase-mode'
    (define-key git-rebase-mode-map (kbd "M-z") 'git-rebase-undo)))

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
  :config (magit-todos-mode))

(use-package magit-lfs
  :after (magit))

(use-package git-timemachine
  :config
  (defun git-timemachine-quit-advice ()
    "Quit the `git-timemachine' window and kill the buffer."
    (interactive) (quit-window t))
  (advice-add 'git-timemachine-quit :override 'git-timemachine-quit-advice))

(use-package monky
  :bind (("C-x v M-m" . monky-status)))

;; File management
(defun add-to-invisibility-spec-override-advice (element)
  "Dedup `buffer-invisibility-spec'."
  (if (eq buffer-invisibility-spec t)
      (setf buffer-invisibility-spec (list t)))
  (setq buffer-invisibility-spec
        (delete-dups (cons element buffer-invisibility-spec))))
(advice-add 'add-to-invisibility-spec :override 'add-to-invisibility-spec-override-advice)

(defun keymap-commands (keymap)
  "Return all commands from KEYMAP."
  (let (result)
    (map-keymap
     (lambda (event type)
       (cond ((keymapp type)
              (setq result (nconc result (keymap-commands type))))
             ((commandp type)
              (setq result (cons type result)))))
     keymap)
    (delete-dups result)))

(defun error-to-user-error-advice (fn &rest args)
  (condition-case err
      (apply fn args)
    (error (user-error (error-message-string err)))))

(with-eval-after-load 'dired
  (put 'dired-find-alternate-file 'disabled nil)

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
          (oname (buffer-name))
          (newbuf (apply fn args)))
      (when is-dired
        (when hide-dotfiles (dired-hide-dotfiles-mode))
        (setf truncate-lines tl
              word-wrap ww
              visual-line-mode vlm
              mode-line-format mlf
              auto-revert-verbose arv)
        (setq-local dired-hide-details-hide-information-lines hide-information-lines)
        (setq-local dired-hide-details-hide-symlink-targets hide-symlink-targets)
        (when hide-details (dired-hide-details-mode))
        (when (and (boundp 'purpose-x-code1-dired-buffer-name)
                   (equal oname purpose-x-code1-dired-buffer-name)
                   (not (equal oname (buffer-name))))
          (rename-buffer oname)))
      newbuf))
  (advice-add 'find-alternate-file :around 'find-alternate-file-advice)

  (add-hook 'dired-mode-hook
            (lambda ()
              (mapc (lambda (cmd)
                      (when (not (null (string-search "dired-" (symbol-name cmd))))
                        (advice-add cmd :around 'error-to-user-error-advice)))
                    (keymap-commands dired-mode-map)))
            90)

  (defface dired-executable
    '((t (:inherit font-lock-warning-face :weight normal)))
    "Face used for executables."
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
  :config
  ;; do not use :hook as it does not respect :if
  (add-hook 'dired-collapse-mode-hook 'all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :demand
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode)))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

;; Window management
(with-eval-after-load 'recentf
  (define-key recentf-dialog-mode-map [remap recentf-cancel-dialog] 'quit-window))

(with-eval-after-load 'wid-browse
  (define-key widget-browse-mode-map [remap bury-buffer] 'quit-window))

(with-eval-after-load 'frameset
  ;; Let the themes deal with these things
  (dolist (param '( background-mode tty-color-mode screen-gamma
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

(use-package ibuffer-projectile
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when (derived-mode-p 'ibuffer-mode)
                    (setq ibuffer-filter-groups (ibuffer-projectile-generate-filter-groups))))))))

(use-package window-purpose
  :quelpa (window-purpose :fetcher github :repo "wyuenho/emacs-purpose" :files (:defaults "layouts")
                          :branch "improve-code1")
  :config
  (define-key purpose-mode-map (kbd "C-c ,") nil)
  (define-key purpose-mode-map (kbd "C-c w") purpose-mode-prefix-map)

  (timeout-debounce! 'purpose-x-code1-update-changed 0.0416)

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
    (defun message-quit-window-advice (&rest _)
      "Quit the window after killing the message buffer."
      (quit-window))
    (advice-add 'message-send-and-exit :after 'message-quit-window-advice)
    (advice-add 'message-kill-buffer :after 'message-quit-window-advice))

  (with-eval-after-load 'window-purpose-x
    (add-to-list 'purpose-x-popwin-buffer-names "*Messages*")
    (add-to-list 'purpose-x-popwin-buffer-names "*Warnings*")
    (purpose-x-popwin-update-conf)

    (with-eval-after-load 'bytecomp
      (add-to-list 'purpose-x-popwin-buffer-names byte-compile-log-buffer)
      (purpose-x-popwin-update-conf))

    (with-eval-after-load 'native-compile
      (add-to-list 'purpose-x-popwin-buffer-names comp-log-buffer-name)
      (add-to-list 'purpose-x-popwin-buffer-names comp-async-buffer-name)
      (purpose-x-popwin-update-conf))

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

  (purpose-mode)

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

  (with-eval-after-load 'desktop
    (add-hook 'desktop-after-read-hook
              (lambda ()
                ;; Bury all special buffers after setting up dummy buffers and
                ;; restoring session buffers
                (dolist (buf (seq-filter
                              (lambda (buf)
                                (string-match-p "^ \\|\\*" (buffer-name buf)))
                              (buffer-list)))
                  (bury-buffer buf))))))
