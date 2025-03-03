;; -*- lexical-binding: t -*-
(setenv "LSP_USE_PLISTS" "true")

(with-eval-after-load 'compile
  (set-keymap-parent compilation-shell-minor-mode-map special-mode-map))

(with-eval-after-load 'comp
  (setq native-comp-enable-subr-trampolines nil)
  (set-keymap-parent native-comp-limple-mode-map special-mode-map))

(setq package-enable-at-startup nil)

;; Set up default fonts
(custom-set-variables
 '(face-font-family-alternatives
   '(("JetBrains Mono NL" "Noto Sans Mono" "DejaVu Sans Mono" "Menlo")
     ("SF Pro" "Helvetica Neue" "Helvetica" "Noto Sans"))))
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono NL" :weight 'regular :width 'normal)
(set-face-attribute 'fixed-pitch-serif nil :family "Courier New" :weight 'regular :width 'normal)
;; Used for UI elements. Customize individual faces for text related uses.
(set-face-attribute 'variable-pitch nil :family "SF Pro" :weight 'regular :width 'normal)
(set-face-attribute 'default nil :family "JetBrains Mono NL" :weight 'regular :width 'normal)

(load-theme 'bootstrap t)

;; Set up initial and default frame params
(pcase-dolist (`(,param . ,value)
               '((background-mode        . dark)
                 (fullscreen             . maximized)
                 (cursor-type            . bar)
                 (vertical-scroll-bars   . nil)
                 (horizontal-scroll-bars . nil)))
  (setf (alist-get param initial-frame-alist) value))

(pcase-dolist (`(,param . ,value)
               '((ns-transparent-titlebar . nil)
                 (ns-appearance           . dark)))
  (push (cons param value) (alist-get 'ns window-system-default-frame-alist nil)))

;; Set up frame title format
(setf frame-title-format (list '(:eval
                                 (when (buffer-file-name)
                                   (abbreviate-file-name (buffer-file-name))))))

;; Turn off the tool bar early
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Set up NS port specific variables
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (pcase (window-system)
                ('ns
                 (setq ns-use-thin-smoothing t
                       ns-use-mwheel-momentum t
                       ns-use-mwheel-acceleration t))))))
