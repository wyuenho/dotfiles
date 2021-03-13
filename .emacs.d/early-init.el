;; -*- lexical-binding: t -*-
(require 'cl-lib)

;; Set up default fonts
(set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono" :weight 'regular :width 'normal)
(set-face-attribute 'fixed-pitch-serif nil :family "Courier New" :weight 'regular :width 'normal)
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :weight 'regular :width 'normal)
(set-face-attribute 'default nil :family "Noto Sans Mono" :weight 'regular :width 'normal)

;; Will at least display native Unicode emojis if the multicolor font
;; patch is applied
;; (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)

;; Set up initial and default frame params
(pcase-dolist (`(,param . ,value)
               '((background-mode      . dark)
                 (background-color     . "#002b36")
                 (scoll-bar-background . "#002b36")
                 (fullscreen           . maximized)))
  (setf (alist-get param initial-frame-alist) value))

(pcase-dolist (`(,param . ,value)
               '((ns-transparent-titlebar . nil)
                 (ns-appearance           . dark)))
  (push (cons param value) (alist-get 'ns window-system-default-frame-alist nil)))

;; Set up frame title format
(setq frame-title-format (list '(:eval
                                 (when (buffer-file-name)
                                   (abbreviate-file-name (buffer-file-name))))))

;; Turn off the turn the tool bar early
(tool-bar-mode -1)

;; Set up NS port specific variables
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (pcase (window-system)
                ('ns
                 (setq ns-use-thin-smoothing t
                       ns-use-mwheel-momentum t
                       ns-use-mwheel-acceleration t))))))

;; After desktop.el has restored all the buffers, the top of the buffer list in
;; the last emacs session should be restored as the top.
(setq initial-buffer-choice (lambda () (car (buffer-list))))

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

(with-eval-after-load 'package
  (defun package--removable-packages-advice (fn &rest args)
    "Make sure `package-autoremove' does not consider quelpa-installed packages.

FIXME: this currently does not take into account of package
versions due to limitations in package.el."
    (let ((removable-packages (apply fn args))
          (quelpa-packages (mapcar #'car (quelpa-read-cache))))
      (cl-set-difference removable-packages quelpa-packages)))
  (advice-add 'package--removable-packages :around 'package--removable-packages-advice))
