;; -*- lexical-binding: t -*-
(setenv "LSP_USE_PLISTS" "true")

(require 'cl-lib)

;; Set up default fonts
(custom-set-variables
 '(face-font-family-alternatives
   '(("JetBrains Mono NL" "Noto Sans Mono" "DejaVu Sans Mono" "Menlo")
     ("Noto Sans" "Helvetica Neue" "Helvetica"))))
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono NL" :weight 'regular :width 'normal)
(set-face-attribute 'fixed-pitch-serif nil :family "Courier New" :weight 'regular :width 'normal)
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :weight 'regular :width 'normal)
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
                 (setf ns-use-thin-smoothing t
                       ns-use-mwheel-momentum t
                       ns-use-mwheel-acceleration t
                       x-colors (ns-list-colors)))))))

;; After desktop.el has restored all the buffers, the top of the buffer list in
;; the last emacs session should be restored as the top.
(setf initial-buffer-choice (lambda () (car (buffer-list))))

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)

(with-eval-after-load 'package
  (setq package-install-upgrade-built-in t)

  (defun package-delete-advice (fn &rest args)
    "Queue package deletion during native compilation."
    (if (and (native-comp-available-p)
             (or comp-files-queue
                 (> (comp-async-runnings) 0)))
        (let* ((pkg-desc (car args))
               (pkg-name (symbol-name (package-desc-name pkg-desc)))
               (version-string (mapconcat (lambda (n) (format "%s" n)) (package-desc-version pkg-desc) "-"))
               (optional-args (mapconcat (lambda (arg) (format "%s" arg)) (cdr args) "-"))
               (func-name (mapconcat 'identity (list "package-delete" pkg-name version-string optional-args) "-"))
               (func-sym (intern func-name)))
          (fset func-sym (lambda ()
                           (condition-case err
                               (apply fn args)
                             (error (minibuffer-message "%s" (error-message-string err))))
                           (remove-hook 'native-comp-async-all-done-hook func-sym)
                           (unintern func-sym nil)))
          (add-hook 'native-comp-async-all-done-hook func-sym))
      (apply fn args)))
  (advice-add 'package-delete :around 'package-delete-advice)

  (defun package-unpack-advice (pkg-desc)
    "Recompile dependents after installation."
    (cl-loop for (_ pkg) in (package--alist)
             if (member (package-desc-name pkg-desc)
                        (cl-loop for (req-name _) in (package-desc-reqs pkg)
                                 collect req-name))
             do (when (package-activate-1 pkg :reload :deps) ;; copied from package-unpack
                  (package--compile pkg)
                  (when package-native-compile
                    (package--native-compile-async pkg))
                  (if (functionp 'package--reload-previously-loaded)
                      (package--reload-previously-loaded pkg)
                    (package--load-files-for-activation pkg :reload)))))
  (advice-add 'package-unpack :after 'package-unpack-advice)

  (defun package--get-activatable-pkg-advice (pkg-name)
    "So `package-activate' can activate the correct version."
    (let ((pkg-descs (sort (cdr (assq pkg-name package-alist))
                           (lambda (p1 p2)
                             (let ((v1 (package-desc-version p1))
                                   (v2 (package-desc-version p2)))
                               (or
                                ;; Prefer VC packages.
                                (package-vc-p p1)
                                (package-vc-p p2)
                                (version-list-< v2 v1)))))))
      (while
          (when pkg-descs
            (let ((available-version (package-desc-version (car pkg-descs))))
              (package-disabled-p pkg-name available-version)))
        (setq pkg-descs (cdr pkg-descs)))
      (car pkg-descs)))
  (advice-add 'package--get-activatable-pkg :override 'package--get-activatable-pkg-advice)

  (with-eval-after-load 'quelpa
    (defun package--removable-packages-advice (fn &rest args)
      "Make sure `package-autoremove' does not consider quelpa-installed packages.

FIXME: this currently does not take into account of package
versions due to limitations in package.el."
      (let ((removable-packages (apply fn args))
            (quelpa-packages (mapcar #'car (quelpa-read-cache))))
        (cl-set-difference removable-packages quelpa-packages)))
    (advice-add 'package--removable-packages :around 'package--removable-packages-advice)))
