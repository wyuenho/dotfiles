;; -*- lexical-binding: t -*-
(setenv "LSP_USE_PLISTS" "true")

(require 'cl-lib)
(require 'nadvice)

;; Bury the noisy compile logs
(defvar compile-log-buffer-names nil)

(define-advice get-buffer-create
    (:filter-return (buffer) "bury-compilation-mode")
  (when (member (buffer-name buffer) compile-log-buffer-names)
    (with-current-buffer buffer
      (unless (derived-mode-p 'compilation-mode)
        (compilation-mode)))
    (bury-buffer-internal buffer))
  buffer)

(with-eval-after-load 'compile
  (define-advice compilation-buffer-name
      (:filter-return (name) "record-compile-log-buffer-name")
    (add-to-list 'compile-log-buffer-names name)
    name)

  (set-keymap-parent compilation-shell-minor-mode-map special-mode-map))

(with-eval-after-load 'native-compile
  (when (boundp 'comp-log-buffer-name)
    (add-to-list 'compile-log-buffer-names comp-log-buffer-name))
  (when (boundp 'comp-async-buffer-name)
    (add-to-list 'compile-log-buffer-names comp-async-buffer-name)))

(with-eval-after-load 'comp
  (put 'native-comp-limple-mode 'derived-mode-parent 'special-mode)
  (when (boundp 'comp-log-buffer-name)
    (add-to-list 'compile-log-buffer-names comp-log-buffer-name)))

(with-eval-after-load 'comp-run
  (when (boundp 'comp-async-buffer-name)
    (add-to-list 'compile-log-buffer-names comp-async-buffer-name)))

(with-eval-after-load 'bytecomp
  (add-to-list 'compile-log-buffer-names byte-compile-log-buffer))

;; Patch package.el so it's slightly less insane
(with-eval-after-load 'package
  (define-advice package-delete (:around (fn &rest args) "queue-deletion")
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

  (define-advice package-unpack (:after (pkg-desc) "recompile-dependants")
    (cl-loop for (_ pkg) in (package--alist)
             if (member (package-desc-name pkg-desc)
                        (cl-loop for (req-name _) in (package-desc-reqs pkg)
                                 collect req-name))
             ;; FIXME: advice `package-activate-1' so it deals with versions
             ;; properly
             do (when (package-activate-1 pkg :reload :deps) ;; copied from package-unpack
                  (package--compile pkg)
                  (when package-native-compile
                    (package--native-compile-async pkg))
                  (if (functionp 'package--reload-previously-loaded)
                      (package--reload-previously-loaded pkg)
                    (package--load-files-for-activation pkg :reload)))))

  (with-eval-after-load 'quelpa
    (define-advice package--removable-packages (:filter-return (removable-packages) "skip-quelpa")
      "Make sure `package-autoremove' does not consider quelpa-installed packages.

FIXME: this currently does not take into account of package
versions due to limitations in package.el."
      (let ((quelpa-packages (mapcar #'car (quelpa-read-cache))))
        (cl-set-difference removable-packages quelpa-packages))))


  ;; FIXME: `package-autoremove' can't remove obsolete package because
  ;; `package--removable-packages' only returns the package name, but not the
  ;; version or the package description struct. Furthermore, the removable code
  ;; path always takes the latest version from `package-alist', which is wrong.
  )

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
                 (setf ns-use-thin-smoothing t
                       ns-use-mwheel-momentum t
                       ns-use-mwheel-acceleration t))))))

;; No more yes and no and y and n inconsistencies
(fset 'yes-or-no-p 'y-or-n-p)
