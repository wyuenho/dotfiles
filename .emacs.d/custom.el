(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(ac-candidate-menu-min 2)
 '(ac-menu-height 20)
 '(ac-trigger-commands nil)
 '(ad-default-compilation-action (quote always))
 '(auto-compile-on-load-mode t)
 '(auto-compile-on-save-mode t)
 '(auto-compile-update-autoloads t)
 '(auto-revert-check-vc-info t)
 '(auto-revert-use-notify t)
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(background-mode light)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(blink-cursor-mode t)
 '(c-basic-offset 2)
 '(case-fold-search t)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(current-language-environment "English")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(dabbrev-case-fold-search nil)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-modes-not-to-save
   (quote
    (tags-table-mode Info-mode info-lookup-mode fundamental-mode help-mode shell-mode completion-list-mode inferior-python-mode comint-mode)))
 '(desktop-path (quote ("~/.emacs.d")))
 '(desktop-restore-eager 5)
 '(desktop-save-mode t)
 '(diff-switches "-u -B")
 '(ecb-layout-name "right1")
 '(ecb-tip-of-the-day nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(el-get-user-package-directory "~/.emacs.d/el-get-init-files/")
 '(emmet-indentation 2)
 '(ensime-default-server-root "~/.emacs.d/ensime")
 '(eval-expression-print-level 1000)
 '(exec-path-from-shell-check-startup-files nil)
 '(fill-column 80)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy jade javascript-eslint javascript-jshint javascript-gjslint javascript-jscs javascript-standard json-jsonlint json-python-json less luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-rubylint ruby ruby-jruby rust-cargo rust sass scala scala-scalastyle scss-lint scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim sql-sqlint tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flymake-no-changes-timeout 1)
 '(global-anzu-mode t)
 '(global-company-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-move-dup-mode t)
 '(global-undo-tree-mode t)
 '(global-visual-line-mode t)
 '(global-whitespace-cleanup-mode t)
 '(graphviz-dot-delete-completions t)
 '(graphviz-dot-toggle-completions t)
 '(history-length 250)
 '(ido-enable-tramp-completion nil)
 '(ido-mode (quote both) nil (ido))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 524288)
 '(imenu-max-items 100)
 '(imenu-sort-function (quote imenu--sort-by-position))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "hunspell")
 '(jedi-direx:hide-imports t)
 '(jedi:complete-on-dot t)
 '(jedi:install-imenu t)
 '(js-doc-author "Jimmy Yuen Ho Wong")
 '(js-doc-license "MIT @license")
 '(js-doc-mail-address "wyuenho@gmail.com")
 '(js-enabled-frameworks (quote (javascript)))
 '(js-expr-indent-offset -2)
 '(js-flat-functions t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-basic-offset 2)
 '(js2-include-jslint-globals nil)
 '(js2-include-node-externs t)
 '(js2-strict-inconsistent-return-warning nil)
 '(kept-new-versions 10)
 '(kept-old-versions 10)
 '(linum-delay t)
 '(linum-format "%4d ")
 '(mac-emulate-three-button-mouse (quote reverse))
 '(mac-input-method-mode t)
 '(mac-print-mode t)
 '(magit-diff-use-overlays nil)
 '(make-pointer-invisible nil)
 '(markdown-command "markdown_py -x extra -x sane_lists")
 '(menu-bar-mode t)
 '(minibuffer-frame-alist (quote ((width . 80) (height . 2))))
 '(minimap-dedicated-window t)
 '(minimap-window-location (quote right))
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-auto-compile mode-line-remote mode-line-frame-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(neo-theme (quote icons))
 '(nlinum-format "%4d")
 '(ns-command-modifier (quote meta))
 '(ns-pop-up-frames nil)
 '(nxhtml-default-encoding (quote utf-8))
 '(nxhtml-skip-welcome t)
 '(nxml-default-buffer-file-coding-system (quote utf-8))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://www.mirrorservice.org/sites/melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (syntax-subword use-package mocha-snippets react-snippets editorconfig markdown-mode magit magit-filenotify magit-gh-pulls magit-gitflow move-dup all-the-icons-ivy counsel counsel-projectile ivy swiper projectile try tide spaceline-all-the-icons spaceline persp-projectile perspective vlf rich-minority golden-ratio gitattributes-mode gitconfig-mode gitignore-mode company-quickhelp which-key expand-region magithub ztree rainbow-mode exec-path-from-shell less-css-mode whitespace-cleanup-mode anaconda-mode company string-inflection eslintd-fix py-autopep8 all-the-icons-dired neotree pyimport pyenv-mode-auto python python-docstring company-dict company-shell zoom-frm yasnippet yaml-mode web-mode undo-tree sublimity sphinx-doc solarized-theme smooth-scrolling smartparens scss-mode py-isort nose multiple-cursors monky memory-usage js-doc iedit highlight-indent-guides graphviz-dot-mode flycheck-mypy evil-numbers emmet-mode dumb-jump dockerfile-mode csv-mode company-web company-tern company-anaconda auto-compile ag)))
 '(php-mode-warn-if-mumamo-off "Don't warn")
 '(php-template-compatibility nil)
 '(popcmp-completion-style (quote emacs-default))
 '(powerline-gui-use-vcs-glyph t)
 '(py-honor-comment-indentation (quote other))
 '(python-skeleton-autoinsert t)
 '(recentf-auto-cleanup (quote never))
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(require-final-newline (quote ask))
 '(ring-bell-function (quote ignore))
 '(rm-blacklist
   (quote
    (" Anaconda" " FlyC" " DS" " hs" " Anzu" " $" " ElDoc" " yas" " company" " md" " Undo-Tree" " WK" " hl-p" " WSC" " Wrap")))
 '(rst-adjust-hook (quote rst-toc-update))
 '(rst-indent-comment 2)
 '(rst-indent-field 2)
 '(rst-indent-literal-normal 2)
 '(safe-local-variable-values
   (quote
    ((pungi-setup-jedi . t)
     (pungi-setup-jedi)
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode)
           (when
               (fboundp
                (quote flycheck-mode))
             (flycheck-mode -1))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons ".." load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)))))
 '(save-place-mode t)
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(scss-compile-at-save nil)
 '(select-enable-clipboard t)
 '(semantic-imenu-auto-rebuild-directory-indexes t)
 '(semantic-imenu-index-directory t)
 '(semantic-which-function-use-color t)
 '(server-mode t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smartparens-global-mode t)
 '(smooth-scrolling-mode t)
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-smtp-user "wyuenho")
 '(smtpmail-stream-type (quote ssl))
 '(spaceline-all-the-icons-clock-always-visible nil)
 '(spaceline-all-the-icons-hide-long-buffer-path t)
 '(standard-indent 2)
 '(tab-stop-list
   (quote
    (2 4 8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)))
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(udev-cedet-dir "~/.emacs.d")
 '(udev-ecb-dir "~/.emacs.d")
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(vc-follow-symlinks nil)
 '(version-control t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(web-mode-style-padding 2)
 '(which-func-modes
   (quote
    (emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode python-mode)))
 '(which-function-mode t)
 '(which-key-paging-key nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "DejaVu Sans Mono"))))
 '(flycheck-fringe-error ((t (:background nil :foreground "#FF6E64" :weight bold))))
 '(flycheck-fringe-info ((t (:background nil :foreground "#69B7F0" :weight bold))))
 '(flycheck-fringe-warning ((t (:background nil :foreground "#DEB542" :weight bold))))
 '(js2-warning ((t (:underline (:color "orange" :style wave)))))
 '(linum ((((type tty) (class color) (min-colors 16)) (:background "dim gray")) (((type x w32 mac)) (:background "#003b4a" :foreground "#586e75" :underline nil :weight normal))))
 '(mode-line ((t (:background "#073642" :foreground "#839496" :box (:line-width -1 :color "#073642") :overline nil :underline nil))))
 '(mouse ((t (:background "white"))))
 '(smerge-base ((((class color) (min-colors 88) (background light) (supports)) (:background "#ffffaa")) (((class color) (min-colors 88) (background dark) (supports :foreground "white smoke")) (:background "#888833")) (((class color)) (:foreground "yellow"))))
 '(smerge-refined-changed ((t (:foreground "white smoke")))))
