(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(ad-default-compilation-action (quote always))
 '(ag-arguments (quote ("--smart-case" "--stats" "--search-zip")))
 '(ag-highlight-search t)
 '(ag-ignore-list (quote ("*.min.js")))
 '(ag-reuse-buffers t)
 '(ag-reuse-window t)
 '(amx-mode t)
 '(apib-drafter-executable "aglio")
 '(auth-sources (quote ("~/.authinfo.gpg")))
 '(auto-compile-on-save-mode t)
 '(auto-compile-update-autoloads t)
 '(auto-hscroll-mode (quote current-line))
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-timeout 10)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(bash-completion-args nil)
 '(bash-completion-prog "/opt/local/bin/bash")
 '(blink-cursor-mode t)
 '(browse-url-chrome-program "chrome")
 '(c-basic-offset 2)
 '(case-fold-search t)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(company-global-modes
   (quote
    (lisp-mode emacs-lisp-mode restclient-mode js-mode js2-mode js-jsx-mode js2-jsx-mode rjsx-mode python-mode go-mode rust-mode web-mode c-mode c++-mode objc-mode css-mode sh-mode typescript-mode ruby-mode)))
 '(company-lsp-enable-recompletion t)
 '(company-require-match nil)
 '(company-statistics-mode t)
 '(company-tooltip-align-annotations t)
 '(compilation-message-face (quote default))
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(current-language-environment "English")
 '(custom-buffer-done-kill t)
 '(dabbrev-case-fold-search nil)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-modes-not-to-save
   (quote
    (tags-table-mode Info-mode info-lookup-mode fundamental-mode help-mode shell-mode completion-list-mode inferior-python-mode comint-mode anaconda-mode)))
 '(desktop-path (quote ("~/.emacs.d")))
 '(desktop-save (quote if-exists))
 '(desktop-save-mode t)
 '(diff-hl-draw-borders nil)
 '(diff-switches "-u -B")
 '(dimmer-fraction 0.3)
 '(dired-auto-revert-buffer (quote dired-directory-changed-p))
 '(dired-dwim-target t)
 '(dired-omit-verbose nil)
 '(display-line-numbers-width-start t)
 '(display-time-world-list
   (quote
    (("America/Los_Angeles" "San Francisco")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Hong_Kong" "Hong Kong")
     ("Asia/Tokyo" "Tokyo"))))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(editorconfig-mode t)
 '(eglot-autoreconnect 300)
 '(eglot-connect-timeout 300)
 '(eldoc-eval-preferred-function (quote eval-expression))
 '(eldoc-in-minibuffer-mode t)
 '(emmet-indentation 2)
 '(emmet-self-closing-tag-style " /")
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(epa-file-cache-passphrase-for-symmetric-encryption t)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GOPATH")))
 '(executable-prefix-env t)
 '(explicit-shell-file-name "/opt/local/bin/bash")
 '(fill-column 80)
 '(flx-ido-mode t)
 '(flx-ido-use-faces t)
 '(flycheck-global-modes
   (quote
    (sh-mode json-mode yaml-mode js-mode js-jsx-mode js2-mode js2-jsx-mode rjsx-mode typescript-mode python-mode go-mode web-mode css-mode scss-mode rust-mode)))
 '(flycheck-pos-tip-max-width 80)
 '(gc-cons-threshold 35000000)
 '(global-auto-revert-non-file-buffers t)
 '(global-company-mode t)
 '(global-diff-hl-mode t)
 '(global-flycheck-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t)
 '(global-magit-file-mode t)
 '(global-move-dup-mode t)
 '(global-origami-mode t)
 '(global-syntax-subword-mode t)
 '(global-undo-tree-mode t)
 '(global-whitespace-cleanup-mode t)
 '(gnutls-crlfiles
   (quote
    ("/opt/local/etc/grid-security/certificates/*.crl.pem" "/etc/grid-security/certificates/*.crl.pem")))
 '(gnutls-min-prime-bits nil)
 '(gnutls-trustfiles
   (quote
    ("/opt/local/etc/openssl/cert.pem" "/etc/ssl/cert.pem" "/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt")))
 '(help-at-pt-display-when-idle
   (quote
    (keymap local-map button kbd-help flymake-diagnostic)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.5)
 '(history-length 250)
 '(ibuffer-elide-long-columns t)
 '(ibuffer-expert t)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-enable-tramp-completion nil)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(ido-vertical-mode t)
 '(imagemagick-render-type 1)
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 524288)
 '(imenu-list-size 0.2)
 '(imenu-max-items 100)
 '(importmagic-be-quiet t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "hunspell")
 '(js-chain-indent t)
 '(js-doc-author "Jimmy Yuen Ho Wong")
 '(js-doc-license "MIT @license")
 '(js-doc-mail-address "wyuenho@gmail.com")
 '(js-enabled-frameworks (quote (javascript)))
 '(js-flat-functions t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-imenu-enabled-frameworks (quote (jquery jquery-ui dojo backbone enyo sencha)))
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-skip-preprocessor-directives t)
 '(json-reformat:indent-width 2)
 '(kept-new-versions 10)
 '(linum-delay t)
 '(linum-format "%4d ")
 '(load-prefer-newer t)
 '(lsp-inhibit-message t)
 '(lsp-report-if-no-buffer nil)
 '(lsp-response-timeout 30)
 '(lsp-ui-doc-position (quote at-point))
 '(lsp-ui-sideline-ignore-duplicate t)
 '(mac-emulate-three-button-mouse t)
 '(mac-input-method-mode t)
 '(mac-mouse-wheel-mode t)
 '(mac-print-mode t)
 '(mac-system-move-file-to-trash-use-finder t)
 '(magit-diff-refine-hunk (quote all))
 '(magit-diff-use-overlays nil)
 '(magithub-clone-default-directory "~/Documents/workspace")
 '(markdown-code-lang-modes
   (quote
    (("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("javascript" . js2-mode)
     ("jsx" . rjsx-mode)
     ("typescript" . typescript-mode)
     ("tsx" . web-mode)
     ("html" . web-mode)
     ("css" . web-mode)
     ("python" . python-mode)
     ("go" . go-mode))))
 '(markdown-command
   "markdown_py -x pymdownx.github -x pymdownx.highlight --output_format=html5")
 '(markdown-css-paths
   (quote
    ("https://cdn.rawgit.com/aahan/pygments-github-style/master/github.css" "https://unpkg.com/github-markdown-css@2.9.0/github-markdown.css")))
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-indent-on-enter nil)
 '(markdown-xhtml-body-epilogue "</article>")
 '(markdown-xhtml-body-preamble "<article class=\"markdown-body\">")
 '(menu-bar-mode t)
 '(minibuffer-frame-alist (quote ((width . 80) (height . 2))))
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-tilt-scroll t)
 '(multi-term-program "/opt/local/bin/bash")
 '(multi-term-program-switches "--login")
 '(network-security-level (quote high))
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(ns-pop-up-frames nil)
 '(nsm-save-host-names t)
 '(olivetti-body-width 120)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (add-node-modules-path ag all-the-icons all-the-icons-dired amx apib-mode async auto-compile auto-yasnippet blacken browse-kill-ring buffer-move bug-hunter cargo cl-lib-highlight clang-format cmake-font-lock company-emoji company-flx company-go company-lsp company-quickhelp company-restclient company-shell company-statistics company-tern company-web cquery crux csv-mode cycle-quotes default-text-scale delight diff-hl dimmer dired-collapse dired-hacks-utils dired-hide-dotfiles dired-single docker dockerfile-mode dumb-jump editorconfig eglot elisp-def elisp-refs emmet-mode eslintd-fix evil-numbers exec-path-from-shell expand-region flx-ido flx-isearch flycheck flycheck-clang-analyzer flycheck-golangci-lint flycheck-mypy flycheck-objc-clang flycheck-pos-tip flycheck-rust flycheck-yamllint focus fontify-face form-feed git-timemachine gitattributes-mode gitconfig-mode gitignore-mode go-eldoc go-mode go-projectile god-mode goto-chg graphviz-dot-mode helpful hook-helpers ialign ido-completing-read+ ido-vertical-mode imenu-anywhere import-js importmagic jq-mode js-doc js-format js2-mode js2-refactor kurecolor lorem-ipsum lsp-go lsp-ocaml lsp-ui macrostep magit magit-filenotify magit-gh-pulls magit-gitflow magit-todos magithub monky move-dup multi-term multiple-cursors nodejs-repl nvm olivetti origami osx-trash package-build package-lint pager-default-keybindings pcre2el pdf-tools persp-mode persp-mode-projectile-bridge prettier-js projectile py-autopep8 py-isort pyimport python python-docstring quelpa-use-package racer rainbow-mode reason-mode restclient rg rjsx-mode rust-mode scss-mode smartparens smooth-scrolling solarized-theme spaceline sphinx-doc string-inflection tern tide try ts-comint tuareg typescript-mode undo-tree use-package visual-regexp-steroids web-mode wgrep-ag which-key whitespace-cleanup-mode xterm-color yaml-mode yarn-mode yasnippet yasnippet-snippets)))
 '(pipenv-projectile-after-switch-function (quote pipenv-projectile-after-switch-extended))
 '(projectile-mode t nil (projectile))
 '(purpose-x-code1-dired-goto-file t)
 '(purpose-x-popwin-major-modes
   (quote
    (help-mode compilation-mode occur-mode helpful-mode)))
 '(quelpa-checkout-melpa-p nil)
 '(quelpa-self-upgrade-p nil)
 '(rainbow-html-colors-major-mode-list
   (quote
    (html-mode php-mode nxml-mode xml-mode web-mode rjsx-mode mhtml-mode)))
 '(reb-re-syntax (quote string))
 '(recentf-auto-cleanup (quote never))
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(require-final-newline (quote ask))
 '(ring-bell-function (quote ignore))
 '(rst-adjust-hook (quote rst-toc-update))
 '(rst-indent-comment 2)
 '(rst-indent-field 2)
 '(rst-indent-literal-normal 2)
 '(rxt-global-mode t)
 '(save-place-mode t)
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(send-mail-function (quote smtpmail-send-it))
 '(server-mode t)
 '(shell-file-name "/opt/local/bin/bash")
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(show-smartparens-global-mode t)
 '(size-indication-mode t)
 '(smartparens-global-mode t)
 '(smooth-scrolling-mode t)
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-stream-type (quote starttls))
 '(solarized-use-variable-pitch nil)
 '(sp-hybrid-kill-excessive-whitespace t)
 '(spaceline-all-the-icons-clock-always-visible nil)
 '(spaceline-all-the-icons-separator-type (quote arrow))
 '(standard-indent 2)
 '(tab-stop-list
   (quote
    (2 4 8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)))
 '(tab-width 2)
 '(temp-buffer-resize-mode t)
 '(timer-max-repeats 1)
 '(tls-checktrust t)
 '(tls-program
   (quote
    ("openssl s_client -connect %h:%p -no_ssl3 -no_ssl2 -ign_eof -CAfile %t" "gnutls-cli --x509cafile %t -p %p --dh-bits=2048 --ocsp --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:%%PROFILE_MEDIUM' %h")))
 '(tool-bar-mode nil)
 '(treemacs-change-root-without-asking t)
 '(treemacs-collapse-dirs 3)
 '(treemacs-filewatch-mode t)
 '(treemacs-follow-mode t)
 '(treemacs-git-integration t)
 '(treemacs-no-png-images t)
 '(truncate-lines t)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(url-automatic-caching t)
 '(use-package-enable-imenu-support t)
 '(user-full-name "Jimmy Yuen Ho Wong")
 '(user-mail-address "wyuenho@gmail.com")
 '(vc-follow-symlinks nil)
 '(vr/engine (quote pcre2el))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(web-mode-style-padding 2)
 '(which-function-mode t)
 '(which-key-idle-secondary-delay 0.0)
 '(which-key-mode t)
 '(winner-mode t)
 '(x-underline-at-descent-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
