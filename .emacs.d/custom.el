(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(abridge-diff-mode t)
 '(ad-default-compilation-action 'always)
 '(ag-arguments '("--smart-case" "--stats" "--search-zip" "--all-types"))
 '(ag-highlight-search t)
 '(ag-ignore-list '("*.min.js"))
 '(ag-reuse-buffers t)
 '(ag-reuse-window t)
 '(aggressive-indent-excluded-modes
   '(elm-mode haskell-mode inf-ruby-mode makefile-mode makefile-gmake-mode python-mode python-ts-mode sql-interactive-mode text-mode yaml-mode yaml-ts-mode tcl-mode dockerfile-mode dockerfile-ts-mode go-mode go-ts-mode json-ts-mode jsonian-mode web-mode java-mode))
 '(apib-drafter-executable "aglio")
 '(auth-source-protocols
   '((imap "imap" "imaps" "143" "993")
     (pop3 "pop3" "pop" "pop3s" "110" "995")
     (ssh "ssh" "22")
     (sftp "sftp" "115")
     (smtp "smtp" "25" "465" "587")))
 '(auth-sources '("~/.authinfo.gpg"))
 '(auto-compile-on-save-mode t)
 '(auto-compile-update-autoloads t)
 '(auto-hscroll-mode 'current-line)
 '(auto-revert-avoid-polling t)
 '(auto-revert-check-vc-info t)
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-timeout 10)
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(bash-completion-args nil)
 '(bash-completion-prog "/opt/local/bin/bash")
 '(bidi-paragraph-direction 'left-to-right)
 '(blink-cursor-mode t)
 '(browse-url-chrome-program "chrome")
 '(byte-compile-warnings '(not docstrings docstrings-non-ascii-quotes))
 '(c-basic-offset 2)
 '(case-fold-search t)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(company-box-icons-alist 'company-box-icons-all-the-icons)
 '(company-dabbrev-downcase nil)
 '(company-global-modes
   '(c++-mode c-mode css-mode c++-ts-mode c-ts-mode css-ts-mode emacs-lisp-mode enh-ruby-mode go-mode go-ts-mode inferior-emacs-lisp-mode js-ts-mode js-jsx-mode js-mode lisp-interaction-mode lisp-mode objc-mode python-mode python-ts-mode restclient-mode ruby-mode rust-ts-mode rust-mode rust-ts-mode scss-mode sh-mode sh-base-mode typescript-ts-mode tsx-ts-mode web-mode org-mode))
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 20)
 '(compilation-always-kill t)
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(custom-buffer-done-kill t)
 '(dabbrev-case-fold-search nil)
 '(dap-auto-configure-mode t)
 '(dap-python-debugger 'debugpy)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-minor-mode-table
   '((defining-kbd-macro nil)
     (isearch-mode nil)
     (vc-mode nil)
     (vc-dir-mode nil)
     (erc-track-minor-mode nil)
     (savehist-mode nil)
     (anaconda-mode nil)
     (auto-compile-mode nil)))
 '(desktop-modes-not-to-save
   '(tags-table-mode Info-mode info-lookup-mode fundamental-mode help-mode shell-mode completion-list-mode inferior-python-mode comint-mode))
 '(desktop-path '("~/.emacs.d"))
 '(desktop-save 'if-exists)
 '(desktop-save-mode t)
 '(diff-font-lock-syntax nil)
 '(diff-hl-draw-borders nil)
 '(diff-refine 'navigation)
 '(dimmer-fraction 0.3)
 '(dired-auto-revert-buffer 'dired-directory-changed-p)
 '(dired-dwim-target t)
 '(dired-omit-verbose nil)
 '(direnv-mode t)
 '(display-line-numbers-widen t)
 '(display-line-numbers-width-start t)
 '(display-time-world-list
   '(("America/Los_Angeles" "San Francisco")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Hong_Kong" "Hong Kong")
     ("Asia/Tokyo" "Tokyo")))
 '(edebug-eval-macro-args t)
 '(edebug-print-length 120)
 '(edebug-unwrap-results t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(editorconfig-exclude-modes '(image-mode))
 '(editorconfig-exclude-regexps '("\\`\\*" "\\` "))
 '(editorconfig-mode t)
 '(eglot-autoreconnect 300)
 '(eglot-connect-timeout 300)
 '(eldoc-eval-preferred-function 'eval-expression)
 '(emmet-indentation 2)
 '(emmet-self-closing-tag-style " /")
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(epa-file-cache-passphrase-for-symmetric-encryption t)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(eval-expression-print-maximum-character 4194303)
 '(exec-path-from-shell-arguments '("-l"))
 '(exec-path-from-shell-variables
   '("PATH" "MANPATH" "GOPATH" "PYTHONSTARTUP" "PYENV_ROOT" "SDKMAN_DIR" "NVM_DIR" "MAVEN_HOME" "SBT_HOME" "LEININGEN_HOME" "ANT_HOME" "GRADLE_HOME" "KEYTOOL" "JAVA_HOME" "KOTLIN_HOME" "GROOVY_HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME" "XDG_CONFIG_DIRS" "XDG_DATA_DIRS"))
 '(executable-prefix-env t)
 '(expand-region-fast-keys-enabled nil)
 '(explicit-shell-file-name "/opt/local/bin/bash")
 '(face-font-family-alternatives
   '(("JetBrains Mono NL" "Noto Sans Mono" "DejaVu Sans Mono" "Menlo")
     ("Noto Sans" "Helvetica Neue" "Helvetica")))
 '(fancy-compilation-mode t)
 '(fancy-compilation-override-colors nil)
 '(fill-column 80)
 '(find-file-existing-other-name nil)
 '(find-file-suppress-same-file-warnings t)
 '(flycheck-disabled-checkers '(json-jsonlint json-python-json))
 '(flycheck-emacs-lisp-initialize-packages t)
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-global-modes
   '(jsonian-mode js-ts-mode js-jsx-mode js-mode typescript-ts-mode tsx-ts-mode scss-mode css-mode css-ts-mode python-mode python-ts-mode ruby-mode ruby-ts-mode enh-ruby-mode go-mode go-ts-mode rust-mode rust-ts-mode swift-mode scala-mode c-mode c++-mode c-ts-mode c++-ts-mode objc-mode sh-mode sh-base-mode dockerfile-ts-mode))
 '(flycheck-javascript-eslint-executable "eslint_d")
 '(flycheck-pos-tip-max-width 80)
 '(frame-resize-pixelwise t)
 '(garbage-collection-messages t)
 '(gcmh-high-cons-threshold 80000000)
 '(gcmh-idle-delay 'auto)
 '(gcmh-mode t)
 '(global-aggressive-indent-mode t)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-company-mode t)
 '(global-diff-hl-mode t)
 '(global-diff-hl-show-hunk-mouse-mode t)
 '(global-flycheck-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t)
 '(global-move-dup-mode t)
 '(global-origami-mode t)
 '(global-so-long-mode t)
 '(global-syntax-subword-mode t)
 '(global-tree-sitter-mode t)
 '(global-undo-tree-mode t)
 '(global-whitespace-cleanup-mode t)
 '(gnutls-algorithm-priority
   "PFS:-VERS-ALL:+VERS-TLS1.2:%PROFILE_MEDIUM:%SAFE_RENEGOTIATION")
 '(gnutls-crlfiles
   '("/opt/local/etc/grid-security/certificates/*.crl.pem" "/etc/grid-security/certificates/*.crl.pem"))
 '(gnutls-trustfiles
   '("/opt/local/etc/openssl/cert.pem" "/etc/ssl/cert.pem" "/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/etc/certs/ca-certificates.crt"))
 '(gofmt-command "gofumpt")
 '(help-at-pt-display-when-idle
   '(keymap local-map button kbd-help help-echo flymake-diagnostic) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.5)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'top)
 '(history-length 250)
 '(ialign-initial-regexp "(\\s+)")
 '(ialign-pcre-mode t)
 '(ibuffer-elide-long-columns t)
 '(ibuffer-expert t)
 '(imagemagick-render-type 1)
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 524288)
 '(imenu-list-idle-update-delay 0.1)
 '(imenu-list-size 0.2)
 '(imenu-max-items 100)
 '(importmagic-be-quiet t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(isearch-lazy-count t)
 '(ispell-program-name "hunspell")
 '(js-chain-indent t)
 '(js-enabled-frameworks '(javascript))
 '(js-flat-functions t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(jsonian-default-indentation 2)
 '(kept-new-versions 10)
 '(libgit-auto-rebuild t)
 '(linum-delay t)
 '(linum-format "%4d ")
 '(load-prefer-newer t)
 '(lsp-auto-guess-root t)
 '(lsp-clients-typescript-prefer-use-project-ts-server t)
 '(lsp-completion-default-behaviour :insert)
 '(lsp-debounce-full-sync-notifications-interval 0.25)
 '(lsp-disabled-clients
   '(pyls pylsp ruff-lsp eslint ocaml-ls bash-ls html-ls css-ls))
 '(lsp-eldoc-enable-hover nil)
 '(lsp-enable-indentation nil)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-go-library-directories nil)
 '(lsp-go-use-gofumpt t)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-idle-delay 0.25)
 '(lsp-imenu-index-symbol-kinds
   '(Class Method Property Field Constuctor Enum Interface Function Struct))
 '(lsp-imenu-sort-methods '(position))
 '(lsp-jedi-markup-kind-preferred "plaintext")
 '(lsp-jedi-trace-server "off")
 '(lsp-keep-workspace-alive nil)
 '(lsp-metals-server-args '("\"-J-Dmetals.allow-multiline-string-formatting=off\""))
 '(lsp-pyright-multi-root nil)
 '(lsp-response-timeout 60)
 '(lsp-signature-function 'lsp-signature-posframe)
 '(lsp-sourcekit-executable
   "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")
 '(lsp-typescript-surveys-enabled nil)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-imenu-auto-refresh t)
 '(lsp-ui-imenu-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics nil)
 '(mac-emulate-three-button-mouse t)
 '(mac-input-method-mode t)
 '(mac-mouse-wheel-mode t)
 '(mac-print-mode t)
 '(mac-system-move-file-to-trash-use-finder t)
 '(magit-diff-refine-hunk 'all)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(major-mode-remap-alist
   '((python-mode . python-ts-mode)
     (js-mode . typescript-ts-mode)
     (css-mode . css-ts-mode)
     (conf-toml-mode . toml-ts-mode)))
 '(markdown-code-lang-modes
   '(("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-ts-mode)
     ("cpp" . c++-ts-mode)
     ("C++" . c++-ts-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . bash-ts-mode)
     ("javascript" . js-ts-mode)
     ("jsx" . js-ts-mode)
     ("typescript" . typescript-ts-mode)
     ("tsx" . tsx-ts-mode)
     ("html" . web-mode)
     ("css" . css-ts-mode)
     ("python" . python-ts-mode)
     ("go" . go-ts-mode)))
 '(markdown-command "multimarkdown --full --to=html")
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-indent-on-enter nil)
 '(max-lisp-eval-depth 16000)
 '(max-specpdl-size 25000 t)
 '(menu-bar-mode t)
 '(message-kill-buffer-on-exit t)
 '(minibuffer-depth-indicate-mode t)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
 '(mouse-wheel-tilt-scroll t)
 '(multi-term-program "/opt/local/bin/bash")
 '(multi-term-program-switches "--login")
 '(native-comp-always-compile t)
 '(native-comp-async-jobs-number 10)
 '(native-comp-async-report-warnings-errors nil)
 '(ns-alternate-modifier 'alt)
 '(ns-command-modifier 'meta)
 '(ns-pop-up-frames nil)
 '(ns-right-command-modifier 'super)
 '(ns-right-control-modifier 'hyper)
 '(nsm-save-host-names t)
 '(olivetti-body-width 120)
 '(orderless-matching-styles
   '(orderless-regexp orderless-initialism orderless-prefixes orderless-flex))
 '(origami-fold-replacement "▶️")
 '(package-archive-priorities '(("melpa" . 2) ("nongnu" . 1) ("gnu" . 0)))
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-menu-use-current-if-no-marks nil)
 '(package-native-compile t)
 '(package-selected-packages
   '(add-node-modules-path ag aggressive-indent all-the-icons all-the-icons-dired apib-mode arduino-mode auto-compile auto-yasnippet beginend bind-key browse-kill-ring buffer-move bug-hunter buttercup cape capf-autosuggest cargo cask-mode company company-native-complete company-prescient company-quickhelp corfu corfu-prescient corfu-terminal coverage crux csv-mode cycle-quotes dap-mode default-text-scale delight diff-hl dired-collapse dired-hacks-utils dired-hide-dotfiles dired-single direnv docker-compose-mode dotenv-mode editorconfig eglot eldoc elisp-def elisp-refs embark emmet-mode enh-ruby-mode eterm-256color exec-path-from-shell expand-region faceup flycheck flycheck-cask flycheck-golangci-lint flycheck-inline flycheck-package flycheck-plantuml flycheck-projectile flymake focus fontify-face forge gcmh git-commit git-modes git-timemachine go-mode graphql-mode graphviz-dot-mode groovy-mode helpful highlight-indent-guides hl-todo ialign ibuffer-projectile iedit imenu-anywhere imenu-list impostman jq-mode jsonian jsonrpc kurecolor lorem-ipsum lsp-docker lsp-java lsp-jedi lsp-metals lsp-mode lsp-origami lsp-pyright lsp-sourcekit lsp-ui macrostep magit magit-lfs magit-todos marginalia markdown-mode monky move-dup multiple-cursors native-complete nodejs-repl olivetti origami package-build package-lint page-break-lines pager-default-keybindings pcre2el pdf-tools pet plantuml-mode po-mode powerline prettier projectile projectile-rails protobuf-mode python-black python-docstring python-insert-docstring python-isort python-pytest quelpa quelpa-use-package quick-peek rainbow-mode reason-mode reformatter repeat-help request rg ron-mode sass-mode sbt-mode scala-mode scroll-on-jump shift-number shrink-path shut-up smart-semicolon smartparens smooth-scrolling soap-client solarized-theme spaceline sphinx-doc spinner ssh-config-mode string-inflection svg-lib swift-mode terraform-mode ts-comint tuareg udev-mode use-package use-package-ensure-system-package verb verilog-mode vertico vertico-prescient visual-regexp-steroids vterm vundo web-mode wgrep-ag which-key whitespace-cleanup-mode window-purpose yard-mode yarn-mode yasnippet yasnippet-snippets))
 '(page-break-lines-lighter "")
 '(parens-require-spaces nil)
 '(prescient-persist-mode t)
 '(prescient-sort-full-matches-first t)
 '(prettier-enabled-parsers
   '(angular babel babel-flow babel-ts css espree flow graphql json json5 json-stringify less html markdown mdx meriyah scss sh typescript vue yaml))
 '(prettier-inline-errors-flag t)
 '(prettier-pre-warm 'some)
 '(projectile-project-root-functions
   '(projectile-root-local projectile-root-marked projectile-root-top-down projectile-root-bottom-up projectile-root-top-down-recurring))
 '(projectile-project-search-path '("~/Documents/workspace"))
 '(purpose-x-code1-dired-goto-file t)
 '(purpose-x-code1-update-idle-delay 0.5)
 '(purpose-x-popwin-major-modes
   '(help-mode compilation-mode occur-mode helpful-mode comint-mode recentf-dialog-mode))
 '(py-isort-options '("--profile" "black"))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-guess-indent-offset-verbose nil)
 '(python-shell-completion-native-enable nil)
 '(quelpa-upgrade-interval 14)
 '(quelpa-upgrade-p t)
 '(rainbow-html-colors-major-mode-list
   '(html-mode php-mode nxml-mode xml-mode web-mode mhtml-mode js-ts-mode js-jsx-mode tsx-ts-mode))
 '(read-mail-command 'gnus)
 '(reb-re-syntax 'string)
 '(recentf-auto-cleanup 'never)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(report-emacs-bug-no-confirmation t)
 '(report-emacs-bug-no-explanations t)
 '(require-final-newline 'ask)
 '(rg-command-line-flags '("--no-ignore-global" "--threads 8"))
 '(rg-hide-command nil)
 '(rg-keymap-prefix [134217843 114])
 '(ring-bell-function 'ignore)
 '(rst-adjust-hook 'rst-toc-update)
 '(rst-indent-comment 2)
 '(rst-indent-field 2)
 '(rst-indent-literal-normal 2)
 '(rust-format-on-save t)
 '(rxt-global-mode t)
 '(save-place-mode t)
 '(save-place-save-skipped nil)
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(send-mail-function 'smtpmail-send-it)
 '(server-mode t)
 '(server-use-tcp t)
 '(set-mark-command-repeat-pop t)
 '(shell-file-name "/opt/local/bin/bash")
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(show-smartparens-global-mode t)
 '(size-indication-mode t)
 '(smart-semicolon-backspace-commands
   '(backward-delete-char backward-delete-char-untabify delete-backward-char c-electric-backspace sp-backward-delete-char))
 '(smart-semicolon-block-chars '(59 125 44))
 '(smartparens-global-mode t)
 '(smooth-scrolling-mode t)
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-servers-requiring-authorization "smtp\\.gmail\\.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type 'ssl)
 '(so-long-minor-modes
   '(font-lock-mode display-line-numbers-mode flymake-mode flyspell-mode goto-address-mode goto-address-prog-mode hi-lock-mode highlight-changes-mode hl-line-mode linum-mode nlinum-mode prettify-symbols-mode visual-line-mode whitespace-mode diff-hl-amend-mode diff-hl-flydiff-mode diff-hl-mode dtrt-indent-mode flycheck-mode hl-sexp-mode idle-highlight-mode rainbow-delimiters-mode lsp-mode))
 '(so-long-target-modes
   '(css-mode css-ts-mode js-mode js-ts-mode json-mode json-ts-mode))
 '(solarized-use-variable-pitch nil)
 '(sp-hybrid-kill-excessive-whitespace t)
 '(sp-ignore-modes-list '(minibuffer-mode minibuffer-inactive-mode vterm-mode))
 '(sp-navigate-consider-sgml-tags
   '(html-mode web-mode js-jsx-mode nxml-mode js-ts-mode tsx-ts-mode))
 '(spaceline-all-the-icons-clock-always-visible nil)
 '(spaceline-all-the-icons-separator-type 'arrow)
 '(standard-indent 2)
 '(switch-to-buffer-obey-display-actions t)
 '(tab-stop-list '(2 4 8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
 '(tab-width 2)
 '(tabulated-list-gui-sort-indicator-asc 9650)
 '(tabulated-list-gui-sort-indicator-desc 9660)
 '(tabulated-list-tty-sort-indicator-asc 94)
 '(tabulated-list-tty-sort-indicator-desc 118)
 '(temp-buffer-resize-mode t)
 '(tide-completion-ignore-case t)
 '(tide-default-mode "TSX")
 '(timer-max-repeats 1)
 '(tls-checktrust t)
 '(tls-program
   '("openssl s_client -connect %h:%p -min_protocol TLSv1.2 -ign_eof -CAfile %t -nbio -brief" "gnutls-cli --x509cafile %t -p %p --dh-bits=2048 --ocsp --priority='PFS:-VERS-ALL:+VERS-TLS1.2:%PROFILE_MEDIUM:%SAFE_RENEGOTIATION' %h"))
 '(tls-success
   "- Handshake was completed\\|SSL handshake has read \\|CONNECTION ESTABLISHED")
 '(tool-bar-mode nil)
 '(treemacs-change-root-without-asking t)
 '(treemacs-collapse-dirs 3)
 '(treemacs-filewatch-mode t)
 '(treemacs-follow-mode t)
 '(treemacs-git-integration t)
 '(treemacs-no-png-images t)
 '(ts-comint-program-command "ts-node")
 '(typescript-indent-level 2)
 '(undo-limit 16000000)
 '(undo-outer-limit 2400000000)
 '(undo-strong-limit 24000000)
 '(uniquify-buffer-name-style 'reverse nil (uniquify))
 '(use-package-enable-imenu-support t)
 '(use-short-answers t)
 '(user-full-name "Jimmy Yuen Ho Wong")
 '(user-mail-address "wyuenho@gmail.com")
 '(vc-allow-async-revert t)
 '(vc-delete-logbuf-window nil)
 '(vc-follow-symlinks nil)
 '(vc-suppress-confirm t)
 '(vertico-cycle t)
 '(vertico-resize nil)
 '(vr/engine 'pcre2el)
 '(vterm-always-compile-module t)
 '(vterm-max-scrollback 10000)
 '(vundo-roll-back-on-quit nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(web-mode-style-padding 2)
 '(wgrep-auto-save-buffer t)
 '(which-function-mode t)
 '(which-key-idle-secondary-delay 0.0)
 '(which-key-mode t)
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-places t)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(winner-mode t)
 '(words-include-escapes t)
 '(world-clock-list
   '(("America/Los_Angeles" "San Francisco")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Hong_Kong" "Hong Kong")
     ("Asia/Tokyo" "Tokyo")))
 '(x-underline-at-descent-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#839496" :background "#002b36"))))
 '(bold ((t (:weight semi-bold))))
 '(bold-italic ((t (:slant italic :weight semi-bold))))
 '(flycheck-posframe-error-face ((t (:inherit error))))
 '(flycheck-posframe-info-face ((t (:inherit font-lock-function-name-face))))
 '(flycheck-posframe-warning-face ((t (:inherit warning))))
 '(highlight-indent-guides-even-face ((t (:inherit highlight-indent-guides-odd-face))))
 '(highlight-indent-guides-stack-even-face ((t (:inherit highlight-indent-guides-stack-odd-face))))
 '(highlight-indent-guides-top-even-face ((t (:inherit highlight-indent-guides-top-odd-face))))
 '(lsp-headerline-breadcrumb-separator-face ((t (:inherit shadow))))
 '(quick-peek-border-face ((t (:height 1 :strike-through t :extend t :inherit default))))
 '(region ((((type ns)) (:background "selectedTextBackgroundColor" :foreground "selectedTextColor"))))
 '(term-bold ((t (:inherit bold))))
 '(tree-sitter-hl-face:function.call ((t nil)))
 '(tree-sitter-hl-face:method.call ((t nil)))
 '(tree-sitter-hl-face:property ((t nil))))
