(setq user-mail-address "wyuenho@gmail.com"
      user-full-name "Jimmy Yuen Ho Wong")

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
               (nnimap-server-port "imaps")
               (nnimap-stream ssl)
               (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
               (nnmail-expiry-wait immediate)))

(setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)) )

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
