(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap)
               (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
               (nnmail-expiry-wait 90)))

;; (setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)) )

;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
