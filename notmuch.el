(setq sendmail-program "/opt/homebrew/bin/msmtp"
      message-sendmail-f-is-evil t
      message-sendmail-envelope-from 'header
      message-sendmail-extra-arguments '("--read-envelope-from")
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail)


(use-package notmuch
  :ensure t)

(define-key notmuch-show-mode-map "D"
  (lambda ()
    "Mark Message as Trash"
    (interactive)
    (notmuch-show-tag (list "+trash" "-inbox"))))

(define-key notmuch-show-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-deleted"))
      (notmuch-show-tag (list "+deleted")))))

