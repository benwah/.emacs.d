;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mostly taken from "https://gist.github.com/bitops/77308b347bceb54302a2"                              ;;
;;                                                                                                      ;;
;; use jabber-connect to connect. Make sure you tab to select the hipchat account if it's the           ;;
;; only connection configured.                                                                          ;;
;;                                                                                                      ;;
;; a fork of https://gist.github.com/puffnfresh/4002033                                                 ;;
;;                                                                                                      ;;
;; shout out to http://emacs.stackexchange.com/questions/10940/connecting-to-hipchat-xmpp-via-jabber-el ;;
;; for the help.                                                                                        ;;
;;                                                                                                      ;;
;; you can look up the necessary jabber info on the account info page in hipchat                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ssl-program-name "gnutls-cli"
      ssl-program-arguments '("--insecure" "-p" service host)
      ssl-certificate-verification-policy 1)

(setq jabber-history-enabled t)

(defun hipchat-connect()
  (require 'hipchat_settings)
  (interactive)
  (jabber-connect-all)
  )

(defun hipchat-join ()
  (interactive)
  (hipchat-connect)
  (let* ((room-list (sort (mapcar 'car hipchat-room-list) 'string-lessp))
         (selected-room (completing-read "Room name: " room-list))
         (hipchat-mapping (cdr (assoc selected-room hipchat-room-list))))
    (jabber-groupchat-join
     (jabber-read-account)
     (concat hipchat-mapping "@conf.hipchat.com")
     hipchat-nickname
     t)))

(provide 'hipchat)
