;;; packages.el --- wanderlust layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Keith Simmons <keith@the-simmons.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `wanderlust-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `wanderlust/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `wanderlust/pre-init-PACKAGE' and/or
;;   `wanderlust/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst wanderlust-packages
  '(wanderlust (filladapt :location local)))


(defun wanderlust/init-wanderlust ()
  (progn
    (add-hook 'wl-mail-send-pre-hook 'wl-draft-subject-check)
    (add-hook 'wl-mail-send-pre-hook 'wl-draft-attachment-check)

    (setq
      wl-draft-reply-without-argument-list
          '(("Reply-To" ("Reply-To") nil nil)
            ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
            ("From" ("From") nil nil))
      wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
        "^Subject:"
        "^\\(From\\|Reply-To\\):"
        "^Organization:"
        "^\\(Posted\\|Date\\):"
        )
      wl-message-sort-field-list
      '("^From"
        "^Organization:"
        "^X-Attribution:"
        "^Subject"
        "^Date"
        "^To"
        "^Cc")
      wl-stay-folder-window t
      wl-folder-window-width 25
      wl-forward-subject-prefix "Fwd: "
      wl-biff-check-interval 180
      wl-biff-check-delay 10
      wl-biff-use-idle-timer nil
      wl-folder-check-async t
      wl-draft-reply-with-argument-list
          '(("Followup-To" nil nil ("Followup-To"))
            ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
            ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
            ("From" ("From") ("To" "Cc") ("Newsgroups"))))

    (spacemacs/set-leader-keys (kbd "a w") 'wl)
    (evil-define-key 'normal wl-folder-mode-map
      (kbd "RET") 'wl-folder-jump-to-current-entity
      (kbd "M-RET") 'wl-folder-update-recursive-current-entity
      (kbd "w") 'wl-draft
      (kbd "s") 'wl-folder-check-current-entity
      (kbd "S") 'wl-folder-sync-current-entity
      (kbd "K") 'wl-folder-prev-unread
      (kbd "J") 'wl-folder-next-unread
      (kbd "L") 'wl-folder-open-all
      (kbd "H") 'wl-folder-close-all
      (kbd "q") 'wl-exit
      (kbd "c") 'wl-folder-suspend)
    (evil-define-key 'visual wl-folder-mode-map
      (kbd "s") 'wl-folder-check-region
      (kbd "S") 'wl-folder-sync-region)
    (evil-define-key 'normal wl-summary-mode-map
      (kbd "RET") 'wl-summary-read
      (kbd ".") 'wl-summary-redisplay
      (kbd "i") 'wl-summary-mark-as-important
      (kbd "M i") 'wl-summary-target-mark-mark-as-important
      (kbd "C-h") 'wl-summary-exit
      (kbd "C-l") 'wl-summary-jump-to-current-message
      (kbd "C-k") 'wl-summary-prev-line-content
      (kbd "C-j") 'wl-summary-next-line-content
      (kbd "TAB") 'wl-thread-open-close
      (kbd "ss") 'wl-summary-auto-refile
      (kbd "r") 'wl-summary-reply
      (kbd "R") 'wl-summary-reply-with-citation
      (kbd "w") 'wl-summary-write
      (kbd "f") 'wl-summary-forward
      (kbd "F") 'wl-summary-fill-message
      (kbd "q") 'wl-summary-exit
      (kbd "H") 'wl-summary-jump-to-parent-message
      (kbd "u") 'wl-summary-mark-as-unread
      (kbd "M u") 'wl-summary-target-mark-mark-as-unread
      (kbd "m") 'wl-summary-target-mark-line
      (kbd "d") 'wl-summary-dispose
      (kbd "M d") 'wl-summary-target-mark-delete
      (kbd "o") 'wl-summary-refile
      (kbd "x") 'wl-execute-temp-marks)
    (evil-define-key 'visual wl-summary-mode-map
      (kbd "d") 'wl-summary-dispose-region
      (kbd "u") 'wl-summary-mark-as-unread
      (kbd "i") 'wl-summary-mark-as-important
      (kbd "m") 'wl-summary-target-mark-region
      (kbd "o") 'wl-summary-refile-region)))

(defun wanderlust/init-filladapt ()
  (use-package filladapt))

(when (configuration-layer/package-usedp 'bbdb)
  (defun bbdb/init-bbdb-wl ()
    (use-package bbdb-wl
      :config
      (bbdb-wl-setup))))

(when (configuration-layer/package-usedp 'company)
  (when (configuration-layer/package-usedp 'bbdb)
    (defun bbdb/post-init-bbdb ()
      (add-hook 'wl-draft-mode-hook 'company-mode))))
