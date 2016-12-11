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
  '(wanderlust))

(defun wanderlust/init-wanderlust ()
  (progn
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
      (kbd "C-k") 'wl-summary-prev-line-content
      (kbd "C-j") 'wl-summary-next-line-content
      (kbd "TAB") 'wl-thread-open-close
      (kbd "r") 'wl-summary-reply
      (kbd "R") 'wl-summary-reply-with-citation
      (kbd "w") 'wl-summary-write
      (kbd "f") 'wl-summary-forward
      (kbd "q") 'wl-summary-exit
      (kbd "H") 'wl-summary-jump-to-parent-message
      (kbd "u") 'wl-summary-mark-as-unread)))
