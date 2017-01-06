;;; packages.el --- bbdb layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <keith@DESKTOP-1PIKNT7>
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
;; added to `bbdb-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bbdb/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bbdb/pre-init-PACKAGE' and/or
;;   `bbdb/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bbdb-packages
  '(bbdb))

(defun bbdb/init-bbdb ()
  (use-package bbdb
    :config
    (bbdb-initialize)
    (setq
     bbdb-offer-save 1
     bbdb-use-pop-up t
     bbdb-electric-p t
     bbdb-popup-target-lines 1
     bbdb-dwim-net-address-allow-redundancy t
     bbdb-quiet-about-name-mismatches 2
     bbdb-always-add-address t
     bbdb-canonicalize-redundant-nets-p t
     bbdb-completion-type nil
     bbdb-complete-name-allow-cycling t
     bbbd-message-caching-enabled t
     bbdb-use-alternate-names t
     bbdb-elided-display t
     bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook)))
