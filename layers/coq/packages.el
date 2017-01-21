;;; packages.el --- coq layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <keith@the-simmons.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst coq-packages
  '(
    (proof-general :location "~/.spacemacs.d/layers/coq/proof-general")
    company-coq
    ))

(defun coq/init-proof-general ()
  "Initialize Proof General"
  (use-package proof-site
    :defer t
    :mode ("\\.v\\'" . coq-mode)
    :load-path "~/.spacemacs.d/layers/coq/proof-general/generic"))

(defun coq/init-company-coq ()
  (use-package company-coq
    :defer t
    :config
    (add-hook 'coq-mode-hook #'company-coq-initialize)))

;;; packages.el ends here
