(defconst frame-move-packages
  '((framemove :location local)))

(defun frame-move/init-framemove ()
  (use-package framemove
    :init
    (setq framemove-hook-into-windmove t)))


