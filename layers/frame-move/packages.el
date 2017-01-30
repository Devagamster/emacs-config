(defconst frame-move-packages
  '((frame-move :location local)))

(defun frame-move/init-frame-move ()
  (use-package frame-move
    :init
    (setq framemove-hook-into-windmove t)))


