(defconst personal-packages
  '(gdb-mi theme-changer))

(defun personal/post-init-gdb-mi ()
  (with-eval-after-load "gdb-mi"
    (message "gdb-mi evilify")
    (evilified-state-evilify-map gdb-breakpoints-mode-map ;; Add delete function
      :mode gdb-breakpoints-mode)
    (evilified-state-evilify-map gdb-locals-mode-map
      :mode gdb-locals-mode)
    (evilified-state-evilify-map gdb-registers-mode-map
      :mode gdb-registers-mode-map)
    (evilified-state-evilify-map gdb-frames-mode-map
      :mode gdb-frames-mode)
    (evilified-state-evilify-map gdb-disassembly-mode-map
      :mode gdb-disassembly-mode)
    (evilified-state-evilify-map gdb-memory-mode-map
      :mode gdb-memory-mode)))

(defun geolocation/init-theme-changer ()
  "Initialize theme-changer"
  (use-package theme-changer
    :config
    (progn
      (when (> (length dotspacemacs-themes) 1)
        (change-theme (nth 0 dotspacemacs-themes)
                      (nth 1 dotspacemacs-themes))))))
