(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   `(graphviz
     csharp
     vimscript
     csv
     yaml
     autohotkey
     coffeescript
     typescript
     csharp
     yaml
     pdf
     markdown
     javascript
     rust
     html
     windows-scripts
     helm
     auto-completion
     emacs-lisp
     git
     version-control
     org
     latex
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     frame-move
     personal
     evil-mini
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t))
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-enable-emacs-pdumper t
   dotspacemacs-emacs-pdumper-executable-file "c:/dev/Tools/emacs/bin/emacs.exe"
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-use-spacelpa nil
   dotspacemacs-gc-cons '(100000000 0.1)
   dotspacemacs-verify-spacelpa-archives nil
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 10))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-initial-scratch-message nil
   dotspacemacs-themes '(spacemacs-dark)
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font `("Fira Code"
                               :size ,(if (file-exists-p "~/.highdpi") 25 13)
                               :weight light
                               :width normal
                               :powerline-scale 1.5)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-lay
   dotspacemacs-auto-generate-layout-names nilouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server t
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("rg")
   dotspacemacs-frame-title-format "%b"
   dotspacemacs-icon-title-format nil
   dotspacemacs-zone-out-when-idle t
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-load ()
  (spacemacs|when-dumping
    (dolist (d (directory-files package-user-dir t nil 'nosort))
      (unless (or (string-equal ".." (substring d -2))
                  (string-equal "." (substring d -1))
                  (not (file-directory-p d)))
        (message "%s" d)
        (dolist (f (directory-files d t "\\.el$" 'nosort))
          (unless (string-match-p ".*pkg\\.el$" f)
            (message "%s" f)
            (ignore-errors (load f t)))))))
  (spacemacs|when-dumping
    (yas-reload-all t))

  (setq
   typescript-auto-indent-flag nil

   mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil))
   mouse-wheel-progressive-speed nil

   backup-directory-alist `(("." . "c:/dev/Temp/"))
   auto-save-list-file-prefix "c:/dev/Temp/.auto-saves-"
   auto-save-file-name-transforms `((".*" "c:/dev/Temp/" t))
   create-lockfiles nil

   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   js-indent-level 2
   typescript-indent-level 2
   powershell-indent 2

   ispell-program-name "hunspell")


  (define-key evil-normal-state-map (kbd "<escape>") 'spacemacs/evil-search-clear-highlight)
  (defhydra hydra-scrolling ()
    "scrolling"
    ("j" (evil-scroll-down 10) "scroll 10 down")
    ("k" (evil-scroll-up 10) "scroll 10 up")
    ("J" (evil-scroll-down 50) "scroll 50 down")
    ("K" (evil-scroll-up 50) "scroll 50 up")
    ("l" (evil-scroll-page-down 1) "scroll page down")
    ("h" (evil-scroll-page-up 1) "scroll page up")
    ("L" (evil-scroll-page-down 2) "scroll 2 pages down")
    ("H" (evil-scroll-page-up 2) "scroll 2 pages up"))

  (evil-leader/set-key
    "wl" 'keith/window-right
    "wk" 'keith/window-up
    "wj" 'keith/window-down
    "wh" 'keith/window-left
    "wo" 'keith/split-window-right
    "wi" 'keith/split-window-up
    "wu" 'keith/split-window-down
    "wy" 'keith/split-window-left
    "fn" 'new-frame
    "df" 'keith/delete-frame
    "db" 'evil-delete-buffer
    "dw" 'delete-window
    "s SPC" 'hydra-scrolling/body)

  (with-eval-after-load "golden-ratio"
    (setq golden-ratio-extra-commands
          (append golden-ratio-extra-commands
                  '(keith/split-window-right
                    keith/split-window-up
                    keith/split-window-down
                    keith/split-window-left
                    keith/window-right
                    keith/window-up
                    keith/window-down
                    keith/window-left))))

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'text-mode-hook 'spacemacs/toggle-truncate-lines-on)
  (spacemacs-buffer/goto-buffer))

(defun dotspacemacs/user-config ()
  (server-start))

(defun keith/split-window-right ()
  (interactive)
  (evil-beginning-of-line)
  (split-window-right-and-focus))
(defun keith/split-window-up ()
  (interactive)
  (evil-beginning-of-line)
  (split-window-below))
(defun keith/split-window-down ()
  (interactive)
  (evil-beginning-of-line)
  (split-window-below-and-focus))
(defun keith/split-window-left ()
  (interactive)
  (evil-beginning-of-line)
  (split-window-right))

(defun keith/window-right ()
  (interactive)
  (evil-beginning-of-line)
  (evil-window-right 1))
(defun keith/window-left ()
  (interactive)
  (evil-beginning-of-line)
  (evil-window-left 1))
(defun keith/window-up ()
  (interactive)
  (evil-beginning-of-line)
  (evil-window-up 1))
(defun keith/window-down ()
  (interactive)
  (evil-beginning-of-line)
  (evil-window-down 1))

(defun keith/delete-frame ()
  (interactive)
  "Delete the selected frame. If the last one, kill-terminal"
  (condition-case nil (delete-frame) (error (save-buffers-kill-terminal))))

(setq custom-file "~/.spacemacs.d/custom.el")
(load custom-file)
