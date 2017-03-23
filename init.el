(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   `(
     autohotkey
     typescript
     c-c++
     markdown
     javascript
     ,(if (eq system-type 'gnu/linux) 'ocaml)
     ,(if (eq system-type 'gnu/linux) 'coq)
     ;; rust
     bbdb
     ;; wanderlust
     ruby
     html
     windows-scripts
     racket
     helm
     auto-completion
     emacs-lisp
     git
     org
     (mu4e :variables
           mu4e-installation-path "~/mu4e/")
     latex
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     frame-move
     personal)
   dotspacemacs-additional-packages '(dtrt-indent)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 10))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font `("Source Code Pro"
                               :size ,(if (file-exists-p "~/.highdpi") 25 12)
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
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
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("pt")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil))

(defun dotspacemacs/user-init ()
  (setq custom-file "~/.spacemacs.d/custom.el"))

(defun dotspacemacs/user-config ()
  (server-start)
  (evil-leader/set-key
    "xi" 'dtrt-indent-adapt
    "SPC" 'helm-M-x)
  (defhydra hydra-scrolling ()
    "scrolling"
    ("j" (evil-scroll-down 10) "scroll 10 down")
    ("k" (evil-scroll-up 10) "scroll 10 up")
    ("K" (evil-scroll-down 50) "scroll 50 up")
    ("J" (evil-scroll-up 50) "scroll 50 down")
    ("l" (evil-scroll-page-down 1) "scroll page down")
    ("h" (evil-scroll-page-up 1) "scroll page up")
    ("L" (evil-scroll-page-down 2) "scroll 2 pages down")
    ("H" (evil-scroll-page-up 2) "scroll 2 pages up"))
  (evil-leader/set-key
    "oa" 'keith/custom-agenda
    "wl" 'keith/window-right
    "wk" 'keith/window-up
    "wj" 'keith/window-down
    "wh" 'keith/window-left
    "wo" 'keith/split-window-right
    "wi" 'keith/split-window-up
    "wu" 'keith/split-window-down
    "wy" 'keith/split-window-left
    "fn" 'new-frame
    "kf" 'delete-frame
    "kw" 'delete-window
    "fm" 'spacemacs/toggle-maximize-on
    "s SPC" 'hydra-scrolling/body)
  (setq
   default-truncate-lines t

   mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
   mouse-wheel-progressive-speed nil

   mu4e-maildir "~/Mail"

   mu4e-context-policy 'pick-first
   mu4e-compose-context-policy nil

   auto-save-default nil
   backup-inhibited t

   ispell-program-name "aspell"

   org-agenda-skip-deadline-prewarning-if-scheduled t
   org-agenda-skip-scheduled-if-done t

   org-agenda-files (if (eq system-type 'gnu/linux) (quote ("/mnt/c/dev/Projects/Logs/todo.org")) (quote ("c:/dev/Projects/Logs/todo.org")))
   org-agenda-restore-windows-after-quit t
   org-agenda-skip-deadline-prewarning-if-scheduled t
   org-agenda-window-setup (quote current-window)

   org-agenda-custom-commands
   '(("a" "Weekly agenda"
      ((agenda "" ((org-agenda-ndays 7)))
       (tags-todo "EFFORT={.+}+(THISWEEK|URGENT)"
                  ((org-agenda-sorting-strategy '(effort-up))))
       (tags-todo "EFFORT<>{.+}+THISWEEK")))))

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

  (if (eq system-type 'gnu/linux) (setq exec-path-from-shell-check-startup-files nil)))

(defun keith/custom-agenda (&optional arg)
  (interactive "P")
  (org-agenda arg "a"))

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
