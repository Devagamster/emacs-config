(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   `(csv
     shaders
     yaml
     autohotkey
     typescript
     c-c++
     csharp
     markdown
     javascript
     ess
     ,(if (eq system-type 'gnu/linux) 'ocaml)
     ,(if (eq system-type 'gnu/linux) 'coq)
     ess
     rust
     ruby
     html
     windows-scripts
     racket
     helm
     auto-completion
     emacs-lisp
     git
     org
     latex
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     frame-move
     personal
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t))
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
   dotspacemacs-themes '(solarized-light solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font `("Source Code Pro"
                               :size ,(if (file-exists-p "~/.highdpi") 25 13)
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
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
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
  (setq custom-file "~/.spacemacs.d/custom.el"
        calendar-location-name "Seattle, WA"
        calendar-latitude 47.667998
        calendar-longitude -122.321062))

(defun dotspacemacs/user-config ()
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
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
  (define-key evil-normal-state-map (kbd "<escape>") 'spacemacs/evil-search-clear-highlight)

  (setq
   default-truncate-lines t
   typescript-auto-indent-flag nil

   mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
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

(defun keith/escape ()
  (interactive)
  (evil-escape)
  (spacemacs/evil-search-clear-highlight))

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(golden-ratio-extra-commands
   (quote
    (winum-select-window-9 winum-select-window-8 winum-select-window-7 winum-select-window-6 winum-select-window-5 winum-select-window-4 winum-select-window-3 winum-select-window-2 winum-select-window-1 winum-select-window-0-or-10 quit-window evil-window-move-very-bottom evil-window-move-far-right evil-window-move-far-left evil-window-move-very-top evil-window-rotate-downwards evil-window-rotate-upwards evil-window-vnew evil-window-new evil-window-prev evil-window-next evil-window-mru evil-window-top-left evil-window-bottom-right evil-window-down evil-window-up evil-window-right evil-window-left evil-window-vsplit evil-window-split evil-window-delete evil-avy-goto-line evil-avy-goto-word-or-subword-1 buf-move-down buf-move-up buf-move-right buf-move-left avy-pop-mark ace-maximize-window ace-swap-window ace-select-window ace-delete-window ace-window windmove-left windmove-right windmove-down windmove-up keith/split-window-right keith/split-window-up keith/split-window-down keith/split-window-left keith/window-right keith/window-up keith/window-down keith/window-left)))
 '(package-selected-packages
   (quote
    (tide typescript-mode mmm-mode markdown-toc markdown-mode gh-md disaster company-c-headers cmake-mode clang-format ws-butler window-numbering which-key web-mode web-beautify wanderlust volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rake rainbow-delimiters racket-mode racer quelpa pug-mode powershell popwin persp-mode paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file neotree mu4e-maildirs-extension mu4e-alert move-text minitest magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link flyspell-correct-helm flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dumb-jump dtrt-indent define-word company-web company-tern company-statistics company-auctex column-enforce-mode coffee-mode clean-aindent-mode chruby cargo bundler bracketed-paste bbdb auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
