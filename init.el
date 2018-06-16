(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   `(
     csharp
     vimscript
     csv
     clojure
     yaml
     autohotkey
     typescript
     c-c++
     csharp
     pdf
     markdown
     javascript
     rust
     ruby
     html
     windows-scripts
     racket
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
     ;;personal
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t))
   dotspacemacs-additional-packages '(dtrt-indent clipmon)
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
   dotspacemacs-use-spacelpa nil
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
  (setq
   calendar-location-name "Seattle, WA"
   calendar-latitude 47.667998
   calendar-longitude -122.321062
   powershell-prompt-regex "(?>(?>\\S| )*\\n)+([A-Z]:\\\\(?>(?>\\w| )+\\\\)*(?>\\w| )+(?> \\[(?>\\S| )*\\])?)> "

   default-truncate-lines t
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

   ispell-program-name "aspell"

   helm-ag-base-command "pt -e --nocolor --nogroup"
   omnisharp-server-executable-path "C:\\dev\\Tools\\omnisharp\\OmniSharp.exe"

   flycheck-check-syntax-automatically '())
  (load "evil-surround.el")
  (load "helm.el")

  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

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
    "df" 'keith/delete-frame
    "db" 'evil-delete-buffer
    "dw" 'delete-window
    "fm" 'spacemacs/toggle-maximize-on
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
  (add-hook 'rust-mode-hook 'keith/disable-racer-eldoc)

  (if (eq system-type 'gnu/linux) (setq exec-path-from-shell-check-startup-files nil))
  (autoload-do-load 'org-mode)
  (autoload-do-load 'emacs-lisp-mode)
  (require 'yasnippet)
  (yas/reload-all t))

(defun keith/disable-racer-eldoc ()
  (setq eldoc-documentation-function nil))

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
 '(package-selected-packages
   '(ws-butler winum volatile-highlights vi-tilde-fringe uuidgen toc-org symon string-inflection spaceline-all-the-icons all-the-icons memoize spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode password-generator paradox spinner overseer org-bullets open-junk-file neotree nameless move-text macrostep lorem-ipsum link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-xref helm-themes helm-swoop helm-purpose window-purpose imenu-list helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-cleverparens smartparens paredit evil-args evil-anzu anzu eval-sexp-fu highlight elisp-slime-nav editorconfig dumb-jump f dash s define-word counsel-projectile projectile counsel swiper ivy pkg-info epl column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-compile packed aggressive-indent ace-window ace-link ace-jump-helm-line helm avy helm-core popup which-key use-package pcre2el org-plus-contrib hydra font-lock+ evil goto-chg undo-tree diminish bind-map bind-key async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
