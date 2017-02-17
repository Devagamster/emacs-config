(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   `(c-c++
     markdown
     javascript
     ,(if (eq system-type 'gnu/linux) 'ocaml)
     ,(if (eq system-type 'gnu/linux) 'coq)
     rust
     bbdb
     wanderlust
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
     spell-checking
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
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
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
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
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

(defun dotspacemacs/user-init ())

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
    "wy" 'split-window-right
    "wi" 'split-window-below
    "wu" 'split-window-below-and-focus
    "wo" 'split-window-right-and-focus
    "fn" 'new-frame
    "kf" 'delete-frame
    "kw" 'delete-window
    "fm" 'spacemacs/toggle-maximize-on
    "s SPC" 'hydra-scrolling/body)
  (setq
   mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
   mouse-wheel-progressive-speed nil

   mu4e-maildir "~/Mail"

   mu4e-context-policy 'pick-first
   mu4e-compose-context-policy nil

   backup-directory-alist
   `((".*" . ,(if (eq system-type 'gnu/linux) "/mnt/c/dev/Temp/" "c:/dev/Temp")))
   auto-save-file-name-transforms
   `((".*" ,(if (eq system-type 'gnu/linux) "/mnt/c/dev/Temp/" "c:/dev/Temp") t))

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

  (if (eq system-type 'gnu/linux) (setq exec-path-from-shell-check-startup-files nil)))

(defun keith/custom-agenda (&optional arg)
  (interactive "P")
  (org-agenda arg "a"))

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
 '(package-selected-packages
   (quote
    (winum semi flim apel powerline faceup pcre2el alert log4e gntp hydra parent-mode helm-purpose window-purpose imenu-list projectile request haml-mode gitignore-mode fuzzy flyspell-correct pos-tip flycheck pkg-info epl flx magit magit-popup git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree highlight f diminish web-completion-data s company rust-mode inf-ruby bind-map bind-key yasnippet packed dash helm avy helm-core async auto-complete popup disaster company-c-headers cmake-mode clang-format mmm-mode markdown-toc markdown-mode gh-md mu4e-maildirs-extension mu4e-alert ht sublimity web-beautify utop tuareg caml ocp-indent merlin livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern company-coq company-math math-symbol-lists coffee-mode ws-butler window-numbering which-key web-mode wanderlust volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rake rainbow-delimiters racket-mode racer quelpa pug-mode powershell popwin persp-mode paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file neotree move-text minitest magit-gitflow macrostep lorem-ipsum linum-relative link-hint less-css-mode info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link flyspell-correct-helm flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dumb-jump dtrt-indent define-word company-web company-statistics company-auctex column-enforce-mode clean-aindent-mode chruby cargo bundler bbdb auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
