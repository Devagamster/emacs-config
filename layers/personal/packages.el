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

(defun personal/post-init-evil-snipe ()
  (push '(?\[ "[[{(]") evil-snipe-aliases)
  (push '(?\] "[]})]") evil-snipe-aliases))

(defun personal/init-theme-changer ()
  "Initialize theme-changer"
  (use-package theme-changer
    :config
    (progn
      (when (> (length dotspacemacs-themes) 1)
        (change-theme (nth 0 dotspacemacs-themes)
                      (nth 1 dotspacemacs-themes))))))

(defun my-correct-symbol-bounds (pretty-alist)
  "Prepend a TAB character to each symbol in this alist,
this way compose-region called by prettify-symbols-mode
will use the correct width of the symbols
instead of the width measured by char-width."
  (mapcar (lambda (el)
            (setcdr el (string ?\t (cdr el)))
            el)
          pretty-alist))

(defun my-ligature-list (ligatures codepoint-start)
  "Create an alist of strings to replace with
codepoints starting from codepoint-start."
  (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
    (-zip-pair ligatures codepoints)))

; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
(setq my-fira-code-ligatures
      (let* ((ligs '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                     "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
                     "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
                     "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
                     ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
                     "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
                     "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
                     "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
                     ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                     "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
                     "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
                     "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
                     "x" ":" "+" "+" "*")))
        (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

;; nice glyphs for haskell with hasklig
(defun my-set-fira-code-ligatures ()
  "Add hasklig ligatures for use with prettify-symbols-mode."
  (setq prettify-symbols-alist
        (append my-fira-code-ligatures prettify-symbols-alist))
  (prettify-symbols-mode))

(add-hook 'prog-mode-hook 'my-set-fira-code-ligatures)
