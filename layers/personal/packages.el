(defconst personal-packages
  '(gdb-mi mu4e))

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

(defun personal/post-init-mu4e ()
  (with-eval-after-load "mu4e"
    (message "mu4e contexts")
    (setq
     mu4e-contexts
     `(,(make-mu4e-context
         :name "Home"
         :enter-func (lambda () (mu4e-message "Entering Home Context"))
         :leave-func (lambda () (mu4e-message "Leaving Home Context"))
         :match-func (lambda (msg)
                       (when msg
                         (mu4e-message-contact-field-matches msg
                                                             :to "keith@the-simmons.net")))
         :vars '( ( user-mail-address . "keith@the-simmons.net")
                  (user-full-name     . "Keith Simmons")
                  ( mu4e-sent-folder  . "/Simmons/[Gmail].Sent Mail" )
                  ( mu4e-drafts-folder  . "/Simmons/[Gmail].Drafts" )
                  ( mu4e-trash-folder  . "/Simmons/[Gmail].Trash" )))
       ,(make-mu4e-context
         :name "School"
         :enter-func (lambda () (mu4e-message "Entering School Context"))
         :leave-func (lambda () (mu4e-message "Leaving School Context"))
         :match-func (lambda (msg)
                       (when msg
                         (mu4e-message-contact-field-matches msg
                                                             :to "keithsim@uw.edu")))
         :vars '( ( user-mail-address . "keithsim@uw.edu" )
                  ( user-full-name    . "Jonathan (Keith) Simmons" )
                  ( mu4e-sent-folder  . "/School/[Gmail].Sent Mail" )
                  ( mu4e-drafts-folder  . "/School/[Gmail].Drafts" )
                  ( mu4e-trash-folder  . "/School/[Gmail].Trash" )))))))
