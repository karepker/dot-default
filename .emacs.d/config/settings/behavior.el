;; Turn off the goddamn beeping.
(setq ring-bell-function 'ignore)

;; Turn off clipboard integration for evil.
(setq x-select-enable-clipboard nil)

;; Turn off visual line wrapping
(set-default 'truncate-lines t)

;; Turn off two spaces after a period when filling.
(setq sentence-end-double-space nil)

;; Turn off scrolling the window horizontally.
(setq-default auto-hscroll-mode t)

(custom-set-variables '(inhibit-startup-screen t))

;; Automatically reload files.
(global-auto-revert-mode t)

;; Backup files settings.
(setq make-backup-files nil)

;; Autosave files settings.
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Include underscore as part of a word.
(modify-syntax-entry ?_ "w")

;; Set a default tab width of 4.
(setq-default tab-width 4)
;; No tabs!
(setq-default indent-tabs-mode nil)

