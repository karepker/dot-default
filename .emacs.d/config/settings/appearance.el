(require 'fill-column-indicator)

;; Add ability to use custom themes.
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme/")
;; Third argument turns off emacs warning about loading lisp code.
(load-theme 'blackboard t)

;; Turn on cursor highlighting.
(global-hl-line-mode t)

;; Don't show the toolbar.
(setq tool-bar-mode nil)

;; Turn on relative line numbering.
(linum-mode)
;(linum-relative-global-mode)
;(setq linum-relative-current-symbol "")

;; Set frame properties except font.
(setq default-frame-alist '((font-backend . "xft")
                            (width . 175)
                            (height . 60)))
(setq initial-frame-alist '((width . 175)
                           (height . 60)))

;; Find an appropriate font and use it in frame properties.
(defun frame-font-setup
    (&rest ...)
  (unless (assoc 'font default-frame-alist)
    (let* ((font-family (catch 'break
                          (dolist (font-family
                                   '("Inconsolata-g"
                                     "Inconsolata-g"
                                     "DejaVu Sans Mono"
                                     "Monospace"))
                            (when (member font-family (font-family-list))
                              (throw 'break font-family)))))
           (font (when font-family (format "%s-10" font-family))))
      (when font
        (add-to-list 'default-frame-alist (cons 'font font))
        (set-frame-font font t t)))))
(add-hook 'focus-in-hook #'frame-font-setup)

;; Place a column indicator at column 80.
(setq-default fill-column 80)
(define-globalized-minor-mode global-fci-mode fci-mode turn-on-fci-mode)
(global-fci-mode t)

