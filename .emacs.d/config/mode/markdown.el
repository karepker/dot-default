(require 'markdown-mode)

;; Markdown mode and settings. Use GFM to get non-italicized intra-word
;; underscores.
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(setq-default markdown-list-indent-width 2)
(setq evil-shift-width markdown-list-indent-width)
(setq tab-width markdown-list-indent-width)

;; Turn on spell checking on the fly in markdown.
(add-hook 'gfm-mode-hook
		  (lambda ()
            (flyspell-mode 1)
            (set (make-local-variable 'whitespace-style) nil)))


