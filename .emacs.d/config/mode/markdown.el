(require 'markdown-mode)

;; Markdown mode and settings. Use GFM to get non-italicized intra-word
;; underscores.
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(setq-default markdown-list-indent-width 2)

;; Turn on spell checking on the fly in markdown.
(add-hook 'gfm-mode-hook
		  (lambda ()
            (flyspell-mode 1)
            (setq evil-shift-width markdown-list-indent-width)
            (setq tab-width markdown-list-indent-width)
            (set (make-local-variable 'whitespace-style) nil)))

;; Extracts title from Jekyll filename.
;; e.g. (jekyll-extract-title-from-filename "2017-07-27-The-Man-from-Earth.md")
;;   ⇒ "The Man from Earth"
(defun jekyll--extract-title-from-file-name (file-name)
  (when
      (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-\\(.+\\)\\.md"
                    file-name)
    (let ((title-with-dashes (match-string 1 file-name)))
      (replace-regexp-in-string "-" " " title-with-dashes nil t nil nil))))

;; Creates formatted YAML front matter for a title given a file name.
;; Returns an empty string when no title is found.
;;
;; e.g. (jekyll-make-title-front-matter-from-file-name
;;       "2017-07-27-The-Man-from-Earth.md")
;;  ⇒ "title: _The Man from Earth_\n"
;;
;; e.g. (jekyll-make-title-front-matter-from-file-name
;;       "2017-07-27-.md")
;;  ⇒ ""
(defun jekyll--make-title-front-matter-from-file-name (file-name)
  (let ((title (jekyll--extract-title-from-file-name file-name)))
	(if (= (length title) 0)
        ""
        (concat "title: _" title "_\n"))))

;; Autoinsert YAML front matter for Jekyll and position cursor at the end.
(define-auto-insert '("/_posts/.*\\.md\\'" . "Jekyll skeleton")
					'("Short description: "
					  "---\n"
					  (jekyll--make-title-front-matter-from-file-name
						(file-name-nondirectory (buffer-file-name)))
                      "---\n\n"
					  _ "\n"))
