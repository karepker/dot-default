(require 'evil)
(require 'evil-args)
(require 'evil-surround)
(require 'evil-tabs)

;; You know what to do.
(evil-mode t)

;; Turn on tabs in emacs.
(global-evil-tabs-mode t)

;; Turn on surround.
(global-evil-surround-mode t)

;; Make vsplits to the right.
(setq-default evil-vsplit-window-right t)

;; Make Y yank to end of the line, i.e. `nnoremap Y y$`.
(setq-default evil-want-Y-yank-to-eol t)

;; Macro for defining custom text objects.
;; Courtesy: https://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp/22418983#22418983
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; Custom text object to select word segments.
(define-and-bind-text-object "v" "[_\s[:upper:]]" "[_\s[:upper:]]")

;; Create a text object for arguments in Evil.
(define-key evil-inner-text-objects-map "," 'evil-inner-arg)
(define-key evil-outer-text-objects-map "," 'evil-outer-arg)

;; Functions to insert newline above/below then return.
(defun insert-newline-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-previous-line))

(defun insert-newline-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-next-line))

;; Courtesy: https://github.com/davvil/.emacs.d/blob/master/init.el
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; As suggested in: https://emacs.stackexchange.com/questions/18299/how-to-define-normal-mode-keychords-in-evil-mode
(with-eval-after-load 'evil-maps
  ;; Shift-Enter inserts newline above, Enter inserts newline below.
  ;; Mappings for terminal: RET, and GUI: <return> (supposedly).
  (define-key evil-normal-state-map (kbd "RET") 'insert-newline-below)
  (define-key evil-normal-state-map (kbd "<return>") 'insert-newline-below)
  (define-key evil-normal-state-map (kbd "S-<return>") 'insert-newline-above)
  (define-key evil-normal-state-map (kbd "S-RET") 'insert-newline-above)
  ;; ESC quits everything!
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))
