;; Custom c++ style
(c-add-style "karepker"
	     '("karepker"
	       (indent-tabs-mode . t)  ; Use tabs for indentation.
	       (c-basic-offset . 4)))  ; Tab size of 4.

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "karepker")))
