;; CMake mode
;; Add cmake listfile names to the mode list.
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(autoload 'cmake-mode "cmake-mode" "Major mode for editing CMake files" t)
