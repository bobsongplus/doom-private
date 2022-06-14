;;; ../../development/dotfiles/.config/doom/language.el -*- lexical-binding: t; -*-


;; NOTE disable go lsp format, go lsp will mess up the code
(setq-hook! 'go-mode-hook +format-with-lsp nil)
;; using goimports instead of gofmt
(setq! gofmt-command "goimports")

;;  speedups the process of lsp for the first lookup
(setq! lsp-enable-file-watchers nil)
