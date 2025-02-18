;;; ../../development/dotfiles/.config/doom/language.el -*- lexical-binding: t; -*-


;; NOTE disable go lsp format, go lsp will mess up the code
(setq-hook! 'go-mode-hook +format-with-lsp nil)
;; using goimports instead of gofmt
(setq! gofmt-command "goimports")

(add-hook! 'before-save-hook #'lsp-organize-imports)
(add-hook! 'before-save-hook #'lsp-format-buffer)

;;  speedups the process of lsp for the first lookup
(setq! lsp-enable-file-watchers nil)

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2))

(after! eglot
  :config
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))
  )

;; (add-hook! 'prog-mode-hook 'flyspell-mode)
