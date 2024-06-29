;;; ../../development/dotfiles/.config/doom/ui.el -*- lexical-binding: t; -*-

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.

(setq doom-unicode-font (font-spec :family "Fira Mono"))
(setq doom-font (font-spec :size 15 ))

;; (setq doom-font (font-spec :size 13))
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-palenight)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-material)
;; (setq doom-theme 'doom-molokai)
(setq doom-theme 'doom-nord)

;; modus-vivendi
;; doom-challenger-deep
;; doom-gruvbox

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; fix lsp-ui-doc frame issue: https://github.com/emacs-lsp/lsp-ui/issues/464
(setq focus-follows-mouse t)


;; fullscreen doom-emacs when start-up
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; lsp-ui-doc mode enable and custome lsp-ui-doc customization
(add-hook! 'lsp-ui-mode-hook #'lsp-ui-doc-mode)
(after! lsp-ui
  (setq!
   lsp-ui-doc-position 'top
   lsp-ui-doc-show-with-mouse t
   lsp-ui-doc-max-height 20
   lsp-ui-doc-max-width 200
   lsp-ui-doc-show-with-cursor t
   lsp-ui-peek-always-show t
   lsp-ui-sideline-show-hover t
   )
  )

;; ;; info color: This makes manual pages nicer to look at by adding variable pitch
;; ;; fontification and colouring
(use-package! info-colors
  :commands (info-colors-fontify-node))

;; (add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; ;; witter’s emojis look nicer than emoji-one. Other than that, this is pretty great OOTB 😀.
(setq emojify-emoji-set "twemoji-v2")

;; ;; show kebingding in the mode line, call by keycast-mode
(use-package! keycast
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
      :height 0.9)
    '(keycast-key :inherit custom-modified
      :height 1.1
      :weight bold))
  (keycast-mode))

;; language setting
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-persp-name t)

;; Better Default
(setq-default
 delete-by-moving-to-trash t            ; Delete files to trash
 window-combination-resize t ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(display-time-mode 1)                             ; Enable time in the mode-line

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

(global-subword-mode 1)                           ; Iterate through CamelCase words

;; Frame sizing
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; 隐藏 title bar
(setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
