;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "song"
      user-mail-address "tinysong1226@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org-notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq doom-load-envvars-file "~/.config/doom.d/.local/env")

;; Ctrl-s active swiper search in the current buffer
(global-set-key (kbd "C-s") #'swiper)

;; lsp-ui-doc mode enable and custome lsp-ui-doc customization
(add-hook! 'lsp-ui-mode-hook #'lsp-ui-doc-mode)
(after! lsp-ui
  (setq!
  lsp-ui-doc-position 'top
  lsp-ui-doc-show-with-mouse t
  lsp-ui-doc-max-height 20
  lsp-ui-doc-max-width 200
  lsp-ui-doc-show-with-cursor t
   )
  )

;; remap localleader key
(after! evil
  (setq! doom-localleader-key ",")
  )

;; fullscreen doom-emacs when start-up
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;
(defun tinysong/insert-chrome-current-tab-title ()
  (interactive)
  (insert (retrieve-chrome-current-table-title))
  )


(defun retrieve-chrome-current-table-title ()
  "Get the tile of chrome first window"
  (interactive)
  (let
      ((title (do-applescript
               (concat
                "set frontmostApplication to path to frontmost application\n"
                "tell application \"Google Chrome\"\n"
                "set theTitle to get title of active tab of first window\n"
                "set theResult to (get theTitle) \n"
                "end tell\n"
                "activate application (frontmostApplication as text)\n"
                "set links to {}\n"
                "copy theResult to the end of links\n"
                "return links as string\n"
                ))))
    (format "%s" title))
  )


(map!
  "C-c y" #'youdao-dictionary-search-at-point-tooltip)


;; custom magit keybinding
(map! :leader
;;; <leader> g --- git/version control
      (:prefix-map ("g" . "git")
       :desc "Magit status"              "s"   #'magit-status
       )
      )

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
