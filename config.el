;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "song"
      user-mail-address "tinysong1226@gmail.com"
      )

(setq doom-incremental-load-immediately nil)

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
 "C-c y" #'youdao-dictionary-search-at-point-tooltip
 "C-c t" #'hl-todo-insert
 )


;; custom magit keybinding
(map! :leader
;;; <leader> g --- git/version control
      (:prefix-map ("g" . "git")
       :desc "Magit status"              "s"   #'magit-status
       :desc "Magit log"              "l"   #'magit-log
       :desc "Magit rebase"              "r"   #'magit-rebase
       :desc "magit commit amend" "a" #'magit-commit-amend
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




(setq
   org_notes (concat (getenv "HOME") "/Documents/org-roam")
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory org_notes
   )

;; ;; enhancement org-noter
;; ;; HACK: more informatin org-noter: https://github.com/weirdNox/org-noter
(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_notes)
   )
  )

;; (use-package! websocket
;;     :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; ;; I like short names
;; (general-evil-setup t)
;; ;; Stop telling me things begin with non-prefix keys
;; (general-auto-unbind-keys)


;; ;; TODO: org-protocol
;; ;; TODO: crawls content send to emacs by org-protocol
;; ;; TODO: optimize org-capture
(after! org-capture
  ;; Firefox and Chrome
  (add-to-list 'org-capture-templates
               '("P" "Protocol" entry   ; key, name, type
                 (file+headline +org-capture-notes-file "Inbox") ; target
                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                 :prepend t             ; properties
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
                 :prepend t
                 :kill-buffer t))
  )

(map! :localleader
      :map markdown-mode-map
      :prefix ("i" . "Insert")
      :desc "Blockquote"    "q" 'markdown-insert-blockquote
      :desc "Bold"          "b" 'markdown-insert-bold
      :desc "Code"          "c" 'markdown-insert-code
      :desc "Emphasis"      "e" 'markdown-insert-italic
      :desc "Footnote"      "f" 'markdown-insert-footnote
      :desc "Code Block"    "s" 'markdown-insert-gfm-code-block
      :desc "Image"         "i" 'markdown-insert-image
      :desc "Link"          "l" 'markdown-insert-link
      :desc "List Item"     "n" 'markdown-insert-list-item
      :desc "Pre"           "p" 'markdown-insert-pre
      (:prefix ("h" . "Headings")
        :desc "One"   "1" 'markdown-insert-atx-1
        :desc "Two"   "2" 'markdown-insert-atx-2
        :desc "Three" "3" 'markdown-insert-atx-3
        :desc "Four"  "4" 'markdown-insert-atx-4
        :desc "Five"  "5" 'markdown-insert-atx-5
        :desc "Six"   "6" 'markdown-insert-atx-6))


;; ;; These bindings should probably be after org-noter is loaded.
(map! :localleader
      :map (org-mode-map pdf-view-mode-map)
      (:prefix ("o" . "Org")
       (:prefix ("n" . "Noter")
        :desc "Noter" "n" 'org-noter
        )))

(after! org (map! :localleader
      :map org-mode-map
      :desc "Eval Block" "e" 'ober-eval-block-in-repl
      (:prefix "o"
        :desc "Tags" "t" 'org-set-tags
        :desc "Roam Bibtex" "b" 'orb-note-actions
        (:prefix ("p" . "Properties")
          :desc "Set" "s" 'org-set-property
          :desc "Delete" "d" 'org-delete-property
          :desc "Actions" "a" 'org-property-action
          )
        )
      (:prefix ("i" . "Insert")
       :desc "Link/Image" "l" 'org-insert-link
       :desc "Item" "o" 'org-toggle-item
       :desc "Citation" "c" 'org-ref-helm-insert-cite-link
       :desc "Footnote" "f" 'org-footnote-action
       :desc "Table" "t" 'org-table-create-or-convert-from-region
       :desc "Screenshot" "s" 'org-download-screenshot
       (:prefix ("b" . "Block")
        :desc "insert" "b" 'org-insert-src-block
        :desc "edit" "e" 'org-edit-src-code
        )
       (:prefix ("h" . "Headings")
        :desc "Normal" "h" 'org-insert-heading
        :desc "Todo" "t" 'org-insert-todo-heading
        (:prefix ("s" . "Subheadings")
         :desc "Normal" "s" 'org-insert-subheading
         :desc "Todo" "t" 'org-insert-todo-subheading
         )
        )
       (:prefix ("e" . "Exports")
        :desc "Dispatch" "d" 'org-export-dispatch
        )
       )
      )
  )

(after! pdf-view
 ;; open pdfs scaled to fit page
 (setq-default pdf-view-display-size 'fit-width)
 (add-hook! 'pdf-view-mode-hook (evil-colemak-basics-mode -1))
 ;; automatically annotate highlights
 (setq pdf-annot-activate-created-annotations t
       pdf-view-resize-factor 1.1)
  ;; faster motion
(map!
  :map pdf-view-mode-map
  :n "g g"          #'pdf-view-first-page
  :n "G"            #'pdf-view-last-page
  :n "N"            #'pdf-view-next-page-command
  :n "E"            #'pdf-view-previous-page-command
  :n "e"            #'evil-collection-pdf-view-previous-line-or-previous-page
  :n "n"            #'evil-collection-pdf-view-next-line-or-next-page
  :localleader
  (:prefix "o"
   (:prefix "n"
    :desc "Insert" "i" 'org-noter-insert-note
    ))
))

;; I like having the date on my TODO items.
(setq org-log-done "time"
      org-log-done-with-time 't)

;; This controls what is used to open links in org documents.
;; Since there are only a few defaults defined, I am just prepending them to my
;; changes instead of dealing with append and stuff.
(setq org-file-apps
  '((auto-mode . emacs)
    ("\\.mm\\'" . default)
    ("\\.x?html?\\'" . default)
    ("\\.pdf\\'" . default)
    ("\\.png\\'" . viewnior)
    ("\\.jpg\\'" . viewnior)
    ("\\.svg\\'" . viewnior)
    ))

;; ;; enable word-wrap in C/C++/ObjC/Java
(add-hook! 'markdown-mode-hook #'+word-wrap-mode)
(add-hook! 'text-mode-hook #'+word-wrap-mode)
(add-hook! 'tex-mode-hook #'+word-wrap-mode)


;; TODO: org hugo setting
(setq org-hugo-auto-set-lastmod 't
      org-hugo-section "posts"
      org-hugo-suppress-lastmod-period 43200.0
      org-hugo-export-creator-string "Emacs 28.0.50 (Org mode 9.5 + ox-hugo + song)"
      )

;; view pdf  in dark-mode
(use-package pdf-view
  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  :hook (pdf-tools-enabled . hide-mode-line-mode)
  :config
  (setq pdf-view-midnight-colors '("#ABB2BF" . "#282C35")))

;; ;; Actually start using templates
(after! org-capture
  ;; Firefox
  (add-to-list 'org-capture-templates
               '("P" "Protocol" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                 :prepend t
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
                 :prepend t
                 :kill-buffer t))
  ;; Misc
  (add-to-list 'org-capture-templates
               '("a"                                         ; key
                 "Article"                                   ; name
                 entry                                       ; type
                 (file+headline +org-capture-notes-file "Article") ; target
                 "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?" ; template
                 :prepend t             ; properties
                 :empty-lines 1         ; properties
                 :created t             ; properties
                 ))
  )
;; (setq org-roam-ref-capture-templates
;;         '(("r" "ref" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "websites/${slug}"
;;            :head "#+SETUPFILE:./hugo_setup.org
;; #+ROAM_KEY: ${ref}
;; #+HUGO_SLUG: ${slug}
;; #+TITLE: ${title}")
;; ;; TODO: org brain
;; ;; (after! org
;; ;;   <<org-conf>>
;; ;; )

;; ;; http://wenshanren.org/?p=327
;; ;; change it to helm
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "yaml" "go")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;; ;; using goimports instead gofmt
;; (setq! gofmt-command "gofmt")
;; ;; REVIEW org capture https://mediaonfire.com/blog/2017_07_21_org_protocol_firefox.html
