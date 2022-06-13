;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "song"
      user-mail-address "tinysong1226@gmail.com"
      )

(setq doom-incremental-load-immediately nil)

(setq doom-load-envvars-file "~/.config/doom.d/.local/env")

(load! "ui")
(load! "tmux")
(load! "evil")
(load! "git")
(load! "tools")
(load! "note")
(load! "language")















;; TODO when enable noter, trigger a new bug, below code cannot excute
;;(after! org (map! :localleader
;;      :map org-mode-map
;;      :desc "Eval Block" "e" 'ober-eval-block-in-repl
;;      (:prefix "o"
;;        :desc "Tags" "t" 'org-set-tags
;;        :desc "Roam Bibtex" "b" 'orb-note-actions
;;        (:prefix ("p" . "Properties")
;;          :desc "Set" "s" 'org-set-property
;;          :desc "Delete" "d" 'org-delete-property
;;          :desc "Actions" "a" 'org-property-action
;;          )
;;        )
;;      (:prefix ("i" . "Insert")
;;       :desc "Link/Image" "l" 'org-insert-link
;;       :desc "Item" "o" 'org-toggle-item
;;       :desc "Citation" "c" 'org-ref-helm-insert-cite-link
;;       :desc "Footnote" "f" 'org-footnote-action
;;       :desc "Table" "t" 'org-table-create-or-convert-from-region
;;       :desc "Screenshot" "s" 'org-download-screenshot
;;       (:prefix ("b" . "Block")
;;        :desc "insert" "b" 'org-insert-src-block
;;        :desc "edit" "e" 'org-edit-src-code
;;        )
;;       (:prefix ("h" . "Headings")
;;        :desc "Normal" "h" 'org-insert-heading
;;        :desc "Todo" "t" 'org-insert-todo-heading
;;        (:prefix ("s" . "Subheadings")
;;         :desc "Normal" "s" 'org-insert-subheading
;;         :desc "Todo" "t" 'org-insert-todo-subheading
;;         )
;;        )
;;       (:prefix ("e" . "Exports")
;;        :desc "Dispatch" "d" 'org-export-dispatch
;;        )
;;       )
;;      )
;;  )







(use-package! sis
  ;; should after doom-theme, or cursor color will not be retained.
  :after (evil tmux-pane doom-theme)
  :config
  (delete "C-h" sis-prefix-override-keys)
  (sis-global-respect-mode t)
  (sis-global-inline-mode t)
  (sis-global-context-mode t)
  (sis-global-cursor-color-mode t))

