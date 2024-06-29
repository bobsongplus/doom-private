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
(load! "llm")














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






;; (use-package! sis
;;   :hook
;;   ;; enable the /follow context/ and /inline region/ mode for specific buffers
;;   ((text-mode prog-mode) . sis-context-mode)
;;   ((text-mode prog-mode) . sis-inline-mode)

;;   :config

;;   (add-hook! 'evil-insert-state-exit-hook #'sis-set-english)

;;   (setq sis-auto-refresh-seconds 30)
;;   ;; For MacOS
;;   (sis-ism-lazyman-config

;;    ;; English input source may be: "ABC", "US" or another one.
;;    ;; "com.apple.keylayout.ABC"
;;    "com.apple.keylayout.US"

;;    ;; Other language input source: "rime", "sogou" or another one.
;;     "im.rime.inputmethod.Squirrel.Rime"
;;    ;; "com.sogou.inputmethod.sogou.pinyin"
;;    )

;; enable the /cursor color/ mode
;; (sis-global-cursor-color-mode t)
;; enable the /respect/ mode
;; (sis-global-respect-mode t)
;; enable the /follow context/ mode for all buffers
;; (sis-global-context-mode t)
;; enable the /inline english/ mode for all buffers
;; (sis-global-inline-mode t)
;;  )

(advice-add 'ispell-lookup-words :around
            (lambda (orig &rest args)
              (shut-up (apply orig args))))


(setq delete-by-moving-to-trash t)
(when (eq window-system 'mac)
  (setq mac-system-move-file-to-trash-use-finder t))
