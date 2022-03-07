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

(setq doom-unicode-font (font-spec :family "Fira Mono"))

;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

;; modus-vivendi
;; doom-challenger-deep
;; doom-gruvbox

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; fix lsp-ui-doc frame issue: https://github.com/emacs-lsp/lsp-ui/issues/464
(setq focus-follows-mouse t)

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
   lsp-ui-peek-always-show t
   lsp-ui-sideline-show-hover t
   )
  )

;; remap localleader key
(after! evil
  (setq! doom-localleader-key ",")
  )

;; fullscreen doom-emacs when start-up
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(defun tinysong/insert-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (retrieve-chrome-current-tab-url)))
;; execute macOS AppleScript
(defun retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  "	set theUrl to get URL of active tab of first window\n"
                  "	set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n")))
        (title (do-applescript
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
    (message "%s" result)
    (message "%s" title)
    (format "[[%s][%s]]" (s-chop-suffix "\"" (s-chop-prefix "\"" result)) title)))


;; retrieve chrom curent table title
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

(
 map!
 "C-c t" #'hl-todo-insert
 "C-c i" #'org-insert-src-block
 "C-c l l" #'tinysong/insert-chrome-current-tab-url
 "C-c l t" #'tinysong/insert-chrome-current-tab-title
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
 org-mobile-directory "~/Library/Mobile Documents/iCloud~com~mobileorg~mobileorg/Documents"
 deft-directory org_notes
 org-roam-directory org_notes
 vulpea-directory org_notes
 )

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-completion-everywhere t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(setq! org-hide-emphasis-markers t)

(use-package! org-ref
  :after org
  :config
  (defun org-ref-noter-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (progn
            (find-file-other-window pdf-file)
            (org-noter))
        (message "no pdf found for %s" key))))

  ;; (add-to-list 'org-ref-helm-user-candidates
  ;;              '("Org-Noter notes" . org-ref-noter-at-point))
  (setq org-ref-bibliography-notes "notes.org")
  (setq org-ref-notes-function #'org-ref-notes-function-one-file)
  )



;; ;; I like short names
;; (general-evil-setup t)
;; ;; Stop telling me things begin with non-prefix keys
;; (general-auto-unbind-keys)



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


;; These bindings should probably be after org-noter is loaded.
(map! :localleader
      :map (org-mode-map pdf-view-mode-map)
      (:prefix ("o" . "Org")
       (:prefix ("n" . "Noter")
        :desc "Noter" "n" 'org-noter
        )))

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

(after! pdf-view
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width)
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
  ;; L capture the link and tile of chrome
  ;; p capture the content of chrome
  ;; FIXME frame float on the chrome
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
               '("a"                                               ; key
                 "Article"                                         ; name
                 entry                                             ; type
                 (file+headline +org-capture-notes-file "Article") ; target
                 "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?" ; template
                 :prepend t             ; properties
                 :empty-lines 1         ; properties
                 :created t             ; properties
                 ))
  ;; TODO  convert html to org file
  (add-to-list 'org-capture-templates
               '("w"
                 "Web site"
                 entry
                 (file+headline +org-capture-notes-file "WebSiste")
                 "* %a :website:\n\n%U %?\n\n%:initial"))
  ;; meeting with some one
  (add-to-list 'org-capture-templates
               '("m"
                 "Meeting"
                 entry
                 (file+headline +org-capture-todo-file "Meeting")
                 "* MEETING with %? :MEETING:\n%U"
                 ))
  )

;; XXX
;; (defun transform-square-brackets-to-round-ones(string-to-transform)
;;   "Transforms [ into ( and ] into ), other chars left unchanged."
;;   (concat
;;   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
;;   )

;; Kill the frame if one was created for the capture
(defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

  (defun kk/delete-frame-if-neccessary (&rest r)
    (cond
     ((= kk/delete-frame-after-capture 0) nil)
     ((> kk/delete-frame-after-capture 1)
      (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
     (t
      (setq kk/delete-frame-after-capture 0)
      (delete-frame))))

  (advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

;; (setq org-capture-templates `())
;; Org Capture
;; Thank you random guy from StackOverflow
;; http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection
;; http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/
(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))


;; ;;; Capture Templates
;; ;;;; Add idea, mind-onanism, contacts, movies to download das
;; (setq org-capture-templates
;;       '(("l" "Temp Links from the interwebs" item
;;          (file+headline "links.org" "Temporary Links")
;;          "%?\nEntered on %U\n \%i\n %a")))

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
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "json"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "yaml" "go" "dockerfile" "applescript" "nix")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (ruby . t)
        (go . t)
        (ditaa . t)
        (python . t)
        (sh . t)
        (abc . t)
        (latex . t)
        (plantuml . t)
        (R . t)))

(setq org-babel-min-lines-for-block-output 0)

(after! org
  (dolist (face '(org-level-1
                  org-level-2 org-level-3
                  org-level-4 org-level-5
                  org-level-6 org-level-7
                  org-level-8))
    (set-face-attribute face nil :weight 'normal))
  )


(use-package! ox-pandoc
  :after org
  :config
  ;; ;; REVIEW org capture https://mediaonfire.com/blog/2017_07_21_org_protocol_firefox.html
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  ;; special extensions for markdown_github output
  (setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))
  )

;; ---------------------------------------------------------------------------------
;; latex setting: https://www.geneatcg.com/emacs-org-mode-export-to-pdf/#org9a91d10
(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))


    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))

    (add-to-list 'org-latex-classes
                 '("ethz"
                   "\\documentclass[a4paper,11pt,titlepage]{memoir}
    \\usepackage[utf8]{inputenc}
    \\usepackage[T1]{fontenc}
    \\usepackage{fixltx2e}
    \\usepackage{graphicx}
    \\usepackage{longtable}
    \\usepackage{float}
    \\usepackage{wrapfig}
    \\usepackage{rotating}
    \\usepackage[normalem]{ulem}
    \\usepackage{amsmath}
    \\usepackage{textcomp}
    \\usepackage{marvosym}
    \\usepackage{wasysym}
    \\usepackage{amssymb}
    \\usepackage{hyperref}
    \\usepackage{mathpazo}
    \\usepackage{color}
    \\usepackage{enumerate}
    \\definecolor{bg}{rgb}{0.95,0.95,0.95}
    \\tolerance=1000
          [NO-DEFAULT-PACKAGES]
          [PACKAGES]
          [EXTRA]
    \\linespread{1.1}
    \\hypersetup{pdfborder=0 0 0}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


    (add-to-list 'org-latex-classes
                 '("article"
                   "\\documentclass[11pt,a4paper]{article}
    \\usepackage[utf8]{inputenc}
    \\usepackage[T1]{fontenc}
    \\usepackage{fixltx2e}
    \\usepackage{graphicx}
    \\usepackage{longtable}
    \\usepackage{float}
    \\usepackage{wrapfig}
    \\usepackage{rotating}
    \\usepackage[normalem]{ulem}
    \\usepackage{amsmath}
    \\usepackage{textcomp}
    \\usepackage{marvosym}
    \\usepackage{wasysym}
    \\usepackage{amssymb}
    \\usepackage{hyperref}
    \\usepackage{mathpazo}
    \\usepackage{color}
    \\usepackage{enumerate}
    \\definecolor{bg}{rgb}{0.95,0.95,0.95}
    \\tolerance=1000
          [NO-DEFAULT-PACKAGES]
          [PACKAGES]
          [EXTRA]
    \\linespread{1.1}
    \\hypersetup{pdfborder=0 0 0}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))


    (add-to-list 'org-latex-classes '("ebook"
                                      "\\documentclass[11pt, oneside]{memoir}
    \\setstocksize{9in}{6in}
    \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
    \\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
    \\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
    \\checkandfixthelayout
    % Much more laTeX code omitted
    "
                                      ("\\chapter{%s}" . "\\chapter*{%s}")
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")))


(setq org-todo-keywords
     '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; An ongoing project that cannot be completed in one step
           "INPROCESS(s)"  ; A task that is in progress
           "⚑ WAITING(w)"  ; Something is holding up this task; or it is paused
           "|"
           "☟ NEXT(n)"
           "✰ IMPORTANT(i)"
           "DONE(d)"  ; Task successfully completed
           "✘ CANCELED(c@)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "✍ NOTE(N)"
           "FIXME(f)"
           "☕ BREAK(b)"
           "❤ LOVE(l)"
           "REVIEW(r)"
           )) ; Task was completed
        org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "#ff39a3" :weight bold))
          ("INPROCESS"  . "orangered")
          ("✘ CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
          ("⚑ WAITING" . "pink")
          ("☕ BREAK" . "gray")
          ("❤ LOVE" . (:foreground "VioletRed4"
                                   ;; :background "#7A586A"
                                   :weight bold))
          ("☟ NEXT" . (:foreground "DeepSkyBlue"
                                   ;; :background "#7A586A"
                                      :weight bold))
          ("✰ IMPORTANT" . (:foreground "greenyellow"
                                      ;; :background "#7A586A"
                                      :weight bold))
          ("DONE" . "#008080")
          ("FIXME" . "IndianRed")
          ))

;; https://www.gtrun.org/custom/config.html
;; (after! latex
;;       (add-to-list 'org-latex-classes '("article" "\\documentclass[a4paper,11pt]{article}
;;         [NO-DEFAULT-PACKAGES]
;;           \\usepackage[utf8]{inputenc}
;;           \\usepackage[T1]{fontenc}
;;           \\usepackage{fixltx2e}
;;           \\usepackage{graphicx}
;;           \\usepackage{longtable}
;;           \\usepackage{float}
;;           \\usepackage{wrapfig}
;;           \\usepackage{rotating}
;;           \\usepackage[normalem]{ulem}
;;           \\usepackage{amsmath}
;;           \\usepackage{textcomp}
;;           \\usepackage{marvosym}
;;           \\usepackage{wasysym}
;;           \\usepackage{amssymb}
;;           \\usepackage{booktabs}
;;           \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
;;           \\tolerance=1000
;;           \\usepackage{listings}
;;           \\usepackage{xcolor}
;;           \\usepackage{fontspec}
;;           \\usepackage{xeCJK}
;;           \\setCJKmainfont{Weibei SC}
;;           \\setmainfont{Fantasque Sans Mono}
;;           \\lstset{
;;           %行号
;;           numbers=left,
;;           %背景框
;;           framexleftmargin=10mm,
;;           frame=none,
;;           %背景色
;;           %backgroundcolor=\\color[rgb]{1,1,0.76},
;;           backgroundcolor=\\color[RGB]{245,245,244},
;;           %样式
;;           keywordstyle=\\bf\\color{blue},
;;           identifierstyle=\\bf,
;;           numberstyle=\\color[RGB]{0,192,192},
;;           commentstyle=\\it\\color[RGB]{0,96,96},
;;           stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
;;           %显示空格
;;           showstringspaces=false
;;           }
;;           "
;;                                         ("\\section{%s}" . "\\section*{%s}")
;;                                         ("\\subsection{%s}" . "\\subsection*{%s}")
;;                                         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                                         ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                                         ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;       ;; {{ export org-mode in Chinese into PDF
;;       ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
;;       ;; and you need install texlive-xetex on different platforms
;;       ;; To install texlive-xetex:
;;       ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
;;       ;; }}
;;       ;;(setq org-latex-default-class "ctexart")
;;     (add-to-list 'org-latex-packages-alist '("" "minted"))
;;     (setq org-latex-listings 'minted)
;;     (setq org-src-fontify-natively t)
;;     (setq org-latex-pdf-process
;;             '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;               "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;               "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;               "xelatex -interaction nonstopmode -output-directory %o %f"
;;               "xelatex -interaction nonstopmode -output-directory %o %f"
;;               "xelatex -interaction nonstopmode -output-directory %o %f"
;;               "rm -fr %b.out %b.log %b.tex auto"))
;; ---------------------------------------------------------------------------------
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
;; Org-Mode导出如何禁用下划线转下标
(setq org-export-with-sub-superscripts nil)

;; setting the .org file default coding system
(add-to-list 'file-coding-system-alist '("\\.org" . utf-8) )
;; TODO: add mu4e for email
;; ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
;; (setq doom-modeline-mu4e nil)
;; ;; also enable the start of mu4e-alert
;; (mu4e-alert-enable-mode-line-display)

;; OSX dictionary
(map!
 "C-c d s" #'osx-dictionary-search-input
 "C-c d d" #'osx-dictionary-search-pointer)

(map!
 "C-c y y" #'youdao-dictionary-search-at-point-tooltip
 "C-c y p" #'youdao-dictionary-play-voice-at-point)

;; NOTE disable go lsp format, go lsp will mess up the code
(setq-hook! 'go-mode-hook +format-with-lsp nil)
;; using goimports instead of gofmt
(setq! gofmt-command "goimports")


;; ;; REVIEW Reference: https://dotdoom.rgoswami.me/config.html#org8ed0901
;; From https://github.com/poligen/dotfiles/blob/25785810f9bf98f6eec93e400c686a4ad65ac310/doom.d/config.el
;; My customized org-download to incorporate flameshot gui Workaround to setup flameshot, which enables annotation.
;; In flameshot, set filename as "screenshot", and the command as "flameshot gui -p /tmp", so that we always ends up
;; with /tmp/screenshot.png. Nullify org-download-screenshot-method by setting it to `echo', so that essentially we
;; are only calling (org-download-image org-download-screenshot-file).
(defun hz-org-download-screenshot ()
  "Capture screenshot and insert the resulting file.
The screenshot tool is determined by `org-download-screenshot-method'."
  (interactive)
  (let ((tmp-file "/tmp/screenshot.png"))
    (delete-file tmp-file)
    (call-process-shell-command "flameshot gui -p /tmp/")
    ;; Because flameshot exit immediately, keep polling to check file existence
    (while (not (file-exists-p tmp-file))
      (sleep-for 2))
    (org-download-image tmp-file)))

(use-package! org-download
  :after org
  :config
  (setq-default org-download-image-dir "./images/"
                ;; org-download-screenshot-method "flameshot gui --raw > %s"
                org-download-delete-image-after-download t
                org-download-method 'directory
                org-download-heading-lvl 1
                org-download-screenshot-file "/tmp/screenshot.png"
                )
  (cond (IS-LINUX (setq-default org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s"))
        (IS-MAC (setq-default org-download-screenshot-method "screencapture -i %s"))))

;; (use-package! org-drill
;;   :after org)

;; ;; NOTE citeproc-org: CSL-based export has recently been merged into Org's master branch  (2021-08-01)
(after! ox-hugo
  (use-package! citeproc-org
    :config
    (citeproc-org-setup)
    (setq citeproc-org-org-bib-header "* References\n")
    )
  )

(after! citeproc-org
  (defun hz/min-headline-level ()
    (--> (org-element-parse-buffer)
         (org-element-map it 'headline (apply-partially #'org-element-property :level))
         (or it '(0))
         (-min it)))

  (defadvice! hz/citeproc-org-render-references (orig &rest args)
    :around 'citeproc-org-render-references
    (let* ((minlevel (hz/min-headline-level))
           (totallevel (max 1 minlevel))
           (citeproc-org-org-bib-header (concat (make-string totallevel ?*)
                                                (string-trim-left citeproc-org-org-bib-header "\\**"))))
      (apply orig args))))


;; ;; Misc Highlighting
;; (use-package! vimrc-mode
;;   :mode "\\.vimrc\\'")

;; ;; TODO org-noter
;; ;; enhancement org-noter
;; ;; HACK: more informatin org-noter: https://github.com/weirdNox/org-noter
(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the rclone mega
   org-noter-notes-search-path (list org_notes)
   ;; default node file
   org-noter-default-notes-file-names '("notes.org")
   ;; keep an empty line between headings and content in Org files
   org-noter-separate-notes-from-heading t
   ))


;; ;; TODO: org-protocol
;; ;; TODO: crawls content send to emacs by org-protocol

;; smerge

(defun smerge-repeatedly ()
  "Perform smerge actions again and again"
  (interactive)
  (smerge-mode 1)
  (smerge-transient))
(after! transient
  (transient-define-prefix smerge-transient ()
    [["Move"
      ("n" "next" (lambda () (interactive) (ignore-errors (smerge-next)) (smerge-repeatedly)))
      ("p" "previous" (lambda () (interactive) (ignore-errors (smerge-prev)) (smerge-repeatedly)))]
     ["Keep"
      ("b" "base" (lambda () (interactive) (ignore-errors (smerge-keep-base)) (smerge-repeatedly)))
      ("u" "upper" (lambda () (interactive) (ignore-errors (smerge-keep-upper)) (smerge-repeatedly)))
      ("l" "lower" (lambda () (interactive) (ignore-errors (smerge-keep-lower)) (smerge-repeatedly)))
      ("a" "all" (lambda () (interactive) (ignore-errors (smerge-keep-all)) (smerge-repeatedly)))
      ("RET" "current" (lambda () (interactive) (ignore-errors (smerge-keep-current)) (smerge-repeatedly)))]
     ["Diff"
      ("<" "upper/base" (lambda () (interactive) (ignore-errors (smerge-diff-base-upper)) (smerge-repeatedly)))
      ("=" "upper/lower" (lambda () (interactive) (ignore-errors (smerge-diff-upper-lower)) (smerge-repeatedly)))
      (">" "base/lower" (lambda () (interactive) (ignore-errors (smerge-diff-base-lower)) (smerge-repeatedly)))
      ("R" "refine" (lambda () (interactive) (ignore-errors (smerge-refine)) (smerge-repeatedly)))
      ("E" "ediff" (lambda () (interactive) (ignore-errors (smerge-ediff)) (smerge-repeatedly)))]
     ["Other"
      ("c" "combine" (lambda () (interactive) (ignore-errors (smerge-combine-with-next)) (smerge-repeatedly)))
      ("r" "resolve" (lambda () (interactive) (ignore-errors (smerge-resolve)) (smerge-repeatedly)))
      ("k" "kill current" (lambda () (interactive) (ignore-errors (smerge-kill-current)) (smerge-repeatedly)))
      ("q" "quit" (lambda () (interactive) (smerge-auto-leave)))]]))


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

;; FIXME TODO Define these faces, in your .emacs file, before requiring Org (if you don't have your own color theme)
;; https://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#FFFFEA")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

(use-package! password-store)

(setq! org-superstar-special-todo-items t)

(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
;; (setq org-html-head
;;       "<link rel=\"stylesheet\" href=\"style.css\">")

;; ;; TODO: Embed inline CSS read from a file.
;;   (defun my-org-inline-css-hook (exporter)
;;     "Insert custom inline css"
;;     (when (eq exporter 'html)
;;       (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
;;              (path (concat dir "style.css"))
;;              (homestyle (and (or (null dir) (null (file-exists-p path)))
;;                              (not (null-or-unboundp 'my-org-inline-css-file))))
;;              (final (if homestyle my-org-inline-css-file path)))
;;         (if (file-exists-p final)
;;             (progn
;;               (setq-local org-html-head-include-default-style nil)
;;               (setq-local org-html-head (concat
;;                                          "<style type=\"text/css\">\n"
;;                                          "<!--/*--><![CDATA[/*><!--*/\n"
;;                                          (with-temp-buffer
;;                                            (insert-file-contents final)
;;                                            (buffer-string))
;;                                          "/*]]>*/-->\n"
;;                                          "</style>\n")))))))

;;   (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

(defun org-insert-image ()
  "insert a image from clipboard"
  (interactive)
  (let* ((path (concat default-directory "images/"))
         (image-file (concat
                      path
                      (buffer-name)
                      (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
        (mkdir path))
    (do-applescript (concat
                     "set the_path to \"" image-file "\" \n"
                     "set png_data to the clipboard as «class PNGf» \n"
                     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
                     "write png_data to the_file \n"
                     "close access the_file"))

    ;; (shell-command (concat "pngpaste " image-file))

    (insert
     "#+CAPTION: "  "\n"
     "#+ATTR_HTML: :style max-width: 100% \n"
     )
    (org-insert-link nil
                     (concat
                      "file:" image-file) "")

    (message image-file))
  (org-display-inline-images))

;; functions borrowed from `vulpea' library
;; https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el


;; ;  ;;;;;;;;;;;;;; org agenda in org roam using title as category ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(setq org-agenda-prefix-format
      '((agenda . " %i %-12(vulpea-agenda-category)%?-12t% s")
        (todo . " %i %-12(vulpea-agenda-category) ")
        (tags . " %i %-12(vulpea-agenda-category) ")
        (search . " %i %-12(vaulpea-agenda-category) ")))

(defun vulpea-agenda-category ()
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category)))
    (or (if (and
             title
             (string-equal category file-name))
            title
          category)
        "")))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))
;;  comment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (eq type 'todo))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%" ))]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

(defun update-filetags ()
  )

(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)

;; (advice-add 'org-agenda :before #'vulpea-agenda-files-update)

;; functions borrowed from `vulpea' library
;; https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el

(defun vulpea-buffer-tags-get ()
  "Return filetags value in current buffer."
  (vulpea-buffer-prop-get-list "filetags" " "))

(defun vulpea-buffer-tags-set (&rest tags)
  "Set TAGS in current buffer.
If filetags value is already set, replace it."
  (vulpea-buffer-prop-set "filetags" (string-join tags " ")))

(defun vulpea-buffer-tags-add (tag)
  "Add a TAG to filetags in current buffer."
  (let* ((tags (vulpea-buffer-tags-get))
         (tags (append tags (list tag))))
    (apply #'vulpea-buffer-tags-set tags)))

(defun vulpea-buffer-tags-remove (tag)
  "Remove a TAG from filetags in current buffer."
  (let* ((tags (vulpea-buffer-tags-get))
         (tags (delete tag tags)))
    (apply #'vulpea-buffer-tags-set tags)))

(defun vulpea-buffer-prop-set (name value)
  "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
  (setq name (downcase name))
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                             (point-max) t)
          (replace-match (concat "#+" name ": " value) 'fixedcase)
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

(defun vulpea-buffer-prop-set-list (name values &optional separators)
  "Set a file property called NAME to VALUES in current buffer.
VALUES are quoted and combined into single string using
`combine-and-quote-strings'.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
If the property is already set, replace its value."
  (vulpea-buffer-prop-set
   name (combine-and-quote-strings values separators)))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun vulpea-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
  (let ((value (vulpea-buffer-prop-get name)))
    (when (and value (not (string-empty-p value)))
      (split-string-and-unquote value separators))))

;;;;;;;;;;;;; auto add tag when insrt node  ;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun vulpea-insert ()
  "Insert a link to the note."
  (interactive)
  (when-let*
      ((node (org-roam-node-insert))
       (title (org-roam-node-title node))
       (tags (org-roam-node-tags node)))
    (when (seq-contains-p tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-set-tags
           (seq-uniq
            (cons
             (vulpea--title-to-tag title)
             (org-get-tags nil t)))))))))

(defun vulpea--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "@" (s-replace " " "" title)))

;; (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
;;                   :major-modes '(nix-mode)
;;                   :server-id 'nix))

(use-package! vulpea
  ;; hook into org-roam-db-autosync-mode you wish to enable
  ;; persistence of meta values (see respective section in README to
  ;; find out what meta means)
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

;; Open eshell in split window
;;
(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))

;; Git branch name is partly hidden
(setq doom-modeline-vcs-max-length 15)

;; lsp mode for nix-mode
(add-hook! 'nix-mode-hook #'lsp)

;;https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun er-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; TODO markdown convert to pdf
;; https://www.jianshu.com/p/7f9a9ff053bb
;; https://pandoc.org/MANUAL.html#templates
;; https://www.cnblogs.com/airbird/p/11455210.html

(use-package! pandoc-mode
  :after (markdown-mode org-mode)
  :hook
  (markdown-mode org-mode)
  (pandoc-mode . pandoc-load-default-settings))


(defun tinysong/insert-break-line()
  "Insert a break line at cursor point."
  (interactive)
  (insert "-------------------------------------------------------\n \n")
  (backward-char 1))
