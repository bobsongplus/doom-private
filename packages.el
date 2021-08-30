;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; youdao translate
(package! youdao-dictionary)

;; ;; info colors
;; (package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")


(package! keycast :pin "04ba7519f34421c235bac458f0192c130f732f12")

;; snippets hlissner
(use-package! doom-snippets
  :after yasnippet)
; AndreaCrotti
(use-package! yasnippet-snippets
  :after yasnippet)

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
   )
  )

;; org roam ui just fot roam2
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))


(package! org-protocol-capture-html
  :recipe (:host github
           :repo "alphapapa/org-protocol-capture-html"))


(package! org-ref)

(package! org-mind-map
  :recipe (:host github
            :repo "theodorewiles/org-mind-map"))

;; org google calendar
(package! org-gcal)

;; This needs to be installed specially, https://github.com/alphapapa/org-protocol-capture-html
(package! org-protocol-capture-html
  :recipe (:host github
           :repo "alphapapa/org-protocol-capture-html"))
