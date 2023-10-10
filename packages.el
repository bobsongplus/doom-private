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
(package! osx-dictionary)
;; ;; info colors
(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")


(package! keycast :pin "04ba7519f34421c235bac458f0192c130f732f12")


;; org roam ui just fot roam2
(package! websocket)
(unpin! org-roam)
(package! org-roam)
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

(package! org2ctex
  :recipe (:host github
           :repo "tumashu/org2ctex"))

(package! citeproc-org)

(package! password-store)

(package! vulpea)
(package! pdf-tools)

(package! pandoc-mode)

(package! protobuf-mode)
(package! ox-hugo)

;;TODO the notions of a collection. https://github.com/publicimageltd/delve
(package! delve
  :recipe (:host github
           :repo "publicimageltd/delve"))

(package! sis)
(package! tmux-pane)
(package! evil-pinyin)
(package! term-cursor :recipe
  (:host github :repo "h0d/term-cursor.el"))

(package! vtm :recipe
  (:host github :repo "laishulu/emacs-vterm-manager"))


(package! evil-textobj-column)
(package! evil-textobj-line)
(package! evil-textobj-syntax)
(package! evil-textobj-entire)

(package! rime)


;; Add support for rendering KConfig fragments (from the Linux kernel/Buildroot) nicely.
(package! kconfig-mode :pin "cd87b71c8c1739d026645ece0bbd20055a7a2d4a")
(use-package! kconfig-mode)
