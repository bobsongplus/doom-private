;;; ../../development/dotfiles/.config/doom/tools.el -*- lexical-binding: t; -*-

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

;; OSX dictionary
(map!
 "C-c d s" #'osx-dictionary-search-input
 "C-c d d" #'osx-dictionary-search-pointer)

(map!
 "C-c y y" #'youdao-dictionary-search-at-point-tooltip
 "C-c y p" #'youdao-dictionary-play-voice-at-point)

(use-package! password-store)

(defun convert-yaml-yml-buffer-region-to-json ()
  "Convert the current buffer's selected region from yaml to json format
and save it with the current buffer's file name but with .json extension.
need jq and yq command"
  (interactive)
  (let ((output-dir (read-directory-name "Output directory: "))
	(input-file (file-name-nondirectory buffer-file-name)))
    (setq output-file (concat output-dir (file-name-sans-extension input-file) "-buffer-region.json"))
    (shell-command-on-region
     (region-beginning) (region-end)
     (concat "yq r -j - | jq  > " (shell-quote-argument output-file)))
    ))

(defun tinysong/insert-break-line()
  "Insert a break line at cursor point."
  (interactive)
  (insert "-------------------------------------------------------\n \n")
  (backward-char 1))
