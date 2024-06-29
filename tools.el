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

(map!
 "C-c t" #'hl-todo-insert
 "C-c i" #'org-insert-src-block
 "C-c l l" #'tinysong/insert-chrome-current-tab-url
 "C-c l t" #'tinysong/insert-chrome-current-tab-title
 )

;; OSX dictionary
;; Work with popwin-el (https://github.com/m2ym/popwin-el)
;; (push "*osx-dictionary*" popwin:special-display-config)
;; (use-package! youdao-dictionary
;;   (map!
;;    "C-c d s" #'osx-dictionary-search-input
;;    "C-c d d" #'osx-dictionary-search-pointer)

;;   (map!
;;    "C-c y y" #'youdao-dictionary-search-at-point-tooltip
;;    "C-c y p" #'youdao-dictionary-play-voice-at-point)
;;   )

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

;; (use-package! liberime
;;   :config
;;   (setq! liberime-module-file "~/.config/doom/third-packages/macos-x86-liberime-core.so"
;;          ))
;; (use-package! pyim
;;   ;; :quelpa (pyim :fetcher github :repo "merrickluo/pyim")
;;   :init
;;   (setq pyim-title "R")
;;   :config
;;   ;; (use-package pyim-basedict
;;   ;;   :config
;;   ;;   (pyim-basedict-enable))
;;   (global-set-key (kbd "M-j") 'pyim-convert-string-at-point)
;;   (setq pyim-dcache-auto-update nil)
;;   (setq default-input-method "pyim")
;;   ;; 我使用全拼
;;   (setq pyim-default-scheme 'rime)
;;   (setq pyim-page-tooltip 'child-frame)

;;   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;   ;; 我自己使用的中英文动态切换规则是：
;;   ;; 1. 光标只有在注释里面时，才可以输入中文。
;;   ;; 2. 光标前是汉字字符时，才能输入中文。
;;   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;;   (setq-default pyim-english-input-switch-functions
;; 		        '(pyim-probe-dynamic-english
;; 		          pyim-probe-isearch-mode
;; 		          pyim-probe-program-mode
;;                   pyim-probe-evil-normal-mode
;; 		          pyim-probe-org-structure-template))

;;   (setq-default pyim-punctuation-half-width-functions
;; 		        '(pyim-probe-punctuation-line-beginning
;; 		          pyim-probe-punctuation-after-punctuation)))

(defun +rime-predicate-is-back-quote-or-tilde ()
  (or (equal rime--current-input-key ?`)
      (equal rime--current-input-key ?~)))

(defun +rime-inline-predicate()
  (and (not (or (eq major-mode 'minibuffer-mode) (eq major-mode 'notdeft-mode)))
       (or (rime-predicate-space-after-cc-p)
	   (+rime-predicate-is-back-quote-or-tilde)
	   (rime-predicate-current-uppercase-letter-p))))

(defun +rime-disable-predicate()
  (and (not (or (eq major-mode 'minibuffer-mode) (eq major-mode 'notdeft-mode)))
       (or (rime-predicate-prog-in-code-p) (rime-predicate-after-alphabet-char-p)
	   (meow-normal-mode-p) (meow-motion-mode-p) (meow-keypad-mode-p))))

;; (use-package! rime
;;   :custom
;;   (setq default-input-method "rime"
;;         rime-show-candidate 'popup
;;         rime-posframe-style 'simple
;;         rime-popup-style 'simple
;;         rime-sidewindow-style 'bottom
;;         rime-sidewindow-keep-window t
;;         rime-inline-ascii-trigger 'shift-l
;;         )
;;   (add-hook! (org-mode
;;               markdown-mode
;;               beancount-mode)
;;     (activate-input-method default-input-method))
;;   )

(use-package! rime
  :custom
  (default-input-method "rime")
  (rime-emacs-module-header-root "~/development/emacs/nextstep/Emacs.app/Contents/Resources/include/")
  (rime-librime-root (expand-file-name "third-packages/librime/dist" default-directory))
  (rime-cursor "˰")
  (rime-show-candidate 'posframe)
  ;;https://emacs-china.org/t/doom-emacs-rime/12499/18?u=huos3203
  ;;  (rime-share-data-dir "/Users/boyer/Library/Rime")
  ;;  (rime-user-data-dir "/Users/boyer/Library/Rime")
  (rime-posframe-properties
   (list :background-color "#073642"
         :foreground-color "#839496"
         :internal-border-width 1
         :font "Input Mono Narrow"
         ))
  ;;
  (rime-disable-predicates '(rime-predicate-evil-mode-p
                             rime-predicate-after-alphabet-char-p
                             rime-predicate-prog-in-code-p))
  ;;; 具体参考 mode-line-mule-info 默认值，其中可能有其它有用信息
  (mode-line-mule-info '((:eval (rime-lighter))))
  ;;在 minibuffer 使用后自动关闭输入法
  (rime-deactivate-when-exit-minibuffer t)

  :config
  (set-face-attribute 'rime-default-face nil :foreground "#839496" :background "#073642")
  )





(use-package tramp
  :config
  (add-to-list 'tramp-default-method-alist
               '("\\`localhost\\'" "\\`root\\'" "su"))

  ;; Do not connect to root directly to specific hosts, but use user and then
  ;; su/sudo
  (add-to-list 'tramp-default-proxies-alist
               '("192\\.168\\.42\\.3" "\\`root\\'" "/ssh:%h:"))

  ;; Seriously speed things up by setting async usage with tramp
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:")
                     "direct-async-process" t))

  ;; Speed up tramp by reusing connetions
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=~/.ssh-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=yes"))


  (setq tramp-default-host "localhost")
  (setq tramp-default-method "ssh"))



(after! eglot
  :config
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy")))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(c++-mode . ("clangd" :initializationOptions
                             (:compilationDatabasePath "/tmp")))))



(defun insert-random-uuid ()
  "Insert a UUID.
This commands calls “uuidgen” on MacOS, Linux, and calls PowelShell on Microsoft Windows.
URL `http://xahlee.info/emacs/emacs/elisp_generate_uuid.html'
Version: 2020-06-04 2023-05-13"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command "pwsh.exe -Command [guid]::NewGuid().toString()" t))
   ((eq system-type 'darwin) ; Mac
    (shell-command "uuidgen" t))
   ((eq system-type 'gnu/linux)
    (shell-command "uuidgen" t))
   (t
    ;; code here by Christopher Wellons, 2011-11-18.
    ;; and editted Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
    (let ((xstr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                             (user-uid)
                             (emacs-pid)
                             (system-name)
                             (user-full-name)
                             (current-time)
                             (emacs-uptime)
                             (garbage-collect)
                             (buffer-string)
                             (random)
                             (recent-keys)))))
      (insert (format "%s-%s-4%s-%s%s-%s"
                      (substring xstr 0 8)
                      (substring xstr 8 12)
                      (substring xstr 13 16)
                      (format "%x" (+ 8 (random 4)))
                      (substring xstr 17 20)
                      (substring xstr 20 32)))))))
