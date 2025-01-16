;; Enable auto-insert
(use-package! autoinsert
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory "~/.config/doom/copyright-templates/")
  (auto-insert-mode 1)
  :config
  (setq auto-insert-alist
        '(("\\.cpp\\'" . ["template.cpp" auto-update-header])
          ("\\.hpp\\'" . ["template.hpp" auto-update-header])
          ("\\.go\\'" . ["template.go" auto-update-header])))

  (defun auto-update-header ()
    (save-excursion
      (while (search-forward "COPYRIGHTYEAR" nil t)
        (replace-match (format-time-string "%Y") nil t))
      (while (search-forward "COPYRIGHTDATE" nil t)
        (replace-match (format-time-string "%Y-%m-%d") nil t))
      (while (search-forward "COPYRIGHTAUTHOR" nil t)
        (replace-match user-full-name nil t)))
    )
  )
