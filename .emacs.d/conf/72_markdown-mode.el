;; ------------------------------------------------------------------------------
;; markdown-mode.el --- Emacs Major mode for Markdown-formatted text files
;; ------------------------------------------------------------------------------
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
;(setenv "LC_ALL" "C")                   ; 文字化けする場合、環境変数を設定
(setq markdown-command my-markdown-command)
(setq markdown-bold-underscore t)
(setq markdown-italic-underscore t)

(setq markdown-xhtml-header-content "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
<meta http-equiv=\"Content-Script-Type\" content=\"text/javascript\" />
<meta http-equiv=\"Content-Style-Type\" content=\"text/css\" />
<link rel=\"stylesheet\" href=\"css/minimal.css\" type=\"text/css\" />")

(add-hook 'markdown-mode-hook
          '(lambda ()
             (setq outline-regexp "\\(^\\(.*\\)\n\\(===+\\)$\\|^\\(.*\\)\n\\(---+\\)$\\|^#+\\)")
             (outline-minor-mode t)))


(defun markdown-create-imenu-index ()
  (let ((index)
        (pattern "\\(^\\(.*\\)\n\\(===+\\)$\\|^\\(.*\\)\n\\(---+\\)$\\|^#+\\)"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern (point-max) t)
        (push (cons (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                    (match-beginning 0)) index))
      (nreverse index))))

(add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
(add-hook 'markdown-mode-hook
          '(lambda ()
             (setq imenu-create-index-function 'markdown-create-imenu-index)))

;; (defun outline-level-markdown ()
;;   (interactive)
;;   (let ((str nil))
;;     (looking-at outline-regexp)
;;     (setq str
;;           (buffer-substring-no-properties
;;            (match-beginning 0) (match-end 0)))

;;     (cond
;;      ((string-match "^\\(.*\\)\n\\(===+\\)$" str) 1)
;;      ((string-match "^\\(.*\\)\n\\(---+\\)$" str) 2)
;;      ((string-match "^###" str) 3)
;;      ((string-match "^####" str) 4)
;;      (t 100))))

(add-hook 'markdown-mode-hook
          '(lambda ()
             (setq outline-level
                   (function
                    (lambda ()
                      (save-excursion
                        (let ((str nil))
                          (looking-at outline-regexp)
                          (setq str (buffer-substring-no-properties
                                     (match-beginning 0) (match-end 0)))
                          (cond
                           ((string-match "^\\(.*\\)\n\\(===+\\)$" str) 1)
                           ((string-match "^\\(.*\\)\n\\(---+\\)$" str) 2)
                           ((string-match "^###" str) 3)
                           ((string-match "^####" str) 4)
                           (t 100))))
                      )))))
