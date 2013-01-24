;; zencoding-mode.el --- Unfold CSS-selector-like expressions to markup
;; ------------------------------------------------------------------------------
(require 'zencoding-mode)
(define-key zencoding-mode-keymap "\M-Z" 'zencoding-expand-line)

(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)
