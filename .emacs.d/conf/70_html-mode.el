;; html-mode
;; -----------------------------------------------------------------------------

(add-hook 'html-mode-hook
          (lambda ()
            ;; t tab を使ってindentする | nil 半角スペースを使う
            (setq indent-tabs-mode t)
			(setq indent-line-function 'tab-to-tab-stop)))
