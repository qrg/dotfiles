;; html-mode
;; -----------------------------------------------------------------------------

(add-hook 'html-mode-hook
          (lambda ()
            ;; t tab を使ってindentする | nil 半角スペースを使う
            (setq indent-tabs-mode t)
			(setq indent-line-function 'tab-to-tab-stop)))



(when (require 'tidy)
  (autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
  (autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
  (autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
  (autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)
)

(defun my-html-mode-hook () "Customize my html-mode."
  (tidy-build-menu html-mode-map)
  (local-set-key [(control c) (control c)] 'tidy-buffer)
  (setq sgml-validate-command "tidy"))

(add-hook 'html-mode-hook 'my-html-mode-hook)

(setq tidy-shell-command "/usr/local/bin/tidy")
