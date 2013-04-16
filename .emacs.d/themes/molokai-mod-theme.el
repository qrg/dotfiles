(deftheme molokai-mod
  "based on emacs24 port of molokai for vim")
(custom-theme-set-faces
 'molokai-mod
 '(cursor                           ((t (:background "#f8f8f0"))))
 '(font-lock-builtin-face           ((t (:foreground "#89d8ff"))))
 '(font-lock-function-name-face     ((t (:foreground "#a6e22e" :bold t))))
 '(font-lock-type-face              ((t (:foreground "#66d9ef"))))
 '(font-lock-warning-face           ((t (:inherit error))))
 '(error                            ((t (:background "#1e0010" :foreground "#960050"))))
 '(font-lock-keyword-face           ((t (:foreground "#f92672" :bold t))))
 '(font-lock-delimiter-face         ((t (:foreground "#8f8f8f"))))
 '(font-lock-constant-face          ((t (:foreground "#fd971f"))))
 '(font-lock-string-face            ((t (:foreground "#e6db74"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :italic nil))))
 '(font-lock-comment-face           ((t (:foreground "#8a989b" :italic t))))
 '(show-paren-match                 ((t (:background "#ff5100" :foreground "white"))))
 '(esk-paren-face                   ((t (:foreground "grey50"))))
 '(whitespace-tab                   ((t (:background "#272822"))))
 '(mode-line                        ((t (:background "#444444" :foreground "#dddddd" :box nil))))
 '(mode-line-inactive               ((t (:inherit (mode-line) :background "#080808" :foreground "#75715e" :box nil))))
 '(highlight                        ((t (:background "#403d3d" :inverse-video t))))
 '(region                           ((t (:inherit highlight :background "#437f7e" :inverse-video nil))))
 '(hl-line                          ((t (:background "#212c2e" :inverse-video nil))))
 '(font-lock-number-face            ((t (:foreground "#ae81ff"))))
 '(font-lock-variable-name-face     ((t (:foreground "#fd971f"))))
 '(font-lock-preprocessor-face      ((t (:foreground "#a6e22e"))))

;; custom

 '(priority-low ((t ( :foreground "gray20"))))
 '(header-base ((t ( :background "#222" :weight bold ))))
 '(header-1    ((t ( :inherit header-base :foreground "#9db939" :height 1.0 ))))
 '(header-2    ((t ( :inherit header-base :height 1.0 ))))
 '(header-3    ((t ( :inherit header-base :height 1.0 ))))
 '(header-4    ((t ( :inherit header-base :height 1.0 ))))
 '(header-5    ((t ( :inherit header-base :height 1.0 ))))
 '(header-6    ((t ( :inherit header-base :height 1.0 ))))

;; packages

 '(markdown-header-delimiter-face   ((t (:inherit font-lock-delimiter-face))))
 '(markdown-list-face               ((t (:inherit font-lock-keyword-face))))
 '(markdown-link-face               ((t (:foreground "#808080" :underline t))))
 '(markdown-header-face             ((t (:foreground "#ef5939" :weight bold))))
 '(markdown-url-face                ((t (:inherit font-lock-builtin-face))))
 '(markdown-blockquote-face         ((t ( :inherit font-lock-doc-face))))
 '(markdown-bold-face               ((t ( :inherit font-lock-variable-name-face :bold t))))
 '(markdown-comment-face            ((t ( :inherit font-lock-comment-face))))
 '(markdown-header-face             ((t ( :inherit header-base  ))))
 '(markdown-header-face-1           ((t ( :inherit header-1 ))))
 '(markdown-header-face-2           ((t ( :inherit header-2 ))))
 '(markdown-header-face-3           ((t ( :inherit header-3 ))))
 '(markdown-header-face-4           ((t ( :inherit header-4 ))))
 '(markdown-header-face-5           ((t ( :inherit header-5 ))))
 '(markdown-header-face-6           ((t ( :inherit header-6 ))))
 '(ruler-mode-column-number         ((t ( :inherit ruler-mode-default :box (:line-width 1)))))
 '(ruler-mode-current-column        ((t ( :inherit ruler-mode-default :background "#e6005c" :foreground "#e6005c" :box (:line-width 1)))))
 '(ruler-mode-fringes               ((t ( :inherit ruler-mode-default ))))
 '(ruler-mode-margins               ((t ( :inherit ruler-mode-default ))))
 '(ruler-mode-default               ((t ( :inherit default :background "#111111" :foreground "#888888"
                                          :weight normal
                                          :underline t
                                          :box (:line-width 1 :color "#111" :style released-button)))))

 '(whitespace-empty                    ((t ( :background "yellow" :foreground "firebrick"))))
 '(whitespace-hspace                   ((t ( :background "grey24" :foreground "aquamarine3"))))
 '(whitespace-indentation              ((t ( :background "yellow" :foreground "firebrick"))))
 '(whitespace-line                     ((t ( :background "gray30" :foreground "violet"))))
 '(whitespace-newline                  ((t ( :foreground "gray30" :weight normal))))
 '(whitespace-space                    ((t ( :inherit priority-low))))
 '(whitespace-space-after-tab          ((t ( :background "yellow" :foreground "firebrick"))))
 '(whitespace-space-before-tab         ((t ( :background "DarkOrange" :foreground "firebrick"))))
 '(whitespace-tab                      ((t ( :inherit priority-low))))
 '(whitespace-trailing                 ((t ( :bold nil :background "red1" :foreground "yellow" :weight normal))))

 '(diff-added               ((t ( :bold nil foreground "#60c1c8" :weight normal))))
 '(diff-removed             ((t ( :bold nil foreground "#ff4f23" :weight normal))))


 '(default                          ((t (:background "#1b1d1e" :foreground "#f8f8f2"))))

 )
;;; ###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'molokai-mod)
