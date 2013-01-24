(eval-when-compile (require 'color-theme))

(defun color-theme-user03-dark ()
  "Color theme by QRG, created 2010-08-15."
  (interactive)
  (color-theme-install
   '(color-theme-user02-dark

     ((background-mode                       . dark)
      (foreground-color                      . "#ccc")
      (background-color                      . "#140b19")
      (border-color                          . "#140b19")
      (cursor-color                          . "#fff")
      (mouse ((t (:inherit highlight)))))

     ((goto-address-mail-face                . italic)
      (goto-address-mail-mouse-face          . secondary-selection)
      (goto-address-url-face                 . link)
      (goto-address-url-mouse-face           . highlight)
      (list-matching-lines-buffer-name-face  . underline)
      (list-matching-lines-face              . match)
      (view-highlight-face                   . highlight)
      (widget-mouse-face                     . highlight))

;; basic

     (default                             ((t (nil))))
     (bold                                ((t ( :bold nil :weight normal))))
     (bold-italic                         ((t ( :italic nil :slant normal :weight normal))))
     (buffer-menu-buffer                  ((t ( :bold nil :weight normal))))
     (button                              ((t ( :underline t))))
     (underline                           ((t ( :underline t))))
     (variable-pitch                      ((t ( :family "Sans Serif"))))
     (highlight                           ((t (:background "#2aadd5" :foreground "white"))))
     (highlight2                          ((t (:background "#e6ee00" :foreground "white"))))
     (fixed-pitch                         ((t (:inherit default))))
     (fringe                              ((t (:background "#140b19"))))
     (italic                              ((t (:italic nil :slant normal))))
     (link                                ((t (:inherit font-lock-builtin-face :underline t))))
     (link-visited                        ((t (:foreground "#789dab" :underline t))))

     (match                               ((t (:inherit isearch))))
     (menu                                ((t (:background "#140b19" :foreground "#aaa"))))
     (minibuffer-prompt                   ((t (:inherit font-lock-warning-face))))
     (hl-line                             ((t (:background "#111"))))
     (header-line                         ((t (:height 90 :background "#333" :foreground "#999" :box nil)))) ; dired などの header

     (mode-line                           ((t (:background "gray20" :foreground "gray65" :height 100))))
     (mode-line-buffer-id                 ((t (:inherit font-lock-builtin-face :weight normal))))
     (mode-line-emphasis                  ((t (:weight normal))))
     (mode-line-highlight                 ((t (:background "black" :box nil))))
     (mode-line-inactive                  ((t (:background "black" :foreground "gray28" :box nil :weight light))))

     (next-error                          ((t (:background "lightgoldenrod2"))))
     (nobreak-space                       ((t (:foreground "brown" :underline t))))
     (query-replace                       ((t (:background "magenta3" :foreground "lightskyblue1"))))
     (region                              ((t (:inherit highlight))))
     (ruler-mode-default                  ((t ( :inherit default :background "#050019" :foreground "#888888"
                                                :weight normal
                                                :underline t
                                                :box (:line-width 1 :color "#111" :style released-button)))))
     (ruler-mode-column-number            ((t ( :inherit ruler-mode-default :box (:line-width 1)))))
     (ruler-mode-current-column           ((t ( :inherit ruler-mode-default :background "#e6005c" :foreground "#e6005c"
                                                :box (:line-width 1)))))
     (ruler-mode-fringes                  ((t ( :inherit ruler-mode-default ))))
     (ruler-mode-margins                  ((t ( :inherit ruler-mode-default ))))
     (scroll-bar                          ((t ( :background "red" :foreground "systemscrollbar"))))
     (secondary-selection                 ((t ( :foreground "#140b19"  :background "yellow1"))))
     (shadow                              ((t ( :foreground "grey50"))))
     (show-paren-match                    ((t ( :inherit highlight :weight normal))))
     (show-paren-mismatch                 ((t ( :background "purple" :foreground "#ddd"))))
     (tool-bar                            ((t ( :inherit modeline
                                                :box (:line-width 1
                                                :style released-button)))))
     (tooltip                             ((t ( :background "#fff"
                                                :foreground "#333"
                                                :slant normal :weight normal
                                                :height 90
                                                :width normal
                                                :foundry "outline"
                                                :family "mono"))))

;; font lock

     (font-lock-builtin-face              ((t (:foreground "#88d7dd"))))
     (font-lock-comment-delimiter-face    ((t (:background "#000" :foreground "#888"))))
     (font-lock-comment-face              ((t (:background "#000" :foreground "#888"))))
     (font-lock-constant-face             ((t (:foreground "CadetBlue"))))
     (font-lock-doc-face                  ((t (:foreground "#7b746d"))))
     (font-lock-function-name-face        ((t (:foreground "#ef4057"))))
     (font-lock-keyword-face              ((t (:foreground "#94c100"))))
     (font-lock-negation-char-face        ((t (nil))))
     (font-lock-preprocessor-face         ((t (:foreground "Orchid"))))
     (font-lock-regexp-grouping-backslash ((t (:weight normal))))
     (font-lock-regexp-grouping-construct ((t (:weight normal))))
     (font-lock-string-face               ((t (:foreground "#db86b3"))))
     (font-lock-type-face                 ((t (:inherit font-lock-keyword-face))))
     (font-lock-variable-name-face        ((t (:foreground "yellow2"))))
     (font-lock-warning-face              ((t (:foreground "DeepPink2" :weight normal))))

;; custom
     (custom-button                       ((t ( :background "DeepPink2"
                                                :foreground "White"
                                                :box (:line-width 1 :color "DeepPink2")))))
     (custom-button-mouse                 ((t ( :background "#96cedd"
                                                :foreground "#333"
                                                :box (:line-width 1 :color "#222")))))
     (custom-button-pressed               ((t ( :background "#fff"
                                                :foreground "#140b19"
                                                :box 1))))
     (custom-button-pressed-unraised ((default (:inherit custom-button-unraised))
                                      (((class color) (background dark)) ( :foreground "#a5d5e2"))))
     (custom-button-unraised              ((t ( :inherit underline))))
     (custom-state ((((class color) (background dark)) (        :foreground "#74ab00"))))
     (custom-variable-tag ((((class color) (background dark)) ( :foreground "#a5d5e2"))))

     (priority-low                        ((t ( :foreground "gray20"))))

     (header-base           ((t ( :background "#222" :weight bold ))))
     (header-1              ((t ( :inherit header-base :foreground "#9db939" :height 1.0 ))))
     (header-2              ((t ( :inherit header-base :height 1.0 ))))
     (header-3              ((t ( :inherit header-base :height 1.0 ))))
     (header-4              ((t ( :inherit header-base :height 1.0 ))))
     (header-5              ((t ( :inherit header-base :height 1.0 ))))
     (header-6              ((t ( :inherit header-base :height 1.0 ))))

;;

     (calendar-today                      ((t ( :underline t))))
     (completions-common-part             ((t ( :background "#140b19"
                                                :foreground "#ddd"
                                                :inverse-video nil
                                                :box nil
                                                :strike-through nil
                                                :overline nil
                                                :underline nil
                                                :slant normal
                                                :weight normal
                                                :height 120
                                                :width normal
                                                :foundry "outline"
                                                :family "mono"))))
     (completions-first-difference        ((t ( :bold nil :weight normal))))
     (diary                               ((t (:foreground "yellow1"))))

     (escape-glyph                        ((t (:foreground "brown"))))
     (ffap                                ((t (:inherit highlight))))
     (file-name-shadow                    ((t (:foreground "grey50"))))

     (help-argument-name                  ((t (nil))))
     (holiday                             ((t ( :background "chocolate4"))))

     (info-header-node                    ((t ( :inherit default))))
     (info-header-xref                    ((t ( :inherit link))))
     (info-xref-visited                   ((t ( :inherit link-visited))))
     (info-menu-header                    ((t ( :family "Sans Serif" :height 2.0))))
     (info-menu-star                      ((t ( :inherit font-lock-warning-face))))
     (info-node                           ((t ( :italic nil :bold nil :foreground "white" :slant normal :weight normal))))
     (info-title-1                        ((t ( :bold nil :family "Sans Serif" :height 4.0 :weight normal))))
     (info-title-2                        ((t ( :bold nil :family "Sans Serif" :height 3.0 :weight normal))))
     (info-title-3                        ((t ( :family "Sans Serif" :height 2.0))))
     (info-title-4                        ((t ( :family "Sans Serif" :height 1.4))))
     (info-xref                           ((t ( :inherit link))))

     (isearch                             ((t ( :background "#f7ff00" :foreground "#140b19"))))
     (isearch-fail                        ((t ( :background "#e6005c" :foreground "#fff"))))

     (jaspace-highlight-eol-face          ((t ( :inherit priority-low))))
     (jaspace-highlight-jaspace-face      ((t ( :inherit priority-low))))
     (jaspace-highlight-tab-face          ((t ( :inherit priority-low))))

     (lazy-highlight                      ((t ( :background "#fff" :foreground "#140b19"))))

     (one-key-keystroke                   ((t (:inherit font-lock-string-face))))
     (trailing-whitespace                 ((t ( :background "red"))))
     (vertical-border                     ((t (nil))))

     (which-func                          ((t ( :inherit font-lock-builtin-face))))

     (whitespace-empty                    ((t ( :background "yellow" :foreground "firebrick"))))
     (whitespace-hspace                   ((t ( :background "grey24" :foreground "aquamarine3"))))
     (whitespace-indentation              ((t ( :background "yellow" :foreground "firebrick"))))
     (whitespace-line                     ((t ( :background "gray20" :foreground "violet"))))
     (whitespace-newline                  ((t ( :inherit priority-low))))
     (whitespace-space                    ((t ( :inherit priority-low))))
     (whitespace-space-after-tab          ((t ( :background "yellow" :foreground "firebrick"))))
     (whitespace-space-before-tab         ((t ( :background "DarkOrange" :foreground "firebrick"))))
     (whitespace-tab                      ((t ( :inherit priority-low))))
     (whitespace-trailing                 ((t ( :bold nil :background "red1" :foreground "yellow"))))

     (widget-button                       ((t ( :bold nil :weight normal))))
     (widget-button-pressed               ((t ( :inherit font-lock-warning-face))))
     (widget-documentation                ((t ( :foreground "dark green"))))
     (widget-field                        ((t ( :background "#222"))))
     (widget-inactive                     ((t ( :foreground "#888"))))
     (widget-single-line-field            ((t ( :background "#eee"))))

     ;; markdown-mode
     (markdown-blockquote-face            ((t ( :inherit font-lock-doc-face))))
     (markdown-bold-face                  ((t ( :inherit font-lock-variable-name-face :bold t))))
     (markdown-comment-face               ((t ( :inherit font-lock-comment-face))))
     (markdown-header-face                ((t ( :inherit header-base  ))))
     (markdown-header-face-1              ((t ( :inherit header-1 ))))
     (markdown-header-face-2              ((t ( :inherit header-2 ))))
     (markdown-header-face-3              ((t ( :inherit header-3 ))))
     (markdown-header-face-4              ((t ( :inherit header-4 ))))
     (markdown-header-face-5              ((t ( :inherit header-5 ))))
     (markdown-header-face-6              ((t ( :inherit header-6 ))))
;;    (markdown-inline-code-face             ((t (:inherit font-lock-constant-face))))
;;    (markdown-italic-face                  ((t (:inherit font-lock-variable-name-face :italic t))))
;;    (markdown-link-face                    ((t (:inherit font-lock-keyword-face))))
;;    (markdown-link-title-face              ((t (:inherit font-lock-comment-face))))
;;    (markdown-list-face                    ((t (:inherit font-lock-builtin-face))))
;;    (markdown-math-face                    ((t (:inherit font-lock-string-face))))
;;    (markdown-missing-link-face            ((t (:inherit font-lock-warning-face))))
;;    (markdown-pre-face                     ((t (:inherit font-lock-constant-face))))
;;    (markdown-reference-face               ((t (:inherit font-lock-type-face))))
;;    (markdown-url-face                     ((t (:inherit font-lock-string-face))))



     ;; w3m
     (w3m-anchor                          ((t ( :inherit link))))
     (w3m-bold                            ((t ( :inherit font-lock-keyword-face))))
     (w3m-form                            ((t ( :inherit font-lock-warning-face))))
     (w3m-form-button                     ((t ( :inherit custom-button))))
     (w3m-form-button-mouse               ((t ( :inherit custom-button-mouse))))
     (w3m-form-button-pressed             ((t ( :inherit custom-button-pressed))))
     (w3m-header-line-location-content    ((t ( :inherit mode-line))))
     (w3m-header-line-location-title      ((t ( :inherit mode-line))))
     (w3m-italic                          ((t ( :inherit font-lock-keyword-face))))
     (w3m-tab-background                  ((t ( :inherit mode-line))))

     ;; elscreen
     (elscreen-tab-background-face        ((t (:inherit mode-line))))
     (elscreen-tab-control-face           ((t (:background "#333" :foreground "#999" :height 90))))
     (elscreen-tab-current-screen-face    ((t (:background "#333" :foreground "#aaa" :height 90))))
     (elscreen-tab-other-screen-face      ((t (:background "#111" :foreground "#777" :height 90))))

     ;; linum-mode
     (linum                               ((t ( :foreground "grey25"))))

     ;; wb-line-number
     (wb-line-number-face                 ((t ( :foreground "#555"))))
     (wb-line-number-scroll-bar-face      ((t ( :background "#555" :foreground "#140b19"))))

     ;; auto-complete-mode
     (ac-completion-face  ((t (:foreground "#ffffff" :background "#5a4c62" underline t))))
     (ac-candidate-face  ((t (:foreground "#ffffff" :background "#5a4c62"))))
     (ac-candidate-mouse-face  ((t (:foreground "white" :background "blue"))))
     (ac-selection-face  ((t (:foreground "#000" :background "#fff"))))

     ;; e2wm
     (e2wm:face-files-directory           ((t (:foreground "#94c100"))))
     (e2wm:face-files-main                ((t (:foreground "CadetBlue"))))
     (e2wm:face-files-shadow              ((t (:foreground "grey50"))))
     (e2wm:face-files-symlink             ((t (:inherit font-lock-warning-face))))
     (e2wm:face-history-list-normal       ((t (:foreground "DarkSlateBlue"))))
     (e2wm:face-history-list-select1      ((t (:background "Lightsteelblue1" :foreground "OrangeRed"))))
     (e2wm:face-history-list-select2      ((t (:background "WhiteSmoke" :foreground "Blue"))))
     (e2wm:face-item                      ((t (:foreground "DarkSlateBlue" :family "Sans Serif"))))
     (e2wm:face-subtitle                  ((t (:height 1.2 :family "Sans Serif"))))
     (e2wm:face-title                     ((t (:bold nil :weight normal :height 1.5 :family "Sans Serif"))))

     ;; yascroll
     (yascroll:thumb-text-area            ((t ( :background "#e6ee00" :foreground "#e6ee00"))))
     (yascroll:thumb-fringe               ((t ( :background "#e6ee00" :foreground "#e6ee00"))))
     (yascroll:thumb-face                 ((t ( :background "#e6ee00" :foreground "#e6ee00"))))

)))

(add-to-list 'color-themes '(color-theme-user02-dark  "user03-dark" "QRG"))
