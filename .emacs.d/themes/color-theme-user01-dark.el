(eval-when-compile
  (require 'color-theme))

(defun color-theme-user01-dark ()
  "Color theme by QRG, created 2010-08-15."
  (interactive)
  (color-theme-install
   '(color-theme-user01-dark
     ((background-mode . dark)
      (foreground-color . "#ccc")
      (background-color . "#000")
      (border-color . "#000")
      (cursor-color . "#fff")
      (mouse ((t (:inherit highlight)))))
     ((goto-address-mail-face . italic)
      (goto-address-mail-mouse-face . secondary-selection)
      (goto-address-url-face . link)
      (goto-address-url-mouse-face . highlight)
      (list-matching-lines-buffer-name-face . underline)
      (list-matching-lines-face . match)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (nil))))
     (bold ((t (:weight normal))))
     (bold-italic ((t (:italic t :slant italic :weight normal))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))
     (button ((t (:underline t))))
     (calendar-today ((t (:underline t))))
     (completions-common-part ((t (:background "#000"
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
     (completions-first-difference ((t (:bold t :weight bold))))
     (custom-button ((t (:background "DeepPink2"
                         :foreground "White"
                         :box (:line-width 1 :color "DeepPink2")))))
     (custom-button-mouse ((t (:background "#96cedd"
                               :foreground "#333"
                               :box (:line-width 1 :color "#222")))))
     (custom-button-pressed ((t (:background "#fff"
                                 :foreground "#000"
                                 :box 1))))
     (custom-button-pressed-unraised ((default (:inherit custom-button-unraised))
                                      (((class color) (background dark)) (:foreground "#a5d5e2"))))
     (custom-button-unraised ((t (:inherit underline))))
     (custom-state ((((class color) (background dark)) (:foreground "#74ab00"))))
     (custom-variable-tag ((((class color) (background dark)) (:foreground "#a5d5e2"))))
     (diary ((t (:foreground "yellow1"))))
     (e2wm:face-files-directory ((t (:foreground "#94c100"))))
     (e2wm:face-files-main ((t (:foreground "CadetBlue"))))
     (e2wm:face-files-shadow ((t (:foreground "grey50"))))
     (e2wm:face-files-symlink ((t (:inherit font-lock-warning-face))))
     (e2wm:face-history-list-normal ((t (:foreground "DarkSlateBlue"))))
     (e2wm:face-history-list-select1 ((t (:background "Lightsteelblue1" :foreground "OrangeRed"))))
     (e2wm:face-history-list-select2 ((t (:background "WhiteSmoke" :foreground "Blue"))))
     (e2wm:face-item ((t (:foreground "DarkSlateBlue" :family "Sans Serif"))))
     (e2wm:face-subtitle ((t (:height 1.2 :family "Sans Serif"))))
     (e2wm:face-title ((t (:bold t :weight bold :height 1.5 :family "Sans Serif"))))
     (elscreen-tab-background-face ((t (:inherit mode-line))))
     (elscreen-tab-control-face ((t (:background "#333" :foreground "#999" :height 90))))
     (elscreen-tab-current-screen-face ((t (:background "#333" :foreground "#aaa" :height 90))))
     (elscreen-tab-other-screen-face ((t (:background "#111" :foreground "#777" :height 90))))
     (escape-glyph ((t (:foreground "brown"))))
     (ffap ((t (:inherit highlight))))
     (file-name-shadow ((t (:foreground "grey50"))))
     (fixed-pitch ((t (:inherit default))))
     (font-lock-builtin-face ((t (:foreground "#2aadd5"))))
     (font-lock-comment-delimiter-face ((t (:background "#111" :foreground "#888"))))
     (font-lock-comment-face ((t (:background "#111" :foreground "#888"))))
     (font-lock-constant-face ((t (:foreground "CadetBlue"))))
     (font-lock-doc-face ((t (:foreground "#f2ff00"))))
     (font-lock-function-name-face ((t (:foreground "#94c100"))))
     (font-lock-keyword-face ((t (:foreground "#ec3e5b"))))
     (font-lock-negation-char-face ((t (nil))))
     (font-lock-preprocessor-face ((t (:foreground "Orchid"))))
     (font-lock-regexp-grouping-backslash ((t (:weight normal))))
     (font-lock-regexp-grouping-construct ((t (:weight normal))))
     (font-lock-string-face ((t (:foreground "RosyBrown"))))
     (font-lock-type-face ((t (:foreground "ForestGreen"))))
     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
     (font-lock-warning-face ((t (:foreground "DeepPink" :weight normal))))
     (fringe ((t (:background "#000"))))
     (header-line ((t (:height 90 :background "#333" :foreground "#999" :box nil))))
     (help-argument-name ((t (nil))))
     (highlight ((t (:background "#2aadd5" :foreground "white"))))
     (holiday ((t (:background "chocolate4"))))
     (info-header-node ((t (:inherit default))))
     (info-header-xref ((t (:inherit link))))
     (info-xref-visited ((t (:inherit link-visited))))
     (info-menu-header ((t (:family "Sans Serif" :height 2.0))))
     (info-menu-star ((t (:inherit font-lock-warning-face))))
     (info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
     (info-title-1 ((t (:bold t :family "Sans Serif" :height 4.0 :weight bold))))
     (info-title-2 ((t (:bold t :family "Sans Serif" :height 3.0 :weight bold))))
     (info-title-3 ((t (:family "Sans Serif" :height 2.0))))
     (info-title-4 ((t (:family "Sans Serif" :height 1.4))))
     (info-xref ((t (:inherit link))))
     (isearch ((t (:background "#f7ff00" :foreground "#000"))))
     (isearch-fail ((t (:background "#e6005c" :foreground "#fff"))))
     (italic ((t (:italic t :slant italic))))
     (jaspace-highlight-eol-face ((t (:foreground "gray32"))))
     (jaspace-highlight-jaspace-face ((t (:foreground "gray32"))))
     (jaspace-highlight-tab-face ((t (:foreground "gray32" :strike-through nil))))
     (lazy-highlight ((t (:background "#fff" :foreground "#000"))))
     (link ((t (:inherit font-lock-builtin-face :underline t))))
     (link-visited ((t (:foreground "#789dab" :underline t))))
     (linum ((t (:foreground "grey35"))))
     (match ((t (:inherit isearch))))
     (menu ((t (:background "#000" :foreground "#aaa"))))
     (minibuffer-prompt ((t (:inherit font-lock-warning-face))))
     (mode-line ((t (:background "gray12" :foreground "gray65" :height 100))))
     (mode-line-buffer-id ((t (:weight normal))))
     (mode-line-emphasis ((t (:weight normal))))
     (mode-line-highlight ((t (:background "black" :box nil))))
     (mode-line-inactive ((t (:background "gray12" :foreground "gray28" :box nil :weight light))))
     (next-error ((t (:background "lightgoldenrod2"))))
     (nobreak-space ((t (:foreground "brown" :underline t))))
     (one-key-keystroke ((t (:inherit font-lock-function-name-face))))
     (query-replace ((t (:background "magenta3" :foreground "lightskyblue1"))))
     (region ((t (:inherit highlight))))
     (ruler-mode-column-number ((t (:inherit ruler-mode-default :box (:line-width 1)))))
     (ruler-mode-current-column ((t (:inherit ruler-mode-default
                                     :background "#e6005c"
                                     :foreground "#e6005c"
                                     :box (:line-width 1)
                                     ))))
     (ruler-mode-default ((t (:inherit default
                              :background "#111"
                              :foreground "#888"
                              :weight normal
                              :underline t
                              :box (:line-width 1 :color "#111" :style released-button)
                              ))))
     (scroll-bar ((t (:background "red" :foreground "systemscrollbar"))))
     (secondary-selection ((t (:background "yellow1"))))
     (shadow ((t (:foreground "grey50"))))
     (show-paren-match ((t (:foreground "#e6005c" :weight normal))))
     (show-paren-mismatch ((t (:background "purple" :foreground "#ddd"))))
     (tool-bar ((t (:background "systembuttonface"
                    :foreground "systembuttontext"
                    :box (:line-width 1
                          :style released-button)))))
     (tooltip ((t (:background "systeminfowindow"
                   :foreground "systeminfotext"
                   :slant normal :weight normal
                   :height 90
                   :width normal
                   :foundry "outline"
                   :family "mono"))))
     (trailing-whitespace ((t (:background "red"))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "Sans Serif"))))
     (vertical-border ((t (nil))))
     (w3m-anchor ((t (:inherit link))))
     (w3m-bold ((t (:inherit bold))))
     (w3m-form ((t (:inherit font-lock-warning-face))))
     (w3m-form-button ((t (:inherit custom-button))))
     (w3m-form-button-mouse ((t (:inherit custom-button-mouse))))
     (w3m-form-button-pressed ((t (:inherit custom-button-pressed))))
     (w3m-header-line-location-content ((t (:inherit mode-line))))
     (w3m-header-line-location-title ((t (:inherit mode-line))))
     (w3m-tab-background ((t (:inherit mode-line))))
     (wb-line-number-face ((t (:foreground "#555"))))
     (wb-line-number-scroll-bar-face ((t (:background "#555" :foreground "#000"))))
     (which-func ((t (:inherit font-lock-builtin-face))))
     (whitespace-empty ((t (:background "yellow" :foreground "firebrick"))))
     (whitespace-hspace ((t (:background "grey24" :foreground "aquamarine3"))))
     (whitespace-indentation ((t (:background "yellow" :foreground "firebrick"))))
     (whitespace-line ((t (:background "gray20" :foreground "violet"))))
     (whitespace-newline ((t (:foreground "darkgray" :weight normal))))
     (whitespace-space ((t (:background "grey20" :foreground "aquamarine3"))))
     (whitespace-space-after-tab ((t (:background "yellow" :foreground "firebrick"))))
     (whitespace-space-before-tab ((t (:background "DarkOrange" :foreground "firebrick"))))
     (whitespace-tab ((t (:background "grey22" :foreground "aquamarine3"))))
     (whitespace-trailing ((t (:bold t :background "red1" :foreground "yellow" :weight bold))))
     (widget-button ((t (:bold t :weight bold))))
     (widget-button-pressed ((t (:inherit font-lock-warning-face))))
     (widget-documentation ((t (:foreground "dark green"))))
     (widget-field ((t (:background "#222"))))
     (widget-inactive ((t (:foreground "#888"))))
     (widget-single-line-field ((t (:background "#eee")))))))