; ~/.emacs.d/conf/appearance.el
; -------------------------------------------------------------------------------

;;###############################################################################
;; UI
;;###############################################################################

(setq inhibit-startup-message t)            ;; hide startup message
(tool-bar-mode -1)                          ;; hide tool bar
(menu-bar-mode 1)                           ;; hide menu bar
(scroll-bar-mode -1)                        ;; hide scroll bar
(setq frame-title-format "Emacs %b ( %f )") ;; title bar

(add-hook 'find-file-hook (lambda () (ruler-mode 1))) ;; C-x C-f したときに ruler-mode を適用する

;; line spaceing
;; 整数で指定するとピクセル数、少数で指定すると行の高さに対して相対値で設定
(setq-default line-spacing 0.2)

;;###############################################################################
;; MODE-LINE
;;###############################################################################
;; 改行コードの表記
(setq eol-mnemonic-dos  "(CRLF-dos)"
      eol-mnemonic-mac  "(CR-mac)"
      eol-mnemonic-unix "(LF-unix)")

(setq line-number-mode t)      ;; Enable or disable the display of the current line number, see also LineNumbers
(setq column-number-mode t)    ;; Enable or disable the display of the current column number
(setq size-indication-mode t)  ;; Enable or disable the current buffer size, Emacs 22 and later, see Manual:size-indication-mode
(setq which-function-mode 1)   ;; display functions name at current line

;; (setq mode-line-position
;;        '(:eval (format "L%%l/%d,C%%c" (count-lines (point-max)(point-min)))))

;; display time
(require 'time)
(defface custom-display-time
  '((((type x w32 mac))
     (:foreground "#66b3b7"))
    (((type tty))
     (:foreground "cyan")))
  "Face used to display the time in the mode line."
  :group 'mode-line)
(setq display-time-string-forms
      '((propertize (concat " " (substring year -4) "." month "." day " " dayname " " 24-hours ":" minutes " ")
                    'face 'custom-display-time)))
(display-time-mode 1)

;; display current directory at mode-line
;; like `L100/200  C10  15k`
(let ((ls (member 'mode-line-buffer-identification  mode-line-format)))
  (setcdr ls (cons '(:eval (concat " " (abbreviate-file-name default-directory) " ")) (cdr ls))))

(setq mode-line-position
      '(:eval (format "L%%l/%d  C%%c  %%I" (count-lines (point-max)(point-min)))))



;;###############################################################################
;; THEME
;;###############################################################################
; IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook
        (lambda() (set-cursor-color "#00c8ff")))
(add-hook 'input-method-inactivate-hook
        (lambda() (set-cursor-color "#fff")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/elpa/molokai-theme-20120503.1929/")
(load-theme 'molokai t)
(enable-theme 'molokai)
