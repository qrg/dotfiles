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

;; mode-line の設定を Powerline に移行
;; 改行コードの表記
;; (setq eol-mnemonic-dos  " (CRLF-dos)"
;;       eol-mnemonic-mac  " (CR-mac)"
;;       eol-mnemonic-unix " (LF-unix)")

;; (setq line-number-mode t)      ;; Enable or disable the display of the current line number, see also LineNumbers
;; (setq column-number-mode t)    ;; Enable or disable the display of the current column number
;; (setq size-indication-mode t)  ;; Enable or disable the current buffer size, Emacs 22 and later, see Manual:size-indication-mode
;; (setq which-function-mode 1)   ;; display functions name at current line

;; (setq mode-line-position
;;        '(:eval (format "L%%l/%d,C%%c" (count-lines (point-max)(point-min)))))

;; display time
;; (require 'time)
;; (defface custom-display-time
;;   '((((type x w32 mac))(:foreground "#66b3b7"))
;;     (((type tty))(:foreground "cyan")))
;;   "Face used to display the time in the mode line."
;;   :group 'mode-line)

;; (setq display-time-string-forms
;;       '((propertize (concat " " (substring year -4) "." month "." day " " dayname " " 24-hours ":" minutes " ")
;;                     'face 'custom-display-time)))

;; (display-time-mode 1)


;; ;; display current directory at mode-line
;; (let ((ls (member 'mode-line-buffer-identification  mode-line-format)))
;;   (setcdr ls (cons '(:eval (concat " " (abbreviate-file-name default-directory) " ")) (cdr ls))))

;; ;; display current line numbers
;; ;; e.g.) `L100/200  C10  15k`
;; (setq mode-line-position
;;       '(:eval (format "L%%l/%d  C%%c  %%I" (count-lines (point-max)(point-min)))))



;;###############################################################################
;; FACE, THEME
;;###############################################################################
; IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook
        (lambda() (set-cursor-color "magenta")))
(add-hook 'input-method-inactivate-hook
        (lambda() (set-cursor-color "white")))

;; diff ------------------------------------------------------------------------
;; cf.) http://www.clear-code.com/blog/2012/4/3.html

(defun diff-mode-setup-faces ()
;; 追加された行は緑で表示
  (set-face-attribute 'diff-added nil
                      :foreground "white" :background "#268ebe")
  ;; 削除された行は赤で表示
  (set-face-attribute 'diff-removed nil
                      :foreground "white" :background "#ed3b62")
  ;; 文字単位での変更箇所は色を反転して強調
  (set-face-attribute 'diff-refine-change nil
                      :foreground nil :background nil
                      :weight 'bold :inverse-video t))
(add-hook 'diff-mode-hook 'diff-mode-setup-faces)

;; diffを表示したらすぐに文字単位での強調表示も行う
(defun diff-mode-refine-automatically ()
  (diff-auto-refine-mode t))
(add-hook 'diff-mode-hook 'diff-mode-refine-automatically)

;; diff関連の設定
(defun magit-setup-diff ()
  ;; diffを表示しているときに文字単位での変更箇所も強調表示する
  ;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
  (setq magit-diff-refine-hunk 'all)
  ;; diff用のfaceを設定する
  (diff-mode-setup-faces)
  ;; diffの表示設定が上書きされてしまうのでハイライトを無効にする
  (set-face-attribute 'magit-item-highlight nil :inherit nil))
(add-hook 'magit-mode-hook 'magit-setup-diff)





(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'molokai-mod t)
(enable-theme 'molokai-mod)
