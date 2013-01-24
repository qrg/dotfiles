
;; ~/.emacs.d/conf/keybind.el
;; -----------------------------------------------------------------------------
(global-unset-key (kbd "C-M-S-x"))
(global-unset-key (kbd "C-M-S-q"))

;; C-x C-c
;; (global-set-key   (kbd "C-M-S-x") 'save-buffers-kill-terminal)

;; Undo
(global-unset-key (kbd "C-/"))
(global-set-key   (kbd "C-z") 'undo )
;; Redo
(global-unset-key (kbd "C-S-/"))
(global-set-key   (kbd "C-S-z") 'redo )

;; select all
(global-set-key (kbd "C-a") 'mark-whole-buffer )

;; eval
(defun eval-buffer-or-region ()
  "If Transient Mark mode is on and a region is active,
Execute the region as Lisp code / eval-region.
Otherwise Execute the current buffer as Lisp code / eval-buffer."
  (interactive)
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (eval-region (region-beginning) (region-end))
    (eval-buffer)))

(global-set-key (kbd "C-M-e") 'eval-buffer-or-region)

;; kill the characters from the cursor to the beginning of the current line
(global-set-key (kbd "C-S-k") (kbd "C-u 0 C-k"))

;; both of `C-x k` and `C-x C-k` works as `kill-buffer`
(global-set-key "\C-x\C-k" 'kill-buffer)

;(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-x\C-b" 'anything-buffers+)


(define-prefix-command 'meta-r-prefix)
(global-set-key "\M-r" 'meta-r-prefix)

(global-set-key "\M-r\M-r" 'query-replace)
(global-set-key "\M-r\M-e" 'query-replace-regexp)
(global-set-key "\M-r\M-R" 'replace-string)
(global-set-key "\M-r\M-E" 'replace-regexp)

;; -----------------------------------------------------------------------------
;; Toggle Truncate Lines
;; -----------------------------------------------------------------------------

;; [ubulog: Emacsで行の折り返し表示をON/OFFする]
;; http://ubulog.blogspot.com/2007/09/emacsonoff.html
;; (defun toggle-truncate-lines ()
;;   "折り返し表示をトグル動作します."
;;   (interactive)
;;   (if truncate-lines (setq truncate-lines nil)
;;       (setq truncate-lines t))
;;       (recenter))

;; (global-set-key "\C-c\C-l" 'toggle-truncate-lines)

;; -----------------------------------------------------------------------------
;; Window and Buffer
;; -----------------------------------------------------------------------------

(global-set-key "\M-2" 'make-frame)
(global-set-key "\M-0" 'delete-frame)

(global-set-key     "\M-I"     'beginning-of-buffer)
(global-set-key     "\M-K"     'end-of-buffer)

(global-set-key (kbd "M-L") 'bs-cycle-next)
(global-set-key (kbd "M-J") 'bs-cycle-previous)
(global-set-key [C-M-prior] 'bs-cycle-next)
(global-set-key [C-M-next] 'bs-cycle-previous)

;;; select the windows
(global-unset-key (kbd "M-i" ))(global-unset-key (kbd "M-k" ))
(global-unset-key (kbd "M-j" ))
(global-unset-key (kbd "M-l" ))
(global-set-key   (kbd "M-i" ) 'windmove-up )
(global-set-key   (kbd "M-k" ) 'windmove-down )
(global-set-key   (kbd "M-j" ) 'windmove-left )
(global-set-key   (kbd "M-l" ) 'windmove-right )

;; [分割したウィンドウの大きさをインタラクティヴに変更する - mooz deceives you]
;; (http://d.hatena.ne.jp/mooz/20100119/p1)
;; [Re: 分割したウィンドウの大きさをインタラクティヴに変更する - とりあえず暇だったし何となく始めたブログ]
;; (http://d.hatena.ne.jp/khiker/20100119/window_resize)
;; jklh 以外のキーを押せば、 window-resizer は終了する

(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)                           ; vi style `l`
               (enlarge-window-horizontally dx))
              ((= c ?j)                           ; vi style `h`
               (shrink-window-horizontally dx))
              ((= c ?k)                           ; vi style `j`
               (enlarge-window dy))
              ((= c ?i)                           ; vi style `k`
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-event (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

(global-set-key "\C-\M-w" 'my-window-resizer)



;; [ubulog: Emacsで複数のウィンドウを開いた状態を記憶しておき、いつでも復元できるようにする]
;; (http://ubulog.blogspot.com/2007/09/emacs_13.html)
;;====================================
;; 分割したウィンドウ状態を保存・復元する
;;===================================
;; ;;デフォルトは C-c C-w
;; (require 'windows)
;; ;; 新規にフレームを作らない
;; (setq win:use-frame nil)
;; (win:startup-with-window)
;; (define-key ctl-x-map "C" 'see-you-again)

;; -----------------------------------------------------------------------------
;; scroll
;; -----------------------------------------------------------------------------
;;;
;;; half scrolling and let a cursor go to the top and the end

(global-set-key [next]     ; -------- PageDown
                (lambda () (interactive)
                  (condition-case nil (scroll-up (/ (window-height) 2))
                    (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]    ; -------- PageUp
                (lambda () (interactive)
                  (condition-case nil (scroll-down (/ (window-height) 2))
                    (beginning-of-buffer (goto-char (point-min))))))

;; Ask Web browser to load a URL clicked with the mouse
(global-set-key [S-mouse-2] 'browse-url-at-mouse)

;; -----------------------------------------------------------------------------
;; C-aでインデントを飛ばした行頭に移動
;; [VimからEmacsに乗り換えたので便利機能紹介します - 八発白中]
;; http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html
;; M-m でもインデントを除いた行頭に移動はできる。
;; -----------------------------------------------------------------------------
(defun beginning-of-indented-line (current-point)
  "インデント文字を飛ばした行頭に戻る。ただし、ポイントから行頭までの間にインデント文字しかない場合は、行頭に戻る。"
  (interactive "d")
  (if (string-match
       "^[ ¥t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun beginning-of-visual-indented-line (current-point)
  "インデント文字を飛ばした行頭に戻る。ただし、ポイントから行頭までの間にインデント文字しかない場合は、行頭に戻る。"
  (interactive "d")
  (let ((vhead-pos (save-excursion (progn (beginning-of-visual-line) (point))))
        (head-pos (save-excursion (progn (beginning-of-line) (point)))))
    (cond
     ;; 物理行の1行目にいる場合
     ((eq vhead-pos head-pos)
      (if (string-match
           "^[ ¥t]+$"
           (buffer-substring-no-properties vhead-pos current-point))
          (beginning-of-visual-line)
        (back-to-indentation)))
     ;; 物理行の2行目以降の先頭にいる場合
     ((eq vhead-pos current-point)
      (backward-char)
      (beginning-of-visual-indented-line (point)))
     ;; 物理行の2行目以降の途中にいる場合
     (t (beginning-of-visual-line)))))

(global-set-key "\C-a" 'beginning-of-visual-indented-line)
(global-set-key "\C-e" 'end-of-visual-line)



;; -----------------------------------------------------------------------------
;; font sizing with Ctrl key and mouse scroll
;; -----------------------------------------------------------------------------
(defun font-big ()
    (interactive)
    (set-face-attribute 'default nil :height
                        (+ (face-attribute 'default :height) 10)))
(defun font-small ()
    (interactive)
    (set-face-attribute 'default nil :height
                        (- (face-attribute 'default :height) 10)))

(global-set-key (kbd "<C-wheel-down>") 'font-small)
(global-set-key (kbd "<C-wheel-up>") 'font-big)

;; -----------------------------------------------------------------------------
;; anything
;; -----------------------------------------------------------------------------
(global-set-key "\C-xrl" 'anything-bookmarks)
(global-set-key "\C-b"   'anything-bookmarks)
(global-set-key "\C-\M-f" 'anything-for-files)

(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x C-z") 'anything-buffers+)

;; -----------------------------------------------------------------------------
;; whitespace
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-x w") 'global-whitespace-mode)

;; -----------------------------------------------------------------------------
;; emacs-w3m
;; -----------------------------------------------------------------------------
(global-set-key "\C-cs" 'w3m-search)
(global-set-key "\C-xm" 'browse-url-at-point)

; ------------------------------------------------------------------------------
; linum-mode
(define-key global-map [f9] 'linum-mode)

;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------
;(define-key global-map "\C-cl" 'org-store-link)
;(define-key global-map "\C-ca" 'org-agenda)
;(define-key global-map "\C-cr" 'org-remember)

;; -----------------------------------------------------------------------------
;; howm
;; -----------------------------------------------------------------------------
;(global-set-key "\C-c,," 'howm-menu) ; howm
;(global-set-key (kbd "M-+") 'e2wm:start-management) ; e2wm

;; -----------------------------------------------------------------------------
;; emacs-evernote-mode
;; -----------------------------------------------------------------------------
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)


;; ------------------------------------------------------------------------------
;; e2wm.el --- simple window manager for emacs
;; ---> require window-layout.el
(global-set-key (kbd "M-+") 'e2wm:start-management)
