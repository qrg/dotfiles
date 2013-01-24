;; func.el
;; -----------------------------------------------------------------------------

; 現在カーソルがある場所の文字の face を mini-buffer に表示
(defun describe-face-at-point ()
    "Return face used at point."
    (interactive)
    (message "%s" (get-char-property (point) 'face)))


;; -----------------------------------------------------------------------------
;; *scratch* buffer
;; -----------------------------------------------------------------------------

;; [VimからEmacsに乗り換えたので便利機能紹介します - 八発白中]
;; (http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html)
(defun my-make-scratch (&optional arg)
  ;; *scratch*を消さない
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))
