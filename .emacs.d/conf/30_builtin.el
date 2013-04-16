;;; builtin.el
(require 'cl)

;;###############################################################################
;;  SERVER
;;###############################################################################

;; server start for emacs-client
(require 'server)
;(setq server-auth-dir my-server-dir)
(unless (server-running-p)(server-start))

;; Using emacs --daemon and emacsclient I have often had cause to use emacs at the terminal,
;; but I like to have a colour scheme in my graphical frames that is unreadable in the console.
;; This code lives in my .emacs file and allows me to setup color and font settings for
;; graphical frames, but leave the console frames to use the default colour scheme.
;; I've found this very useful.  Tested with Emacs 24.0.50.1 @ 2010-20-07 -- Geoff Teale

(defun setup-window-system-frame-colours (&rest frame)
  (if window-system
      (let ((f (if (car frame)
                   (car frame)
                 (selected-frame))))
        (progn
          (set-frame-font "Bera Sans Mono-11")
          (set-face-background 'default "#232F2F" f)
          (set-face-foreground 'default "#FFFFFF" f)
          (set-face-background 'fringe  "#ffffff" f)
          (set-face-background 'cursor "#2F4F4F" f)
          (set-face-background 'mode-line "#2F4F4F" f)
          (set-face-foreground 'mode-line "#BCBf91" f)))))

(defadvice server-create-window-system-frame
  (after set-window-system-frame-colours ())
  "Set custom frame colours when creating the first frame on a display"
  (message "Running after frame-initialize")
  (setup-window-system-frame-colours))
(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions 'setup-window-system-frame-colours t)

;;###############################################################################
;;  PACKAGE
;;###############################################################################
;; package.el
(require 'package)

;; add melpa repository
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; add Marmalade repository
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; package install dir
(setq package-user-dir my-elpa-dir)
;; set load path installed packages
(package-initialize)

;; cookie file
(setq url-configuration-directory my-url-dir)

;;###############################################################################
;;  SESSIONS
;;###############################################################################

;; -----------------------------------------------------------------------------
;; auto backup
;; -----------------------------------------------------------------------------
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name my-autobackup-dir)) backup-directory-alist))
(setq make-backup-files t)                      ; Auto Backup file *.~
(setq version-control t)                        ; 複数の世代バックアップを残す
(setq kept-new-versions 5)                      ; 新しいものをいくつ残すか
(setq kept-old-versions 5)                      ; 古いものをいくつ残すか
(setq delete-old-versions t)                    ; 確認しないで古いものを削除する
(setq vc-make-backup-files t)                   ; バージョン管理下のファイルもバックアップを作る

;; backup ファイル に日付日時を付加する
;; (setq backup-by-copying t)
;; (defadvice make-backup-file-name
;;   (around modify-file-name activate)
;;   (let ((backup-dir "~/.emacs.d/auto-backup"))  ; 保存ディレクトリ
;;     (setq backup-dir (expand-file-name backup-dir))
;;     (unless (file-exists-p backup-dir)(make-directory-internal backup-dir))
;;     (if (file-directory-p backup-dir)
;;         (let* ((file-path (expand-file-name file))
;;                (chars-alist '((?/ . (?#))(?# . (?# ?#))(?: . (?\;))(?\; . (?\; ?\;))))
;;                (mapchars(lambda (c) (or (cdr (assq c chars-alist)) (list c)))))
;;           (setq ad-return-value
;;                 (concat backup-dir "/"
;;                         (mapconcat 'char-to-string
;;                                    (apply 'append
;;                                           (mapcar mapchars file-path)) ""))))
;;       ad-do-it)))

;; -----------------------------------------------------------------------------
;; auto-save
;; -----------------------------------------------------------------------------
; Number of input events between auto-saves. 0 means disable auto saving.
(setq auto-save-interval 60)
; Number of seconds idle time before auto-save. 0 means disable auto saving.
(setq auto-save-timeout 600)
(setq auto-save-list-file-prefix
      (expand-file-name my-autosave-prefix))
; Auto Saving file .#*
(setq auto-save-default t)


;; -----------------------------------------------------------------------------
;; history
;; -----------------------------------------------------------------------------
(setq message-log-max 100000)                    ; *Messages* に保持する行数

; [Emacs で最近開いたファイルのリストを自動保存する。 - 日々、とんは語る。](http://d.hatena.ne.jp/tomoya/20110217/1297928222)
(require 'recentf)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 2000)
(setq recentf-save-file (convert-standard-filename my-recentf-file))
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)
(recentf-mode 1)

(run-with-idle-timer 30 t 'recentf-save-list)

; save minibuffer history
(require 'savehist)

; all previous identical elements are deleted from the history list when adding a new history element
(eval-when-compile (require 'cl))
(defun minibuffer-delete-duplicate ()
  (let (list)
    (dolist (elt (symbol-value minibuffer-history-variable))
      (unless (member elt list)
        (push elt list)))
    (set minibuffer-history-variable (nreverse list))))
(setq history-delete-duplicates t)
(add-hook 'minibuffer-setup-hook 'minibuffer-delete-duplicate)

(setq history-length 1000000)                        ; mini-buffer history list length
(setq savehist-file my-history-file)        ; mini-buffer history file path
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; -----------------------------------------------------------------------------
;; save cursor place
;; -----------------------------------------------------------------------------
;; session.el が有効な場合、saveplace.el は無効化されるため、
;; 同機能を利用するには session.el 側の設定で行う
;; (require 'saveplace)
;; (setq save-place-file my-saveplace-file)
;; (setq-default save-place t)

;; -----------------------------------------------------------------------------
;; bookmark
;; -----------------------------------------------------------------------------
(require 'bookmark)
(setq bookmark-default-file my-bookmark-file)

;;###############################################################################
;;  EDIT
;;###############################################################################
(setq x-select-enable-clipboard t)               ; クリップボードにコピー

;; tab
(setq-default indent-tabs-mode nil)              ; t tab を使ってindentする | nil 半角スペースを使う
(setq-default tab-width 4 indent-tabs-mode nil)  ; タブ幅を 4 に設定
(setq tab-stop-list                              ; タブ幅の倍数を設定
    '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

;; indent
(global-set-key "\C-m" 'newline-and-indent)      ; enable auto indent with new line
(global-set-key "\C-m" 'indent-new-comment-line) ; enable auto indent and comment with new line


(setq kill-whole-line t)                         ; C-kで行全体を削除
(delete-selection-mode 1)                        ; BS や Delete キーでリージョン内の文字を削除

(setq completion-ignore-case t)                  ; 補完で大文字小文字の区別をしない
(setq read-file-name-completion-ignore-case t)   ; 補完で大文字小文字の区別をしない

(setq require-final-newline t)                   ; 最終行に必ず1行挿入する

(require 'cua-base)
(cua-mode t)
(setq cua-enable-cua-keys nil)


;;###############################################################################
;;  DISPLAY
;;###############################################################################

(setq transient-mark-mode t)             ; regionをハイライト
(setq scroll-step 1)                     ; 1行ずつスクロールする
(setq highlight-nonselected-windows t)   ; buffer を切り替えても region highlight を維持
(global-hl-line-mode)                    ; highlight the current line.

(setq echo-keystrokes 0.1)               ; show key strokes more quickly in mini-buffer
(fset 'yes-or-no-p 'y-or-n-p)            ; "yes or no"を"y or n"に

(require 'paren)
(show-paren-mode t)                      ; 対応する括弧の強調表示
(setq show-paren-style 'mixed)           ; 'parenthesis 括弧を強調表示 | 'expression 括弧内を強調表示 | 'mixed 括弧と括弧内を強調表示

;;; ------ 折り返しのデフォルト設定
;; Automatically becomes buffer-local when set in any fashion.
;; *Non-nil means do not display continuation lines.
;; Instead, give each line of text just one screen line.
;; Note that this is overridden by the variable `truncate-partial-width-windows'
;; if that variable is non-nil and this buffer is not full-frame width.
(setq-default truncate-lines t)

;; ------ ウィンドウ分割したときの折り返し設定
;; この変数の値が Non-nil だと、frame を分割したときの折り返しは
;; window ではなく frame 全体の幅を基準に折り返される
(setq-default truncate-partial-width-windows nil)


;; Delete all the trailing whitespace across the current buffer before save
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ------------------------------------------------------------------------------
;; whitespace and tab
;; + tab   									/
;; + space 　　　　 　　　　 　　　　 　　　　  /
;; ------------------------------------------------------------------------------
(require 'whitespace)
; see whitespace.el for more details
(setq whitespace-style '(face trailing tabs spaces newline space-mark tab-mark newline-mark))

;;(setq whitespace-space-regexp "\\(\u3000+\\)")
;;(setq whitespace-space-regexp "\\(　+\\)")
(setq whitespace-space-regexp "\\( +\\|\u3000+\\)")

(setq whitespace-display-mappings
      '(
        (space-mark ?\  [?\u25AF]       [?.])
        (space-mark ?\xA0	[?\u00A4]   [?_])  ; hard space - currency
        (space-mark ?\x8A0 [?\x8A4]     [?_])  ; hard space - currency
        (space-mark ?\x920 [?\x924]     [?_])  ; hard space - currency
        (space-mark ?\xE20 [?\xE24]     [?_])  ; hard space - currency
        (space-mark ?\xF20 [?\xF24]     [?_])  ; hard space - currency
        (space-mark ?　[?□]            [?＿]) ; full-width space - square

        ;; NEWLINE is displayed using the face `whitespace-newline'
        (newline-mark ?\n [?↲ ?\n])  ; eol - dollar sign

        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
;       (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]) ; tab - left quote mark
        (tab-mark ?\t [?\u21C0 ?\t] [?\\ ?\t]) ; ⇀
        ))

(set-face-bold-p 'whitespace-space nil)
(setq whitespace-global-modes '(not dired-mode tar-mode))
;; (global-whitespace-mode 1)


;;##############################################################################
;;  MOUSE
;;##############################################################################

;; copy region when dragged. (t) or not (nil)
(setq mouse-drag-copy-region t)

;; マウスのホイールスクロールスピードを調節
(global-set-key [wheel-up]
                '(lambda () "" (interactive) (scroll-down 1)))
(global-set-key [wheel-down]
                '(lambda () "" (interactive) (scroll-up 1)))
(global-set-key [double-wheel-up]
                '(lambda () "" (interactive) (scroll-down 1)))
(global-set-key [double-wheel-down]
                '(lambda () "" (interactive) (scroll-up 1)))
(global-set-key [triple-wheel-up]
                '(lambda () "" (interactive) (scroll-down 2)))
(global-set-key [triple-wheel-down]
                '(lambda () "" (interactive) (scroll-up 2)))

;;###############################################################################
;;  MISC
;;###############################################################################

(cd "~/")                                        ; start a emacs with $HOME dir
(setq ring-bell-function 'ignore)                ; ビープ音を消す
(auto-image-file-mode 1)                         ; visiting of image files as imagse

;; disable case sensitive
(custom-set-variables '(read-file-name-completion-ignore-case t))

;; Whether movement off the edge of the frame wraps around.
;; The minibuffer is skipped over in up/down movements if it is inactive.
(require 'windmove)
(setq windmove-wrap-around t)

;; ------------------------------------------------------------------------------
;; imenu-mode
;; ------------------------------------------------------------------------------
(require 'imenu)
(setq imenu-auto-rescan t)
(setq imenu-after-jump-hook (lambda () (recenter 10)))


;;###############################################################################
;;  AUTO-MODE-ALIST
;;###############################################################################
;; see `~/.emacs.d/conf/packages.el` about auto-mode-alist with packages mode

(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))



;;; coding: utf-8-unix
;;; End:
