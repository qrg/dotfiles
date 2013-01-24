;; ~/.emacs.d/conf/packages.el
;; ------------------------------------------------------------------------------

;;###############################################################################
;; BASE EXTENSIONS
;;###############################################################################

;; ------------------------------------------------------------------------------
;; auto-install.el --- Auto install elisp file
(require 'auto-install)
(setq auto-install-directory my-autoinstall-dir) ; path
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; ------------------------------------------------------------------------------
;; auto-async-byte-compile.el --- Automatically byte-compile when saved
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp my-local-dir) ; path
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; ------------------------------------------------------------------------------
;; moccur-edit.el --- apply replaces to multiple files
;; http://www.bookshelf.jp/elc/moccur-edit.el

;; (require 'moccur-edit)

;; ------------------------------------------------------------------------------
;; color-moccur.el ---  multi-buffer occur (grep) mode
;; http://www.bookshelf.jp/elc/color-moccur.el.
(require 'color-moccur)

;; ------------------------------------------------------------------------------
;; auto-complete.el --- Auto Completion for GNU Emacs
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/elpa/auto-complete-20121022.2254/dict")
(ac-config-default)

;; ------------------------------------------------------------------------------
;; popwin.el --- Popup Window Manager.
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-width 80)
(setq popwin:popup-window-position 'right)
;; (setq popwin:special-display-config
;;       ;; regexp:  width:  height:  position:  noselect:  stick:
;;       (append '(("*Remember*" :stick t)
;;                 ("*Org Agenda*")
;;                 ("*Backtrace*")
;;                 ("*sdic*" :noselect t)
;;                 (" *auto-async-byte-compile*" :noselect t :stick nil)
;;                 ("*anything*" :position left)
;; ;                ("*Evernote-Completions*" :width 150 :position left)
;;                 ("*Process List*" :noselect t)
;;                 ("*w3m*" :width 100 :position left :stick t)
;;                 ("*Moccur*" :width 140 :position left :stick t)
;;                 ) popwin:special-display-config))

;; ------------------------------------------------------------------------------
;; one-key.el --- Easy access configurable popup menus to display keybindings and other things.
;; (require 'one-key-default)                       ; one-key.el も一緒に読み込んでくれる
;; (require 'one-key-config)                        ; one-key.el をより便利にする
;; (one-key-default-setup-keys)                     ; one-key- で始まるメニュー使える様になる
;; (define-key global-map "\C-x" 'one-key-menu-C-x) ; C-x にコマンドを定義

;; ------------------------------------------------------------------------------
;; guide-key.el --- Guide the following key bindings automatically and dynamically
;; alternative one-key.el
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "M-r" "M-R" "C-c"))
(guide-key-mode 1)  ; guide-key-mode を有効にする

;; ------------------------------------------------------------------------------
;; e2wm.el --- simple window manager for emacs
;; ---> require window-layout.el
(require 'e2wm)


;;###############################################################################
;; ADDITIONAL EXTENSIONS
;;###############################################################################

;; ------------------------------------------------------------------------------
;; w3m.el --- an Emacs interface to w3m
(require 'w3m)
;; (require 'w3m-load)
(setq w3m-init-file nil)                          ; Your emacs-w3m startup file name. Nil means no init file will be loaded.
(setq w3m-home-page "http://www.google.co.jp/")   ; the page that opens when emacs-w3m startup
(setq w3m-use-cookies t)                          ; use cookie
(setq browse-url-browser-function 'w3m-browse-url)
;(setq w3m-search-default-engin "google-ja")
(autoload 'w3m-browse-url "w3m" "Ask a  browser to show a URL." t)
(autoload 'w3m-find-file  "w3m" "w3m interface function for local file." t)
(autoload 'w3m-search     "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
(autoload 'w3m-weather    "w3m-weather" "Display weather report." t)

(require 'w3m-session)
(setq w3m-session-time-format "%Y-%m-%d (%a) %H:%M")
(setq w3m-session-automatic-title "Automatic saved sessions")
(setq w3m-session-deleted-title "Removed sessions")
(setq w3m-session-crash-recovery-title "Crash recovery sessions")

;; ------------------------------------------------------------------------------
;; emacs-evernote : http://code.google.com/p/emacs-evernote-mode/
;; (require 'evernote-mode)
;; (setq enh-password-cache-file "~/.emacs.d/etc/evernote-mode/password.gpg")
;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
;; (setq evernote-username "qurage")

;; customize path
;; ~/.emacs.d/pkg/evernote-mode-0_41/ruby/bin/enclient.rb
;; line | description
;;  254 | LOG = Logger.new File.expand_path("~/.emacs.d/etc/evernote-mode/evernote-mode.log"), 3
;; 1252 |     ENMODE_SYS_DIR      = File.expand_path("~/.emacs.d/etc/evernote-mode") + "/"

;; *Non-nil means that password cache is enabled.
;; It is recommended to encrypt the file with EasyPG.
;; EasyPG は Emacs 23 以降には含まれています。
;; Emacs 22 では EasyPG をインストールし、.emacs に以下を記述してください。
;; gpg-agent を使うことで安全にパスワードをキャッシュできます。
;(require 'epa-setup)
;; (setq evernote-password-cache t)

;; ------------------------------------------------------------------------------
;; org-mode, org.el --- Outline-based notes management and organizer
(require 'org)
(require 'org-install)
(add-hook 'org-mode-hook 'turn-on-font-lock)             ; org-mode での強調表示を可能にする
;(setq org-hide-leading-stars t)                         ; 見出しの余分な * を消す
(setq org-directory my-org-dir)                          ; path org-default-notes-file のディレクトリ
(setq org-default-notes-file my-org-file)                ; org-default-notes-file のファイル名


;; ------------------------------------------------------------------------------
;; howm
;; (mapc
;;  (lambda (f)
;;    (autoload f
;;      "howm" "Hitori Otegaru Wiki Modoki" t))
;;  '(howm-menu howm-list-all howm-list-recent
;;              howm-list-grep howm-create
;;              howm-keyword-to-kill-ring))


;;###############################################################################
;; EDITING
;;###############################################################################

;; ------------------------------------------------------------------------------
;; recentf-ext.el --- Recentf extensions
(require 'recentf-ext)

;; ------------------------------------------------------------------------------
;; session.el --- use variables, registers and buffer places across sessions
(when (require 'session nil t)
  (setq session-save-file-coding-system 'utf-8-unix)
  (setq session-save-file (expand-file-name my-session-file)) ; path

    (setq session-initialize '(session keys menus places)) ;; `de-saveplace` disables saveplace.el

  (setq session-globals-max-size 1024)
  (setq session-globals-max-string (* 10240 10240))
  (setq session-globals-include '((kill-ring 512)
                                  (session-file-alist 512)
                                  (file-name-history 10000)
                                  (tags-table-set-list 128)
                                  (tags-table-list 128)))
  (add-hook 'after-init-hook 'session-initialize)
  ;; Save session info every 1 minutes
  (setq my-timer-for-session-save-session (run-at-time t 60 'session-save-session)))

;; ------------------------------------------------------------------------------
;; redo+
(require 'redo+)
(setq undo-no-redo t)
(setq undo-limit 90000)
(setq undo-strong-limit 90000)

;; ------------------------------------------------------------------------------
;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; ------------------------------------------------------------------------------
;; jaspace.el --- make Japanese whitespaces visible
;; whitespace.el のほうを使う
;; * tab   				  /
;; * space 　　　　　　　　　 /
;; (require 'jaspace)
;; (setq jaspace-alternate-jaspace-string "☐")    ; or any other string 　全角空白
;; (setq jaspace-alternate-eol-string "↲\n")       ; 改行記号
;; (setq jaspace-highlight-tabs t)
;; (setq jaspace-highlight-tabs "⇀")                ; use ⇀  as a tab marker

;; ------------------------------------------------------------------------------
;; linum.el --- Display line numbers to the left of buffers
(require 'linum)
(setq linum-format "%6d ") ; 5桁とスペースの領域を割り当てる

;; enable linum-mode in all buffers where `linum-on' would do it.
;; global 表示しちゃうと help とか info にも表示されて微妙だった
;(global-linum-mode t)

; linum.el で emacs の 左側に行番号を表示しよう - わからん
; ≫ http://d.hatena.ne.jp/kitokitoki/20100714/p1
; メジャーモード/マイナーモードでの指定
(defvar my-linum-hook-name nil)
(mapc
 (lambda (hook-name)(add-hook hook-name (lambda () (linum-mode t))))
 my-linum-hook-name)
(setq my-linum-hook-name
      '(emacs-lisp-mode-hook
        slime-mode-hook
        sh-mode-hook
        text-mode-hook
        php-mode-hook
        python-mode-hook
        ruby-mode-hook
        css-mode-hook
        yaml-mode-hook
        howm-mode-hook
        js2-mode-hook
        javascript-mode-hook
        smarty-mode-hook
        html-helper-mode-hook
        markdown-mode-hook))
; ファイル名での判定
(defvar my-linum-file nil)
(defun my-linum-file-name ()
  (when (member (buffer-name) my-linum-file)
                (linum-mode t)))
(add-hook 'find-file-hook 'my-linum-file-name)
(setq my-linum-file '("hosts" "my_site"))
; 拡張子での判定
(defvar my-linum-file-extension nil)
(defun my-linum-file-extension ()
  (when (member (file-name-extension (buffer-file-name)) my-linum-file-extension)
                (linum-mode t)))
(add-hook 'find-file-hook 'my-linum-file-extension)
(setq my-linum-file-extension '("conf" "bat"))


;; ------------------------------------------------------------------------------
;; scratch-log.el --- Utility for *scratch* buffer.
(require 'scratch-log)

(setq sl-scratch-log-file my-scratch-log-file)
(setq sl-prev-scratch-string-file my-prev-scratch-string-file)

;; nil なら emacs 起動時に，最後に終了したときの スクラッチバッファの内容を復元しない。初期値 t
;; (setq sl-restore-scratch-p nil)
;; nil なら スクラッチバッファを削除できるままにする。初期値 t
;; (setq sl-prohibit-kill-scratch-buffer-p nil)




;;###############################################################################
;; SCROLLING
;;###############################################################################

;; ------------------------------------------------------------------------------
;; yascroll.el --- Yet Another Scroll Bar Mode
;; [2011-04-01 - Functional Emacser](http://d.hatena.ne.jp/m2ym/20110401)
;; linum-modeやjaspace-modeとの共存で問題がある (2011-04-01)
(require 'yascroll)

;; Delay to hide scroll bar in seconds. nil means never hide scroll bar.
;; default 0.5
(setq yascroll:delay-to-hide 2.0)

;; Overlays for scroll bar thumb.
;; このオプションで Overlays が何をしてるのかまだいまいちよくわからない。
(setq yascroll:thumb-overlays nil)

;; enable or disable yascroll global
(global-yascroll-bar-mode 1)

;; ------------------------------------------------------------------------------
;; smooth-scroll.el --- Minor mode for smooth scrolling.
;  スクロールが重たくなる
;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)

;; ------------------------------------------------------------------------------
;; smooth-scrolling.el
;; (require 'smooth-scrolling)
;; (setq scroll-step 1)
;; (setq scroll-conservatively 10000)
;; (setq smooth-scroll-margin 15)

;; ------------------------------------------------------------------------------
;; inertial-scroll.el --- global minor mode for inertial scrolling
(require 'inertial-scroll)

;; 初速. 大きいほど一気にスクロールする
(setq inertias-initial-velocity 80)

;; 摩擦抵抗. 大きいほどすぐ止まる
;; Frictional coefficient (lines/sec^2). The larger value stops
;; scrolling more quickly.
(setq inertias-friction 120)

;; 画面描画のwait時間. msec
;; Interval time for scrolling (millisecond).
;; The smaller value makes scrolling smoother,
;; but the emacs needs more machine power.
(setq inertias-update-time 8)

;; 画面端でのバウンド量 0はバウンドしない。1.0で弾性反発
;; Restitusion coefficient.
;; The value 1.0 means elastic rebounding and 0.0 does viscous.
(setq inertias-rest-coef 0)

;; Rebounding flash effect at buffer edges. If nil, no flash effect is shown.
(setq inertias-rebound-flash nil)

(setq inertias-global-minor-mode-map
      (inertias-define-keymap
       '(
         ;; Mouse wheel scrolling
         ("<wheel-up>"   . inertias-down-wheel)
         ("<wheel-down>" . inertias-up-wheel)
         ("<mouse-4>"    . inertias-down-wheel)
         ("<mouse-5>"    . inertias-up-wheel)
         ;; Scroll keys
         ("<next>"  . inertias-up)
         ("<prior>" . inertias-down)
         ("C-v"     . inertias-up)
         ("M-v"     . inertias-down)
         ) inertias-prefix-key))

(inertias-global-minor-mode 1)

;; ------------------------------------------------------------------------------
;; tramp.el --- Transparent Remote Access, Multiple Protocol
(require 'tramp)
(setq tramp-auto-save-directory my-tramp-autosave-dir)

;;(setq tramp-debug-buffer t)
(setq tramp-verbose 10)

(setq-default tramp-default-method "sshx")
(setq-default tramp-persistency-file-name nil)

;; ------------------------------------------------------------------------------
;; ecb.el --- a code browser for Emacs
(require 'ecb)
(defun ecb-toggle ()
    (interactive)
      (if ecb-minor-mode
                (ecb-deactivate)
            (ecb-activate)))
(global-set-key [f2] 'ecb-toggle)

;;###############################################################################
;; ANYTHING
;;###############################################################################

;; ------------------------------------------------------------------------------
;; anything.el --- open anything / QuickSilver-like candidate-selection framework
(require 'anything)
(require 'anything-startup)
(require 'anything-c-moccur)
;(setq anything-c-adaptive-history-file "~/.emacs.d/var/anything-c-adaptive-history")
(setq w3m-bookmark-file "~/.w3m/bookmark.html")


;;###############################################################################
;; AUTO MODE ALIST
;;###############################################################################
;; see `~/.emacs.d/config/builtin.el` about auto-mode-alist with builtin mode
(add-to-list 'auto-mode-alist '("\\.text"   . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt"    . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md"     . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn"   . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdt"    . markdown-mode))
(add-to-list 'auto-mode-alist '("INSTALL"   . markdown-mode))
(add-to-list 'auto-mode-alist '("README"    . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.org$"   . org-mode))
