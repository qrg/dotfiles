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
(setq popwin:special-display-config
      ;; regexp:  width:  height:  position:  noselect:  stick:
      (append '(("*Remember*" :stick t)
                ("*Org Agenda*")
                ("*Backtrace*")
                ("*sdic*" :noselect t)
                (" *auto-async-byte-compile*" :noselect t :stick nil)
                ("*anything*" :position left)
;                ("*helm for files*" :position left)
;                ("*Evernote-Completions*" :width 150 :position left)
                ("*Process List*" :noselect t)
                ("*w3m*" :width 100 :position left :stick t)
                ("*Moccur*" :width 140 :position left :stick t)
                ) popwin:special-display-config))

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
;; GIT
;;###############################################################################

;; git-gutter.el --- Port of Sublime Text 2 plugin GitGutter
(require 'git-gutter)
(setq git-gutter:modified-sign " ") ; two space
(setq git-gutter:added-sign    "+") ; multiple character is OK
(setq git-gutter:deleted-sign  "-")

(set-face-foreground 'git-gutter:modified "white")
(set-face-background 'git-gutter:modified "#5faf13")
(set-face-foreground 'git-gutter:added    "white")
(set-face-background 'git-gutter:added    "#00a2d3")
(set-face-foreground 'git-gutter:deleted  "white")
(set-face-background 'git-gutter:deleted  "#ff2600")

;; diff-hl.el --- Highlight uncommitted changes
(when (require 'diff-hl))

;;###############################################################################
;; ADDITIONAL EMACS EXTENSIONS
;;###############################################################################

;; ------------------------------------------------------------------------------
;; w3m.el --- an Emacs interface to w3m
;; (when (require 'w3m)
;;   ;; (require 'w3m-load)
;;   (setq w3m-init-file nil)                          ; Your emacs-w3m startup file name. Nil means no init file will be loaded.
;;   (setq w3m-home-page "http://www.google.co.jp/")   ; the page that opens when emacs-w3m startup
;;   ;(require 'w3m-cookie)
;;   ;(setq w3m-use-cookies t)                          ; use cookie

;;   (setq w3m-bookmark-file "~/.w3m/bookmark.html")
;;   (setq browse-url-browser-function 'w3m-browse-url)
;;   ;(setq w3m-search-default-engin "google-ja")
;;   (autoload 'w3m-browse-url "w3m" "Ask a  browser to show a URL." t)
;;   (autoload 'w3m-find-file  "w3m" "w3m interface function for local file." t)
;;   (autoload 'w3m-search     "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
;;   (autoload 'w3m-weather    "w3m-weather" "Display weather report." t)

;;   (require 'w3m-session)
;;   (setq w3m-session-time-format "%Y-%m-%d (%a) %H:%M")
;;   (setq w3m-session-automatic-title "Automatic saved sessions")
;;   (setq w3m-session-deleted-title "Removed sessions")
;;   (setq w3m-session-crash-recovery-title "Crash recovery sessions"))

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
(when (require 'org)
  (require 'org-install)
  (add-hook 'org-mode-hook 'turn-on-font-lock)             ; org-mode での強調表示を可能にする
  ;(setq org-hide-leading-stars t)                         ; 見出しの余分な * を消す
  (setq org-directory my-org-dir)                          ; path org-default-notes-file のディレクトリ
  (setq org-default-notes-file my-org-file)                ; org-default-notes-file のファイル名
)

;; ------------------------------------------------------------------------------
;; howm
;; (mapc
;;  (lambda (f)
;;    (autoload f
;;      "howm" "Hitori Otegaru Wiki Modoki" t))
;;  '(howm-menu howm-list-all howm-list-recent
;;              howm-list-grep howm-create
;;              howm-keyword-to-kill-ring))


;; ------------------------------------------------------------------------------
;; powerline.el --- fancy statusline
;;
;;  * emacs 用 powerline は 2 種類あり、今回は後者の fork 版を利用
;;
;;    + powerline.el --- Rewrite of Powerline
;;      - https://github.com/milkypostman/powerline
;;      - package.el で入手できる
;;
;;    + powerline.el --- fancy statusline
;;      - https://github.com/jonathanchu/emacs-powerline
;;      - milkypostman powerline の fork 版
;;      - git submodule add でマニュアルインストール
;;
;;    + cf.) http://hico-horiuchi.com/wiki/doku.php?id=emacs:powerline
;;

(require 'powerline)
(setq powerline-arrow-shape 'arrow)   ;; the default
;; (setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
;; (setq powerline-arrow-shape 'arrow14) ;; best for small fonts

(custom-set-faces
 '(mode-line ((t (:foreground "gray10" :background "gray90" :box nil))))
 '(mode-line-inactive ((t (:foreground "gray50" :background "gray10" :box nil)))))

(setq powerline-color1 "gray22")
(setq powerline-color2 "gray38")

(setq-default mode-line-format
    (list "%e"
        '(:eval (concat
             (powerline-rmw            'left   nil  )
             (powerline-buffer-id      'left   nil  powerline-color1  )
             (powerline-major-mode     'left        powerline-color1  )
             (powerline-minor-modes    'left        powerline-color1  )
             (powerline-narrow         'left        powerline-color1  powerline-color2  )
             (powerline-vc             'center                        powerline-color2  )
             (powerline-make-fill                                     powerline-color2  )
             (powerline-row            'right       powerline-color1  powerline-color2  )
             (powerline-make-text      ":"          powerline-color1  )
             (powerline-column         'right       powerline-color1  )
             (powerline-percent        'right  nil  powerline-color1  )
             (powerline-make-text      "  "    nil  )))))

;; modeline format
;;   %b -- print buffer name.      %f -- print visited file name.
;;   %F -- print frame name.
;;   %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;;         %& is like %*, but ignore read-only-ness.
;;         % means buffer is read-only and * means it is modified.
;;         For a modified read-only buffer, %* gives % and %+ gives *.
;;   %s -- print process status.   %l -- print the current line number.
;;   %c -- print the current column number (this makes editing slower).
;;         To make the column number update correctly in all cases,
;;         `column-number-mode' must be non-nil.
;;   %i -- print the size of the buffer.
;;   %I -- like %i, but use k, M, G, etc., to abbreviate.
;;   %p -- print percent of buffer above top of window, or Top, Bot or All.
;;   %P -- print percent of buffer above bottom of window, perhaps plus Top,
;;         or print Bottom or All.
;;   %n -- print Narrow if appropriate.
;;   %t -- visited file is text or binary (if OS supports this distinction).
;;   %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
;;   %Z -- like %z, but including the end-of-line format.
;;   %e -- print error message about full memory.
;;   %@ -- print @ or hyphen.  @ means that default-directory is on a
;;         remote machine.
;;   %[ -- print one [ for each recursive editing level.  %] similar.
;;   %% -- print %.   %- -- print infinitely many dashes.


(defpowerline buffer-id (propertize (car (propertized-buffer-identification "%b"))
                                    'face (powerline-make-face color1)))
(defpowerline rowcol   "%l:%c") ; 行+カラム番号
(defpowerline percent  "%p")    ; カーソル位置の割合
(defpowerline time     "%M")    ; 時計書式
(defpowerline filesize "%I")    ; ファイルサイズ

;; 右部分の位置合わせ(右端から何文字分を左に寄せるか、デフォルト+15文字)
(defun powerline-make-fill (color)
  (let ((plface (powerline-make-face color)))
    (if (eq 'right (get-scroll-bar-mode))
      (propertize " " 'display '((space :align-to (- right-fringe 36))) 'face plface)
      (propertize " " 'display '((space :align-to (- right-fringe 39))) 'face plface))))

;; Powerline 書式
(setq-default mode-line-format (list
 '("-" mode-line-mule-info mode-line-modified)
 '(:eval (concat
           (powerline-buffer-id   'left   nil powerline-color1)
           (powerline-major-mode  'left       powerline-color1)
           (powerline-minor-modes 'left       powerline-color1)
           (powerline-narrow      'left       powerline-color1 powerline-color2)
           (powerline-vc          'center                      powerline-color2)
           (powerline-make-fill                                powerline-color2)
           (powerline-rowcol      'right      powerline-color1 powerline-color2)
           (powerline-percent     'right      powerline-color1)
           (powerline-filesize    'right      powerline-color1)
           (powerline-time        'right  nil powerline-color1)
           (powerline-make-text   "  "    nil )
           ))))

;; 時計 フォーマット
(setq display-time-string-forms '((format
  "%s/%s(%s) %s:%s" month day dayname 24-hours minutes)))
;; 時計を表示
(display-time-mode 1)

;; 改行コードの表記
(setq eol-mnemonic-dos  " CRLF"
      eol-mnemonic-mac  " CR"
      eol-mnemonic-unix " LF")

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
(when (require 'linum)
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
  (setq my-linum-file-extension '("conf" "bat")))


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

(defface yascroll:thumb-text-area
  '((t (:background "slateblue")))
  "Face for text-area scroll bar thumb."
  :group 'yascroll)

(defface yascroll:thumb-fringe
  '((t (:background "slateblue" :foreground "slateblue")))
  "Face for fringe scroll bar thumb."
  :group 'yascroll)

(custom-set-faces
     '(yascroll:thumb-text-are ((t (:background "skyblue")))))
(custom-set-faces
     '(yascroll:thumb-fringe ((t (:background "skyeblue" :foreground "skyblue")))))

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
(when (require 'tramp)
    (setq tramp-auto-save-directory my-tramp-autosave-dir)

    ;;(setq tramp-debug-buffer t)
    (setq tramp-verbose 10)

    (setq-default tramp-default-method "sshx")
    (setq-default tramp-persistency-file-name nil)
)

;; ------------------------------------------------------------------------------
;; ecb.el --- a code browser for Emacs
(when (require 'ecb)
    (defun ecb-toggle ()
        (interactive)
          (if ecb-minor-mode
                    (ecb-deactivate)
                (ecb-activate)))
    (global-set-key [f2] 'ecb-toggle)
)

;;###############################################################################
;; APPEARANCE
;;###############################################################################
;; col-highlight.el --- Highlight the current column.
;; To use this file, you must also have library `vline.el'.
;;
;;  * この手の拡張はどうしてもマルチバイト文字が入るとずれてしまうのが問題
;;
(when (require 'col-highlight)

  ;; 重いので常時表示はしない
  ;; (colmn-highlight-mode 1)

  ;; 一定時間操作がない場合にハイライト (秒)
  (toggle-highlight-column-when-idle t)
  (col-highlight-set-interval 0.3)

  ;; color
  (custom-set-faces
       '(col-highlight((t (:inherit hl-line)))))
)

;;###############################################################################
;; HELM
;;###############################################################################
(require 'helm-config)
(helm-mode 1)

(require 'helm-c-moccur)

(when (require 'helm-git nil t)(global-set-key (kbd "C-x C-g") 'helm-git-find-files))
(when (require 'helm-ls-git nil t))
(when (require 'helm-themes nil t))
(when (require 'helm-c-moccur nil t)
    (global-set-key (kbd "M-o") 'helm-c-moccur-occur-by-moccur)
    (global-set-key (kbd "C-M-o") 'helm-c-moccur-dmoccur)
    (add-hook 'dired-mode-hook
        '(lambda () (local-set-key (kbd "O") 'helm-c-moccur-dired-do-moccur-by-moccur)))
    (global-set-key (kbd "C-M-s") 'helm-c-moccur-isearch-forward)
    (global-set-key (kbd "C-M-r") 'helm-c-moccur-isearch-backward))
(when (require 'helm-migemo nil t)(define-key global-map [(control ?:)] 'helm-migemo))

;(setq w3m-cookie-file "~/.w3m/cookie")

;; ------------------------------------------------------------------------------
;; anything.el --- open anything / QuickSilver-like candidate-selection framework
;; helm に乗り換え
;; (require 'anything)
;; (require 'anything-startup)
;; (require 'anything-c-moccur)
;(setq anything-c-adaptive-history-file "~/.emacs.d/var/anything-c-adaptive-history")



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
