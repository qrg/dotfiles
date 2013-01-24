; ~/.emacs.d/init.el
; ------------------------------------------------------------------------------

;;###############################################################################
;; LANGUAGE
;;###############################################################################
(set-language-environment "Japanese")
(set-keyboard-coding-system 'japanese-shift-jis)

; 文字コード UTF-8 改行コード LF
;(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
; 極力UTF-8とする
(prefer-coding-system 'utf-8-unix)

;;###############################################################################
;; FONT
;;###############################################################################
; ~^|\!"#$%`&)/*?(){}[]<>_-+.,
; 01234567890123456789
; abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTU
; いろはにほへと ちりぬるを  わかよたれそ   つねならむ
; うゐのおくやま けふこえて  あさきゆめみし ゑひもせす
; 色は匂へど     散りぬるを  我が世誰ぞ     常ならん
; 有為の奥山     境越えて    浅き夢見じ     酔ひもせず

(set-frame-font     "HrgnConsolas 10")                                ; 標準フォント   /  "DjvSans-TkoG-Mono_dark 10" "DjvSAKA-mono 10"
(set-face-attribute 'fixed-pitch    nil :family "HrgnConsolas 10" )   ; 固定等幅フォント
(set-face-attribute 'variable-pitch nil :family "Helvetica" )         ; 可変幅フォント
;; (set-fontset-font   "fontset-default"
;;                     'japanese-jisx0208
;;                     '("DjvSAKA-mono 10" . "jisx0208-sjis"))
;; (set-fontset-font   "fontset-default"
;;                     'katakana-jisx0201
;;                     '("DjvSAKA-mono 10" . "jisx0201-katakana"))


;;###############################################################################
;; INITIALIZATION
;;###############################################################################
(setq initial-frame-alist
    (append '((width  . 263)
              (height . 68)
              (top          . 0)
              (left         . 0)
              (cursor-type  . box)
              (left-fringe  . 20)
              (right-fringe . 20)
              ;(internal-border-width . 30)
              (alpha        . (97 75 100 100))
              (scroll-bar-width . 14)
              (vertical-scroll-bars . left)
              (ime-font     . "DjvSAKA-mono-10")
              ;(font         . "DjvSans-TkoG-Mono_dark-10")   ;;  (font . "DjvSAKA-等幅-10") (DjvSAKA-mono-10)
             )
    initial-frame-alist)
)

;; trancparency
;(if window-system (progn (set-frame-parameter nil 'alpha 98)))


;;###############################################################################
;; PATH
;;###############################################################################
(defconst my-emacs-dir "~/.emacs.d"
  "Path to Emacs directory.")
(defconst my-conf-dir        (concat my-emacs-dir "/conf")
  "Path to Emacs settings directory.")
(defconst my-local-dir       (concat my-emacs-dir "/local")
  "Path to local settings or files directory.")
(defconst my-package-dir     (concat my-emacs-dir "/packages")
  "Path to elisp packages directory.")
(defconst my-theme-dir       (concat my-emacs-dir "/themes")
  "Path to Emacs color theme directory.")

(defconst my-elpa-dir        (concat my-package-dir "/elpa")
  "Path to the directory package installed by ELPA.")
(defconst my-autoinstall-dir (concat my-package-dir "/auto-install")
  "Path to the directory package installed by auto-install.el.")

(defconst my-autosave-prefix (concat my-local-dir "/auto-save/save-")
  "Prefix text for auto save files.")
(defconst my-autobackup-dir  (concat my-local-dir "/auto-backup")
  "Path to the directory for auto backup files.")
(defconst my-saveplace-file  (concat my-local-dir "/places")
  "Path to the file for the cursor places file.")
(defconst my-recentf-file    (concat my-local-dir "/recentf")
  "Path to recentf file.")
(defconst my-history-file    (concat my-local-dir "/history")
  "Path to history file.")
(defconst my-bookmark-file   (concat my-local-dir "/bookmark")
  "Path to bookmark file for emacs-w3m.el.")
(defconst my-server-file     (concat my-local-dir "/server")
  "Path to Emacs server file.")
(defconst my-url-dir         (concat my-local-dir "/url")
  "Path to url file for emacs-w3m.el.")

(defconst my-session-file    (concat my-local-dir "/session")
  "Path to session file for session.el.")

(defconst my-autocomplete-dict-dir (concat my-elpa-dir "/auto-complete-20121022.2254/dict")
  "Path to auto complete dictionary file for auto-complete.el.")

(defconst my-org-dir  (concat my-local-dir "/org")
  "Path to the directory for org-mode")
(defconst my-org-file (concat my-local-dir "/notes.org")
  "Path to the file for org")

(defconst my-scratch-log-file (concat my-local-dir "/scratch-log")
  "Path to scratch log file")

(defconst my-prev-scratch-string-file (concat my-local-dir "/scratch-log-prev")
  "Path to the file for org")

(defconst my-markdown-command "perl ~/script/Markdown_1.0.1/Markdown.pl"
  "Path to markdown command.")
(defconst my-tramp-autosave-dir (concat my-local-dir "/auto-backup")
  "Path to the directory for tramp auto save files.")

;; load-path

(let ((default-directory (expand-file-name my-package-dir)))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(let ((default-directory (expand-file-name my-local-dir)))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; load local settings

(if (file-exists-p (concat my-local-dir "/conf/local.el"))
    (load (concat my-local-dir "/conf/local.el")))



;; -----------------------------------------------------------------------------
;; additional info directory path
;; -----------------------------------------------------------------------------

(require 'info)
(add-to-list 'Info-additional-directory-list "/usr/local/info")
(add-to-list 'Info-additional-directory-list "/usr/local/share/info")
(add-to-list 'Info-additional-directory-list "/usr/share/info")
(add-to-list 'Info-additional-directory-list "/usr/info")
(add-to-list 'Info-additional-directory-list "~/.emacs.d/info")

;;###############################################################################
;; INIT LOADER
;;###############################################################################

(require 'init-loader)
(init-loader-load my-conf-dir)
(setq init-loader-directory my-conf-dir)

;;  デフォルト設定の場合,以下の順序で引数に渡したディレクトリ以下のファイルをロードする.
;; 引数が省略された場合は,変数`init-loader-directory'の値を使用する.デフォルトは"~/.emacs.d/inits".

;; 1. ソートされた,二桁の数字から始まるファイル. e.x, "00_utils.el" "01_ik-cmd.el" "21_javascript.el" "99_global-keys.el"
;; 2. meadowの場合, meadow から始まる名前のファイル. e.x, "meadow-cmd.el" "meadow-config.el"
;; 3. carbon-emacsの場合, carbon-emacs から始まる名前のファイル. e.x, "carbon-emacs-config.el" "carbon-emacs-migemo.el"
;; 4. windowシステム以外の場合(terminal), nw から始まる名前のファイル e.x, "nw-config.el"

;; ファイルロード後,変数`init-loader-show-log-after-init'の値がnon-nilなら,
;; ログバッファを表示する関数を`after-init-hook'へ追加する.
;; ログの表示は, M-x init-loader-show-log でも可能.
;; デフォルト設定の場合,以下の順序で引数に渡したディレクトリ以下のファイルをロードする.
;; 引数が省略された場合は,変数`init-loader-directory'の値を使用する.デフォルトは"~/.emacs.d/inits".

;; 1. ソートされた,二桁の数字から始まるファイル. e.x, "00_utils.el" "01_ik-cmd.el" "21_javascript.el" "99_global-keys.el"
;; 2. meadowの場合, meadow から始まる名前のファイル. e.x, "meadow-cmd.el" "meadow-config.el"
;; 3. carbon-emacsの場合, carbon-emacs から始まる名前のファイル. e.x, "carbon-emacs-config.el" "carbon-emacs-migemo.el"
;; 4. windowシステム以外の場合(terminal), nw から始まる名前のファイル e.x, "nw-config.el"

;; ファイルロード後,変数`init-loader-show-log-after-init'の値がnon-nilなら,ログバッファを表示する関数を`after-init-hook'へ追加する.
;; ログの表示は, M-x init-loader-show-log でも可能.


;;; coding: utf-8-unix
;;; End:
