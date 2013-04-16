; ~/.emacs.d/local/conf/local.el
; ------------------------------------------------------------------------------

;; mozc
;; http://d.hatena.ne.jp/kitokitoki/20120925/p2
;; (require 'mozc) ; or (load-file "/path/to/mozc.el")
;; (setq default-input-method "japanese-mozc")
;; (setq mozc-candidate-style 'overlay)

;; mozc
(when (require 'mozc nil t)
  (setq default-input-method "japanese-mozc")

;; すぐ下に変換候補を表示
(setq mozc-candidate-style 'overlay)
;; (setq mozc-candidate-style 'echo-area) ; default, エコーエリアに表示

;; 変換キーでon、無変換キーでoffで切り替え
(global-set-key
 [henkan]
 (lambda () (interactive)
   (when (null current-input-method) (toggle-input-method))))

(global-set-key
 [muhenkan]
 (lambda () (interactive)
   (inactivate-input-method)))
(defadvice mozc-handle-event (around intercept-keys (event))
  "Intercept keys muhenkan and zenkaku-hankaku, before passing keys to mozc-server (which the function mozc-handle-event does), to properly disable mozc-mode."
  (if (member event (list 'zenkaku-hankaku 'muhenkan))
      (progn (mozc-clean-up-session)
             (toggle-input-method))
    (progn ;(message "%s" event) ;debug
      ad-do-it)))

(ad-activate 'mozc-handle-event))

;; ibus-mode
;; Emacsで、日本語や中国語などの文字列をIBusを経由して入力するためのマイナーモード
;; "irie @ ウィキ - ibus.el" http://www11.atwiki.jp/s-irie/pages/21.html
;; ibus-mode を有効化するためには、XIM を無効化する必要がある
;;   1. ~/.Xresources に Emacs24*useXIM: false を追記
;;      + emacs24 の場合の対処方法 (2012.11.27)
;;        - http://d.hatena.ne.jp/sandmark/20121127/1354046530
;;   2. $ xrdb ~/.Xresources を実行
;;
;; エラー "IBus: IMContext ID is mismatched." が出たりして挙動が不安定

;; (require 'ibus)
;; (add-hook 'after-init-hook 'ibus-mode-on)

;; IBusの状態によってカーソル色を変化させる ("on" "off" "disabled")
;; (setq ibus-cursor-color '("magenta" "white" "cyan"))

;; isearch 時はカーソルを塗りのない長方形にする
;; (setq ibus-isearch-cursor-type 'hollow)

;; ibus-anthy
;; 半角英数モードをトグルするために C-j を使う
;; (ibus-define-common-key ?\C-j t)

;; ibus-skk
;; C-jでひらがなモードに戻せるようにする
;; (ibus-define-common-key ?\C-j t)

;; ibus-mozc
;; 予測変換ウィンドウの表示をカーソル位置に調整
;; (setq ibus-prediction-window-position t)
