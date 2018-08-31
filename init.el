(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; auto-completeを適用
(el-get-bundle auto-complete)

;; dracula-themeを使用 
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; 行を表示
(column-number-mode t)

;; 列を表示
(global-linum-mode t)
(setq linum-format "%5d|")

;; C-kで1行切り取り
(setq kill-whole-line t)

;; カーソルの点滅を止める
(blink-cursor-mode 0)

;; タブ文字の幅を設定
(setq-default tab-width 4)

;; インデント文字をタブではなく空白に設定
(setq-default indent-tabs-mode nil)

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)

;; テキストファイル・新規バッファの文字コード
(set-file-name-coding-system 'utf-8-unix)

;; メニューバーを削除
(menu-bar-mode 0)

;; ツールバーを削除
(tool-bar-mode 0)

;; C-hをBackSpaceに設定
(global-set-key (kbd "C-h") 'delete-backward-char)

;; タイトルにフルパスを表示
(setq frame-title-format "%f")

;; スタートアップメッセージを削除
(setq inhibit-startup-message 1)

;; 初期メッセージを消去
(setq initial-scratch-message "")

;; 日本語に設定する
(set-language-environment "Japanese")

;; IDOを有効化
(ido-mode 1)

;; shellの文字化けを回避
(add-hook 'shell-mode-hook
          (lambda()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
            ))

;; ビープ音を消す
(setq ring-bell-function 'ignore)

;; linux
(when (eq system-type 'gnu/linux)
  ;; 日本語や絵文字を全角にして表示
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (require 'eaw)
  (eaw-fullwidth)
  
  ;; 固定幅フォント
  (set-face-attribute 'fixed-pitch nil: family "Noto Sans Mono CJK JP")

  ;; 可変幅フォント
  (set-face-attribute 'variable-pitch nil: family "TakaoPGothic"))


;; Windows
(when (eq system-type 'windows-nt)
  ;; 固定等幅フォント
  (set-face-attribute 'fixed-pitch nil: family "MS Gothic")

  ;; 可変幅フォント
  (set-face-attribute 'variable-pitch nil: family "Yu Gothic UI"))

;; Mac OS
(when (eq system-type 'darwin)
  ;; 固定幅フォント
  (set-face-attribute 'fixed-pitch nil: family "Osaka-Mono")

  ;;可変幅フォント
  (set-face-attribute 'variable-pitch nil: family "Osaka")

  ;; ウィンドウの透明度を上げる
  (add-to-list 'default-frame-alist '(alpha. (0.5 0.5))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;n; If there is more than one, they won't work right.
 )
