(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

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
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; テキストファイル・新規バッファの文字コード
(set-file-name-coding-system 'utf-8)

;; C-hをBackSpaceに設定
(global-set-key (kbd "C-h") 'delete-backward-char)

;; タイトルにフルパスを表示
(setq frame-title-format "%f")

;; スタートアップメッセージを削除
(setq inhibit-startup-message 1)

;; 初期メッセージを消去
(setq initial-scratch-message "")

;; 日本語に設定する
(set-language-environment "UTF-8")

;; IDOを有効化
(ido-mode 1)

;; 鬼軍曹を追加
;; (add-to-list 'load-path "~/.emacs.d/drill-instructor")
;; (require 'drill-instructor)
;; (setq drill-instructor-global t)

;; shellの文字化けを回避
(add-hook 'shell-mode-hook
          (lambda()
            (set-buffer-process-coding-system 'utf-8 'utf-8)
            ))

;; ビープ音を消す
(setq ring-bell-function 'ignore)

;; バックアップファイルを~/.emacs.d/ehistに保存
(setq backup-directory-alist '((".*" . "~/.emacs.d/ehist")))

(cond ((display-graphic-p)
       ;; 半角英字設定
       (set-face-attribute 'default nil :family "NotoSansMono Nerd Font" :height 150)
       ;(add-to-list 'default-frame-alist '(font . "NotoSansMono Nerd Font-14" ))

       ;; 全角かな設定 Linuxではjapanese-jisx0213となる
       (set-fontset-font (frame-parameter nil 'font)
                         'japanese-jisx0213-2
                         (font-spec :family "Noto Sans CJK JP" :size 20))
       
       ;; 半角ｶﾅ設定 Linuxではkatakana-jisx0213でも問題ない
       (set-fontset-font (frame-parameter nil 'font)
                         'katakana-jisx0201
                         (font-spec :family "Noto Sans CJK JP" :size 20)))
      (t 0))

;; GUI Settings
(if window-system
    (set-frame-parameter nil 'alpha 90)
  ;; ツールバーを削除
  (tool-bar-mode 0)
  ;; メニューバーを削除
  (menu-bar-mode 0))

;; linux()
(when (eq system-type 'gnu/linux)
  ;; 日本語や絵文字を全角にして表示
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (require 'eaw)
  (eaw-fullwidth))


(require 'auto-complete)
(require 'auto-complete-config)
;; enable AC global 
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "M-n") 'ac-next)  ; M-n, next suggest
(define-key ac-completing-map (kbd "M-p") 'ac-previous) ; M-p, previous suggest
(setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
    ;; また、Emacs Lispモードではac-source-symbolsを追加で利用
    (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols t)))
    ;; 以下、自動で補完する人用
    (setq ac-auto-start 3)


;; Rails configration
;; (require 'projectile)
;; (projectile-global-mode)
;; (require 'projectile-rails)
;; (add-hook 'projectile-mode-hook 'projectile-rails-on)


;; ;; shellのPATHを引継ぐ
;; (exec-path-from-shell)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(package-selected-packages
   (quote
    (rust-mode projectile-rails ruby-additional ruby-electric ruby-end ruby-refactor auto-complete dracula-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



