
;;; -*- coding: utf-8 -*-

(defvar auto-install-package-list
  '(ammonite-term-repl
    ensime
    mozc-im
    multi-term
    org
    org-ac
    org-ehtml
    powerline
    rspec-mode
    ruby-compilation
    ruby-extra-highlight
    ruby-factory
    sbt-mode
    scala-mode
    scalariform
    shell-command
    shell-history
    shell-pop
    shelldoc
    total-lines
    tramp
    xterm-color
    flycheck-pycheckers
    json-mode
    json-rpc
    look-mode dumb-jump
    flycheck-popup-tip
    ac-html
    flycheck-status-emoji
    ac-c-headers
    auto-complete-c-headers
    pandoc
    doom-themes
    flycheck
    git
    bash-completion
    slim-mode
    ac-slime
    mozc
    adoc-mode
    rust-mode
    ac-emoji
    ac-html-bootstrap
    ac-mozc
    esh-autosuggest
    fcitx
    web-mode
    web-server
    websocket
    markdown-mode
    exec-path-from-shell
    ruby-additional
    ruby-electric
    ruby-end
    ruby-refactor
    auto-complete)
  "Packages to be installed")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; auto-installation
(dolist (pkg auto-install-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; dracula-themeを使用してたけど、Synthwaveがきになったのでこっちを使う
;; (load-theme 'dracula t)
(load-theme 'doom-laserwave t)

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

;; C-hをBackSpaceに設定
(global-set-key (kbd "C-h") 'delete-backward-char)

;; enable/disable Mozc with "C-,"
(global-set-key (kbd "C-,") 'toggle-input-method)

;; タイトルにフルパスを表示
(setq frame-title-format "%f")

;; スタートアップメッセージを削除
(setq inhibit-startup-message 1)

;; 初期メッセージを消去
(setq initial-scratch-message "")

;; 言語設定
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; テキストファイル・新規バッファの文字コード
(set-file-name-coding-system 'utf-8)

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

;; yesと入力するのは面倒なのでyで十分
(defalias 'yes-or-no-p 'y-or-n-p)

;; バックアップファイルを~/.emacs.d/ehistに保存
(setq backup-directory-alist '((".*" . "~/.emacs.d/ehist")))

;; フォント設定
(cond ((display-graphic-p)
       ;; 半角英字設定
       (set-face-attribute 'default nil :family "NotoSansMono Nerd Font" :height 150)

       (setq use-default-font-for-symbols nil)
       ;; 日本語フォント設定
       (set-fontset-font (frame-parameter nil 'font)
                         'japanese-jisx0208
                         (font-spec :family "Noto Sans Mono CJK JP" :size 18))
       (set-fontset-font (frame-parameter nil 'font)
                         'japanese-jisx0208-1978
                         (font-spec :family "Noto Sans Mono CJK JP" :size 18))
       (set-fontset-font (frame-parameter nil 'font)
                         'japanese-jisx0212
                         (font-spec :family "Noto Sans Mono CJK JP" :size 18))
       (set-fontset-font (frame-parameter nil 'font)
                         'japanese-jisx0213-1
                         (font-spec :family "Noto Sans Mono CJK JP" :size 18))
       (set-fontset-font (frame-parameter nil 'font)
                         'japanese-jisx0213-2
                         (font-spec :family "Noto Sans Mono CJK JP" :size 18))
       (set-fontset-font (frame-parameter nil 'font)
                         'japanese-jisx0213.2004-1
                         (font-spec :family "Noto Sans Mono CJK JP" :size 18))
       (set-fontset-font (frame-parameter nil 'font)
                         'jisx0201
                         (font-spec :family "Noto Sans Mono CJK JP" :size 18))
       (set-fontset-font (frame-parameter nil 'font)
                         'kana
                         (font-spec :family "Noto Sans Mono CJK JP" :size 18))
       ;; Emoji Setting(WIP)
       (set-fontset-font (frame-parameter nil 'font)
                       '(#x1F300 . #x1F9FF)
                       (font-spec :family "NotoSansMono Nerd Font" :size 18))
       ))

;; ERB mode
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; asciidoc alias
(autoload 'adoc-mode "adoc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

;; markdown 
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; Ruby2.0以上を使う場合は、coding utf-8 のマジックコメントを書く必要がないので、自動挿入機能を無効にする。
;;(defun ruby-mode-set-encoding () nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(package-selected-packages
   (quote
    (ammonite-term-repl ensime mozc-im multi-term org org-ac org-ehtml powerline rspec-mode ruby-compilation ruby-extra-highlight ruby-factory sbt-mode scala-mode scalariform shell-command shell-history shell-pop shelldoc total-lines tramp xterm-color flycheck-pycheckers json-mode json-rpc look-mode dumb-jump flycheck-popup-tip ac-html flycheck-status-emoji ac-c-headers auto-complete-c-headers pandoc doom-themes flycheck git bash-completion slim-mode ac-slime mozc adoc-mode rust-mode ac-emoji ac-html-bootstrap ac-mozc esh-autosuggest fcitx web-mode web-server websocket markdown-mode exec-path-from-shell projectile-rails ruby-additional ruby-electric ruby-end ruby-refactor auto-complete)))
 '(ruby-insert-encoding-magic-comment nil))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
; use flycheck
(add-hook 'ruby-mode-hook #'flycheck-mode)
                                        

;; GUI Settings
(if (not window-system)
  ;;  (set-frame-parameter nil 'alpha 90)
  ;; ツールバーを削除
  (tool-bar-mode 0)
  ;; メニューバーを削除
  (menu-bar-mode 0)
  ;; 現在行をアンダーライン
  (setq hl-line-face 'underline)
  (global-hl-line-mode))

;; Launch on linux
(when (eq system-type 'gnu/linux)
  ;; 日本語や絵文字を全角にして表示
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (require 'eaw)
  (eaw-fullwidth)
  ;; shellのPATHを引継ぐ
  (exec-path-from-shell-initialize))

;; auto-complete
(require 'auto-complete nil t)
(require 'auto-complete-config nil t)
;; enable AC global 
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "M-n") 'ac-next)  ; M-n, next suggest
(define-key ac-completing-map (kbd "M-p") 'ac-previous) ; M-p, previous suggest
(setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
    ;; また、Emacs Lispモードではac-source-symbolsを追加で利用
    (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols t)))
    ;; 以下、自動で補完する人用
    (setq ac-auto-start 3)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

