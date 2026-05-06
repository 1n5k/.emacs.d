(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("946e3b6a63d126a329c2e8f11bfb5f0b3f4d33e52e12007b6dba7f7e87e7921a" "1114c56feb07fb44ad12ede602c566a1387aeffcf5990a446952d54fba620be3" default))
 '(package-selected-packages
   '(dracula-theme git powerline corfu adoc-mode markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))


(keyboard-translate ?\C-h ?\C-?)
(column-number-mode t)
(line-number-mode t)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

 (defvar auto-install-package-list
  '(ddskk
    ido-skk
    multi-term
    corfu
    powerline
    dracula-theme
    git
    org)
  "Packages to be installed")
;; auto-installation
(dolist (pkg auto-install-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; 行を表示
(column-number-mode t)

;; 列を表示
;;(global-linum-mode t)
;; (setq linum-format "%5d|")

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

;; toggle japanese input method with "C-,"
(global-set-key (kbd "C-,") 'toggle-input-method)

;; タイトルにフルパスを表示
(setq frame-title-format "%f")

;; スタートアップメッセージを削除
(setq inhibit-startup-message 1)

;; 初期メッセージを消去
(setq initial-scratch-message "")

;; 言語設定
(set-language-environment "Japanese")
(when (require 'skk nil t)
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
  (setq default-input-method "japanese-skk")
  (require 'skk-study))

;; テーマの読み込み
(load-theme 'dracula t)

(when IS-WINDOWS
  (setenv "HOME" (getenv "USERPROFILE"))
  (setq default-directory (concat (getenv "USERPROFILE") "/")))
