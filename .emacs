
;; Author   : Shams Parvez Arka <parvez6826@gamil.com>
;; Version  : 0.1
;; Philosopy: Keep It Super Simple (KISS)
;; License  : MIT
;; URL      : https://github.com/ShamsParvezArka/econf.git
;; Required : Emacs 24+
;; Footnote : When your dev environment gets booring, it
;;            means you're in a stable phase.


;; [Settings::Bootstrap_MELPA]
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; [Settings::Frame]
(setq inhibit-startup-screen t)
(toggle-scroll-bar t)
(scroll-bar-mode 0)
(toggle-menu-bar-mode-from-frame 0)
(toggle-tool-bar-mode-from-frame 0)
(set-fringe-mode 0)
(setq split-height-threshold nil    ;; Uncomment this expression if you want
      split-width-threshold 0)      ;; emacs auto split into vertical mode
(setq initial-frame-alist
     (append initial-frame-alist
              '((width  . 80)
                (height . 30))))


;; [Settings::Scroll]
(pixel-scroll-precision-mode t)


;; [Settings::Text_Wraping]
(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 0 ?\ )


;; [Settings::Lockfile]
(setq make-backup-files nil)
(setq create-lockfiles nil)


;; [Settings::Auto_Completion]
(ido-mode t)
(electric-pair-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))


;; [Settings::Indentation]
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent-offset 4)))


;; [Settings::Rust_Mode]
(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))


;; [Settings::Line_Numbers]
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)


;; [Settings::Font]
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 145)
(set-face-attribute 'variable-pitch nil
                    :family "Hack")
(set-face-attribute 'fixed-pitch nil
                    :family "Hack")
(set-face-attribute 'tooltip nil
                    :family "Aptos Display")


;; [Settigns::Theme]
(setq custom-safe-themes t)
(load-theme 'naysayer)
(set-cursor-color "#80f765")


;; [Settings::Org_Mode]
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (org-indent-mode 1)))


;; [Settings::Eglot]
(use-package eglot)
(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (eglot-inlay-hints-mode -1)))
(setq eldoc-echo-area-use-multiline-p nil)
(with-eval-after-load "eglto"
  (add-to-list 'eglot-stay-out-of 'flymake))
(setq js2-strict-missing-semi-warning nil)


;; [Settings::Macro]
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "C-z") 'undo-fu-only-undo)
(global-set-key (kbd "C-/") 'undo-fu-only-redo)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-s") 'ripgrep-regexp)
(global-set-key (kbd "C-'") 'surround-mark) 
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)    ;; This one nuts


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auto-package-update python-mode github-dark-vscode-theme gruber-darker-theme naysayer-theme gnu-elpa-keyring-update smooth-scrolling eglot-java markdown-preview-eww markdown-toc flymd eldoc-box rust-mode modus-themes nerd-icons-dired all-the-icons-dired nerd-icons babel surround elcord lsp-python-ms glsl-mode vscode-dark-plus-theme lsp-pyright lua-mode gdscript-mode powershell markdown-preview-mode gruvbox-theme org-modern org-bullets org ripgrep smart-tabs-mode undo-fu expand-region company lsp-mode magit multiple-cursors smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
