(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Debloat frame
(setq inhibit-startup-screen t)
(toggle-scroll-bar t)
(scroll-bar-mode 1)
(toggle-menu-bar-mode-from-frame 0)
(toggle-tool-bar-mode-from-frame 0)
(setq split-height-threshold nil
      split-width-threshold 0)

;; Initial frame size
(setq initial-frame-alist
     (append initial-frame-alist
              '((width  . 80)
                (height . 30))))

;; Backup settings
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Auto-pair, auto-completion
(electric-pair-mode t)
(ido-mode t)
(global-company-mode t)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

;; Disable Fringe Mode
(set-fringe-mode 0)

;; Line number
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;; Font
(set-face-attribute 'default nil
                    :family "Hack Nerd Font"
                    :height 145) 
(set-face-attribute 'variable-pitch nil
                    :family "Hack Nerd Font")
(set-face-attribute 'fixed-pitch nil
                    :family "Hack Nerd Font")
(add-to-list 'default-frame-alist
             '(font . "Hack Nerd Font-14.5"))

;; Theme
(setq custom-safe-themes t)
(load-theme 'gruvbox-dark-medium)

;; Org-mode setup
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (indent-mode 1)))

;; Powershell integration
(setq-default shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")

;; Godot gdscript integration
(setq gdscript-godot-executable "C:/Godot/Godot_v4.3-stable_win64.exe")

;; Eglot setup
(use-package eglot)
(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (eglot-inlay-hints-mode -1)))
(setq eldoc-echo-area-use-multiline-p nil)
(add-to-list 'eglot-stay-out-of 'flymake)

;; Key-bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-z") 'undo-fu-only-undo)
(global-set-key (kbd "C-/") 'undo-fu-only-redo)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-s") 'ripgrep-regexp)
(global-set-key (kbd "M-'") 'surround-mark-inner)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(naysayer-theme surround elcord simple-modeline lsp-python-ms glsl-mode vscode-dark-plus-theme lsp-pyright lua-mode gdscript-mode powershell markdown-preview-mode gruvbox-theme org-modern org-bullets org ripgrep smart-tabs-mode undo-fu expand-region company lsp-mode magit multiple-cursors gruber-darker-theme smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
