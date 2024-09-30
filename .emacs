(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Debloat frame
(setq inhibit-startup-screen t)
(toggle-scroll-bar t)
(scroll-bar-mode 1)
(toggle-menu-bar-mode-from-frame 0)
(toggle-tool-bar-mode-from-frame 0)

;; Initial frame size
(setq initial-frame-alist
      (append initial-frame-alist
              '((width  . 90)
                (height . 30))))

;; Column guideline
;;(setq-default display-fill-column-indicator-column 80)

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

;; Line number
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;; Font
(set-face-attribute 'default nil
                    :family "Hack Nerd Font Mono"
                    :height 145)
(set-face-attribute 'variable-pitch nil
                    :family "Hack Nerd Font Mono")
(set-face-attribute 'fixed-pitch nil
                    :family "Hack Nerd Font Mono")

;; Theme
(setq custom-safe-themes t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'naysayer)

;; Org-mode setup
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Powershell integration
(setq-default shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")

;; grep custom commands
(setq grep-command " grep . -r --color=always -nH --null -e ")

;; Godot gdscript integration
(setq gdscript-godot-executable "C:/Godot/Godot_v4.3-stable_win64.exe")

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lua-mode gdscript-mode powershell markdown-preview-mode gruvbox-theme org-modern org-bullets org ripgrep smart-tabs-mode undo-fu expand-region company lsp-mode magit multiple-cursors gruber-darker-theme smex)))


