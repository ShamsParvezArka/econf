(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Debloat frame
(setq inhibit-startup-screen t)
(toggle-scroll-bar t)
(scroll-bar-mode 1)
(toggle-menu-bar-mode-from-frame 0)
(toggle-tool-bar-mode-from-frame 0)
;; (setq split-height-threshold nil
;;      split-width-threshold 0)

;; Initial frame size
(setq initial-frame-alist
     (append initial-frame-alist
              '((width  . 80)
                (height . 30))))

;; Backup settings
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Auto-pair, auto-completion, pretty-symbol, line wrapping
(electric-pair-mode t)
(ido-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(prettify-symbols-mode t)
(toggle-truncate-lines t)
(set-display-table-slot standard-display-table 0 ?\ )

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Indentation
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

;; Disable Fringe Mode
(set-fringe-mode 0)

;; Enable pixel scrolling precision mode
(pixel-scroll-precision-mode t)

;; Rust Integration
;;(require 'rust-mode)
;;(add-hook 'rust-mode-hook
;;          (lambda () (setq indent-tabs-mode nil)))
;;(add-hook 'rust-mode-hook
;;          (lambda () (prettify-symbols-mode)))
;;(setq rust-format-on-save t)

;; Line number
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;; Font
(set-face-attribute 'default nil
                    :family "Hack"
                    :height 145)
(set-face-attribute 'variable-pitch nil
                    :family "Hack")
(set-face-attribute 'fixed-pitch nil
                    :family "Hack")

;; Theme
(setq custom-safe-themes t)
(load-theme 'naysayer)
(set-cursor-color "#80f765")

;; Org-mode setup
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (org-indent-mode 1)))

;; Powershell integration
;; (setq-default shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")

;; Godot gdscript integration
;; (setq gdscript-godot-executable "C:/Godot/Godot_v4.3-stable_win64.exe")

(use-package eglot)
(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (eglot-inlay-hints-mode -1)))
(setq eldoc-echo-area-use-multiline-p nil)
(add-to-list 'eglot-stay-out-of 'flymake)

;; Custom Keybindings
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
(global-set-key (kbd "C-'") 'surround-mark-inner)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(smooth-scrolling eglot-java markdown-preview-eww markdown-toc flymd eldoc-box monokai-theme rust-mode modus-themes nerd-icons-dired all-the-icons-dired nerd-icons babel naysayer-theme surround elcord simple-modeline lsp-python-ms glsl-mode vscode-dark-plus-theme lsp-pyright lua-mode gdscript-mode powershell markdown-preview-mode gruvbox-theme org-modern org-bullets org ripgrep smart-tabs-mode undo-fu expand-region company lsp-mode magit multiple-cursors gruber-darker-theme smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
