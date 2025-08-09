
;; Author   : Shams Parvez Arka <parvez6826@gamil.com>
;; Version  : 0.1
;; Philosopy: Keep It Super Simple (KISS)
;; License  : MIT
;; URL      : https://github.com/ShamsParvezArka/econf.git
;; Required : Emacs 24+
;; Footnote : When your dev environment gets booring, it
;;            means you're in a stable phase.


;; [Settings::MELPA]
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; [Settings::Frame]
(setq inhibit-startup-screen t)
(scroll-bar-mode 0)
(toggle-menu-bar-mode-from-frame 0)
(toggle-tool-bar-mode-from-frame 0)
(set-fringe-mode 0)
;;(setq split-height-threshold nil    ;; Uncomment this expression if you want
;;      split-width-threshold 0)      ;; emacs auto split into vertical mode
;;(setq initial-frame-alist
;;      (append initial-frame-alist
;;              '((width  . 80)
;;                (height . 30))))
(display-fill-column-indicator-mode t)
;; (setq-default display-fill-column-indicator-column 90)
(setq initial-scratch-message "")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(split-window-right)
(other-window -1)
(simple-modeline-mode t)


;; [Settings::Scroll]
(pixel-scroll-precision-mode t)


;; [Settings::Text_Wraping]
(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 0 ?\ )


;; [Settings::Lockfile]
(setq make-backup-files nil)
(setq create-lockfiles nil)


;; [Settings::Dired]
(add-hook 'dired-mode-hook 'auto-revert-mode)


;; [Settings::Compilation ]
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.2)
               (inhibit-same-window . nil))) 

(defun custom/select-compilation-window ()
  (when (string= (buffer-name) "*compilation*")
    ;; Ensure buffer is displayed and focused
    (let ((win (display-buffer (current-buffer))))
      (when (window-live-p win)
        (select-window win)))))
(add-hook 'compilation-mode-hook #'custom/select-compilation-window)


;; [Settings::Auto_Completion]
(ido-mode t)
;; (electric-pair-mode t)
(add-hook 'after-init-hook 'global-company-mode)
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))


;; [Settings::Eglot]
(use-package eglot	
  :hook
  ((c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (eglot-managed-mode . (lambda ()
                           (eglot-inlay-hints-mode -1)
                           (flymake-mode -1)
                           ;; Disable auto-format on file save
                           (remove-hook 'before-save-hook #'eglot-format-buffer t))))
  :config
  ;; Prevent eglto from auto-formatting(YEAH! AUTO-FORMAT IS A DIRECT SLAP IN PROGRAMMERS FACE).
  (add-to-list 'eglot-ignored-server-capabilities 'documentFormattingProvider)
  (add-to-list 'eglot-ignored-server-capabilities 'documentRangeFormattingProvider)
  (add-to-list 'eglot-server-programs
               '(c-mode . ("clangd" "--fallback-style=none")))
  (add-to-list 'eglot-server-programs
               '(c++-mode . ("clangd" "--fallback-style=none"))))

(with-eval-after-load "eglot"
  (add-to-list 'eglot-stay-out-of 'flymake))
(setq eldoc-echo-area-use-multiline-p nil)
(setq js2-strict-missing-semi-warning nil)


;; [Settings::Indentation]
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq tab-width 2)
            (setq python-indent-offset 4)))


;; (require 'rust-mode)
;; (add-hook 'rust-mode-hook
;;           (lambda () (setq indent-tabs-mode nil)))
;; (add-hook 'rust-mode-hook
;;           (lambda () (prettify-symbols-mode)))


;; [Settings::Line_Numbers]
;;(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)


;; [Settings::Font]
(set-face-attribute 'default nil
                    :family "Liberation Mono"
                    :height 120)
(set-face-attribute 'variable-pitch nil
                    :family "Liberation Mono")
(set-face-attribute 'fixed-pitch nil
                    :family "Liberation Mono")
(set-face-attribute 'tooltip nil
                    :family "Aptos Display")


;; [Settigns::Theme]
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq custom-safe-themes t)
(load-theme 'fleury)
;; The below expression will disable bold font globaly(Must be invoked after theme callback)
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

;;(use-package highlight-indent-guides
;;  :hook
;;  (prog-mode . highlight-indent-guides-mode)
;;  :config
;;  (setq highlight-indent-guides-method 'character)
;;  (setq highlight-indent-guides-character ?\│)
;;  (setq highlight-indent-guides-responsive 'top)
;;;;  (setq highlight-indent-guides-auto-enabled t)
;;  (setq highlight-indent-guides-auto-enabled nil)
;;	(set-face-foreground 'highlight-indent-guides-character-face "gray25")
;;	(set-face-foreground 'highlight-indent-guides-top-character-face "dimgray"))
  

;; [Settings::Org_Mode]
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (org-indent-mode 1)))


;; [Settings::Macro]
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "C-=") 'dired-create-empty-file)
(global-set-key (kbd "C-'") 'surround-mark)
(global-set-key (kbd "C-x C-k") 'c-indent-line-or-region)
(global-set-key (kbd "M-o") 'replace-regexp)
(defun custom/compile-or-recompile ()
  (interactive)
  (if (get-buffer "*compilation*")
      (recompile)
    (call-interactively 'compile)))
(global-set-key (kbd "M-m") #'custom/compile-or-recompile)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons-dired auto-package-update babel company doom-themes
                         eglot-java elcord eldoc-box expand-region
                         flymd gdscript-mode github-dark-vscode-theme
                         glsl-mode gnu-elpa-keyring-update
                         gruber-darker-theme gruvbox-theme
                         highlight-indent-guides js2-mode keycast
                         lsp-mode lsp-pyright lsp-python-ms lua-mode
                         magit markdown-preview-eww
                         markdown-preview-mode markdown-toc
                         modus-themes monokai-pro-theme monokai-theme
                         multiple-cursors naysayer-theme nerd-icons
                         nerd-icons-dired org org-bullets org-modern
                         powershell python-mode rainbow-mode ripgrep
                         rust-mode simple-modeline smart-tabs-mode
                         smex surround undo-fu vscode-dark-plus-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
