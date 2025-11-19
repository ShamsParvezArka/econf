;; Author   : Shams Parvez Arka <parvez6826@gamil.com>
;; Version  : 0.2
;; Philosopy: Keep It Super Simple (KISS)
;; License  : MIT
;; URL      : https://github.com/ShamsParvezArka/econf.git
;; Required : Emacs 24+
;; Footnote : When your dev environment gets booring, it
;;            means you're in a stable phase.


;; [Settings::MELPA] --------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; [Settings::Frame] --------------------------------------------------------------------------------
(setq inhibit-startup-screen t)
(scroll-bar-mode 0)
(toggle-menu-bar-mode-from-frame 0)
(toggle-tool-bar-mode-from-frame 0)
(set-fringe-mode 0)
(display-fill-column-indicator-mode t)
(setq initial-scratch-message "")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(split-window-right)
(other-window -1)
(simple-modeline-mode t)


;; [Settings::Scroll] --------------------------------------------------------------------------------
(pixel-scroll-precision-mode t)


;; [Settings::Text_Wraping] --------------------------------------------------------------------------------
(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 0 ?\ )


;; [Settings::Lockfile] --------------------------------------------------------------------------------
(setq make-backup-files nil)
(setq create-lockfiles nil)


;; [Settings::Dired] --------------------------------------------------------------------------------
(add-hook 'dired-mode-hook 'auto-revert-mode)


;; [Settings::Custom Comment] --------------------------------------------------------------------------------
(defun custom/c-comment ()
  (setq-local comment-insert-comment-function
              (lambda ()
                (let ((dashes (make-string 80 ?-)))                
                (insert "//  " dashes)
                (backward-char 81)))))
(defun custom/batch-comment ()
  (setq-local comment-insert-comment-function
              (lambda ()
                (let ((dashes (make-string 80 ?-)))                
                (insert "::  " dashes)
                (backward-char 81)))))
(add-hook 'c-mode-hook #'custom/c-comment)
(add-hook 'bat-mode-hook #'custom/batch-comment)


;; [Settings::Compilation] --------------------------------------------------------------------------------
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


;; [Settings::Auto Completion] --------------------------------------------------------------------------------
(ido-mode t)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'global-company-mode)
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))


;; [Settings::Eglot] --------------------------------------------------------------------------------
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
  (setq eglot-extend-to-xref nil)

  (add-to-list 'eglot-ignored-server-capabilities 'documentOnTypeFormattingProvider)
  (add-to-list 'eglot-ignored-server-capabilities 'documentFormattingProvider)
  (add-to-list 'eglot-ignored-server-capabilities 'documentRangeFormattingProvider)

  (setf (alist-get 'c-mode eglot-server-programs)
        '("clangd" "--enable-config=no" "--fallback-style=none"))
  (setf (alist-get 'c++-mode eglot-server-programs)
        '("clangd" "--enable-config=no" "--fallback-style=none")))
  
(with-eval-after-load "eglot"
  (add-to-list 'eglot-stay-out-of 'flymake))
(setq eldoc-echo-area-use-multiline-p nil)
(setq js2-strict-missing-semi-warning nil)


;; [Settings::OmniSharp Setup] --------------------------------------------------------------------------------
(setq omnisharp-server-executable-path "C:\\Program Files (x86)\\omnisharp-win-x86\\OmniSharp.exe")
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))


;; [Settings::Indentation] --------------------------------------------------------------------------------
(setq-default indent-tabs-mode nil
	      tab-width 2
	      standard-indent 2)

(defun custom/language-specific-indent ()
  (when (boundp 'c-basic-offset)          (setq c-basic-offset 2))  
  (when (boundp 'python-indent-offset)    (setq python-indent-offset 2))
  (when (boundp 'js-indent-level)         (setq js-indent-level 2))
  (when (boundp 'typescript-indent-level) (setq typescript-indent-level 2))
  (when (boundp 'css-indent-offset)       (setq css-indent-offset 2))
  (when (boundp 'sgml-basic-offset)       (setq sgml-basic-offset 2)))

(defun custom/untabify-buffer ()
  (when (not indent-tabs-mode)
    (untabify (point-min) (point-max))))

(c-add-style "handmade"
             '("k&r"
               (c-offsets-alist (case-label . +)
				(statement-case-open . 0)
				(statement-case-intro . +)
				(substatement-open . 0)
				(block-open . 0)
				(block-close . 0)
				(defun-open . 0)
				(defun-close . 0))))

(add-hook 'c-mode-common-hook (lambda () (c-set-style "handmade")))
(add-hook 'prog-mode-hook 'custom/language-specific-indent)
(add-hook 'text-mode-hook 'custom/language-specific-indent)
(add-hook 'before-save-hook 'custom/untabify-buffer)


;; [Settings::Line Numbers] --------------------------------------------------------------------------------
;;(setq display-line-numbers-type 'relative)
;;(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; DO I REALLY NEED LINE NUMBERS


;; [Settings::Font] --------------------------------------------------------------------------------
(set-face-attribute 'default nil
                    :family "Liberation Mono"
                    :height 130)
(set-face-attribute 'variable-pitch nil
                    :family "Liberation Mono")
(set-face-attribute 'fixed-pitch nil
                    :family "Liberation Mono")
(set-face-attribute 'tooltip nil
                    :family "Aptos Display")


;; [Settigns::Theme] --------------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq custom-safe-themes t)
(load-theme 'fleury)
;; The below expression will disable bold font globaly(Must be invoked after theme callback)
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))


;; [Settings::Org Mode] --------------------------------------------------------------------------------
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (org-indent-mode 1)))


;; [Settings::Macro] --------------------------------------------------------------------------------
(defun custom/compile-or-recompile ()
  (interactive)
  (if (get-buffer "*compilation*")
      (recompile)
    (call-interactively 'compile)))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-u"))

(global-set-key   (kbd "C-u")     'undo)
(global-set-key   (kbd "C-=")     'dired-create-empty-file)
(global-set-key   (kbd "C-'")     'surround-mark)
(global-set-key   (kbd "C-x C-k") 'c-indent-line-or-region)
(global-set-key   (kbd "C-c o")   'window-swap-states)
(global-set-key   (kbd "M-x")     'smex)
(global-set-key   (kbd "M-o")     'replace-regexp)
(global-set-key   (kbd "M-m")     'custom/compile-or-recompile)
(global-set-key   (kbd "TAB")     'self-insert-command)


;; [Mics.] --------------------------------------------------------------------------------

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
                         lsp-pyright lsp-python-ms lua-mode magit
                         markdown-preview-eww markdown-preview-mode
                         markdown-toc modus-themes monokai-pro-theme
                         monokai-theme multiple-cursors naysayer-theme
                         nerd-icons nerd-icons-dired omnisharp org
                         org-bullets org-modern powershell python-mode
                         rainbow-mode ripgrep rust-mode
                         simple-modeline smart-tabs-mode smex surround
                         undo-fu vertico vscode-dark-plus-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
