;; Initialize melpa packages
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Debloat frame
(setq inhibit-startup-screen t)
(toggle-scroll-bar t)
(toggle-menu-bar-mode-from-frame 0)
(toggle-tool-bar-mode-from-frame 0)

;; Startup directory
(setq default-directory "$USERPROFILE")

;; Auto-pair, auto-completion
(electric-pair-mode t)
(ido-mode t)
(global-company-mode t)

;; Font
(set-face-attribute 'default nil
                    :font "Hack Nerd Font Mono-14.5")
(custom-set-faces
 '(fixed-pitch ((t (:family "Hack Nerd Font Mono"))))
 '(fixed-pitch-serif ((t (:family "Hack Nerd Font Mono")))))

;; Theme
(add-to-list 'custom-theme-load-path "$USERPROFILE/AppData/Roaming/.emacs.d/elpa/naysayer-theme/")
(setq custom-safe-themes t)
(load-theme 'gruber-darker)

;; Custom function
(defun insert-line-below ()
    (interactive)
    (save-excursion)
    (end-of-line)
    (newline))

(defun insert-line-above ()
    (interactive)
    (save-excursion)
    (end-of-line 0)
    (newline)
    (end-of-line))

;; Key-bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-o") 'insert-line-below)
(global-set-key (kbd "C-S-o") 'insert-line-above)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(simple-modeline expand-region company lsp-mode magit multiple-cursors gruber-darker-theme smex)))
