(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 '(package-selected-packages '(kaolin-themes dashboard)))

(setq inhibit-startup-message t)

(set-frame-font "UbuntuMono Nerd Font Mono" nil t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default cursor-type 'bar)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-goto-address-mode 1)
(global-visual-line-mode 1)
(delete-selection-mode 1)
(save-place-mode 1)

(global-set-key (kbd "M-q") 'kill-this-buffer)
(global-set-key (kbd "M-n") 'other-window)
(global-set-key (kbd "M-z") 'ibuffer)
(global-set-key (kbd "M-j") 'dired-jump)
(global-set-key (kbd "M-d") 'dired)
(global-set-key (kbd "M-k") 'kill-buffer)
(global-set-key (kbd "M-s") 'save-buffer)

(fset 'yes-or-no-p 'y-or-n-p)

(setq initial-scratch-message "")
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(add-hook 'minibuffer-exit-hook
      #'(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

(setq dired-listing-switches "-lah --group-directories-first")
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(setq sentence-end-double-space nil)

(setq ring-bell-function 'ignore)

(global-display-line-numbers-mode 1)

(electric-pair-mode 1)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(require 'kaolin-themes)
(load-theme 'kaolin-dark t)

(add-to-list 'load-path (concat user-emacs-directory "/modeline"))
(require 'simple-modeline)
(simple-modeline-mode)

(require 'dashboard)
(dashboard-setup-startup-hook)

(setq dashboard-banner-logo-title "Welcome, compromyse.")
(setq dashboard-startup-banner nil)
(setq dashboard-center-content t)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
