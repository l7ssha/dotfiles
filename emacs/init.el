(setq inhibit-startup-message t)
(tool-bar-mode -1)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	      '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-packages)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-safe-themes
   (quote
    ("6dd2b995238b4943431af56c5c9c0c825258c2de87b6c936ee88d6bb1e577cb9" default)))
 '(package-selected-packages
   (quote
    (tide indium all-the-icons neotree slime-company company flycheck atom-one-dark-theme color-theme counsel swiper ace-window org-bullets which-key try use-package)))
 '(which-key-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))

(use-package try
  :ensure t)

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda ()
				     (org-bullets-mode 1))))

(use-package ace-window
  :ensure t
  :init (progn
	  (global-set-key [remap other-window] 'ace-window)
	  (custom-set-faces
	   '(aw-leading-char-face
	     ((t (:inherit ace-jump-face-foreground :height 2.0)))))))

(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :config (progn
	    (ivy-mode 1)
	    (setq ivy-use-virtual-buffers t)
	    (setq enable-recursive-minibuffers t)
	    (global-set-key "\C-s" 'swiper)
	    (global-set-key (kbd "C-c C-r") 'ivy-resume)
	    (global-set-key (kbd "<f6>") 'ivy-resume)
	    (global-set-key (kbd "M-x") 'counsel-M-x)
	    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
	    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
	    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
	    (global-set-key (kbd "<f1> l") 'counsel-find-library)
	    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
	    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
	    (global-set-key (kbd "C-c g") 'counsel-git)
	    (global-set-key (kbd "C-c j") 'counsel-git-grep)
	    (global-set-key (kbd "C-c k") 'counsel-ag)
	    (global-set-key (kbd "C-x l") 'counsel-locate)
	    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
	    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package slime-company
  :ensure t)

(use-package all-the-icons
  :ensure t
  :config (all-the-icons-install-fonts))

(use-package neotree
  :ensure t
  :config (progn
	    (global-set-key [f8] 'neotree-toggle)
	    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

(use-package indium
  :ensure t)

(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defalias 'list-buffers 'ibuffer)

(winner-mode 1)
