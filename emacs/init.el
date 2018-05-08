(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default indent-tabs-mode nil)

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
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#373b41" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "6dd2b995238b4943431af56c5c9c0c825258c2de87b6c936ee88d6bb1e577cb9" default)))
 '(fci-rule-color "#373b41")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(markdown-command "markdown2" t)
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-tomorrow json-mode smart-tab preety-parens slime-company paredit company-slime parinfer smart-yank cider hungry-delete iedit expand-region gfm-mode markdown-mode company-tern js2-refactor tide indium all-the-icons neotree company flycheck atom-one-dark-theme color-theme counsel swiper ace-window org-bullets which-key try use-package)))
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tab-width 4)
 '(which-key-mode t))

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))

(blink-cursor-mode 0)
(set-cursor-color "#ffffff")
(set-face-attribute 'default nil :font "DejaVu Sans Mono-9")

(use-package try
  :ensure t)

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda ())
             (org-bullets-mode 1)))

(use-package ace-window
  :ensure t
  :init (progn)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 2.0))))))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (progn
        (require 'color-theme-sanityinc-tomorrow)
        (color-theme-sanityinc-tomorrow--define-theme night)))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :config (progn)
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
      (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package all-the-icons
  :ensure t)
  ;; :config (all-the-icons-install-fonts))

(use-package neotree
  :ensure t
  :config (progn)
      (global-set-key [f8] 'neotree-toggle)
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package js2-mode
  :ensure t
  :config (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package js2-refactor
  :ensure t)

(use-package company-tern
  :ensure t
  :config (progn)
      (add-to-list 'company-backends 'company-tern)
      (add-hook 'js2-mode-hook (lambda ())
               (tern-mode)
               (company-mode)))

; (setq electric-pair-pairs '(
 ;         (?\" . ?\")
  ;        (?\{ . ?\})))
          

; (electric-pair-mode 1)
(show-paren-mode 1)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown2"))

(use-package indium
  :ensure t)

(use-package tide
  :ensure t)

(use-package cider
  :ensure t
  :config (progn)
      (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
      (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "C-=") 'er/expand-region))

(use-package iedit
  :ensure t)

(use-package json-mode
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1) 
  (company-mode +1))

(use-package web-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(setq web-mode-enable-auto-closing t)

(use-package slime-company
  :ensure t)

(use-package smart-tab
  :ensure t)

(use-package smart-yank
  :ensure t)

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

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

;; move line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

;; move line down
(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(global-set-key [\M-\S-up] 'move-line-up)
(global-set-key [\M-\S-down] 'move-line-down)
