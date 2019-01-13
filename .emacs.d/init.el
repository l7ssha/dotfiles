(require 'package)
;(setq package-enable-at-startup pnil)

(add-to-list 'package-archives
        '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-packages)
  (package-refresh-contents)
  (package-install 'use-package))
(setq browse-url-browser-function 'browse-url-chromium)
;;(setq browse-url-chromium-program "vivaldi-stable")

(setq custom-safe-themes t)
(setq-default indent-tabs-mode nil)

;;; THEME
(set-frame-font "DejaVu Sans Mono 9" nil t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-linum-mode t)
(blink-cursor-mode 0)
(show-paren-mode 1)
(winner-mode 1)
(pending-delete-mode 1)
(global-visual-line-mode 1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-face-attribute 'default (selected-frame) :height 95)

(use-package klere-theme
  :ensure t)

(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (select-frame frame)
                                            (when (display-graphic-p frame)
                                              (load-theme 'klere t)
                                              (set-frame-font "DejaVu Sans Mono 9" nil t)
                                              (set-cursor-color "#ffffff"))))
  (load-theme 'klere t))



(set-cursor-color "#ffffff")

(use-package telephone-line
  :ensure t
  :config (telephone-line-mode 1))

(use-package elcord
  :ensure t
  :config (elcord-mode))

(use-package helm
  :ensure t
  :config (progn
            (global-set-key (kbd "M-x") 'helm-M-x)
            (helm-mode 1)
            (setq helm-split-window-default-side 'other)
            (global-set-key (kbd "C-x C-f") 'helm-find-files)
            (global-set-key (kbd "C-x C-b") 'helm-mini)
            (setq helm-split-window-preferred-function 'ignore)
            (global-set-key (kbd "C-x b") 'helm-mini)
            (add-hook 'eshell-mode-hook
                      (lambda ()
                        (eshell-cmpl-initialize)))))

(use-package swiper-helm
  :ensure t
  :config
  (setq swiper-helm-display-function 'helm-default-display-buffer)
  (global-set-key (kbd "C-s") 'swiper-helm)
  (global-set-key (kbd "C-r") 'swiper-helm))

(use-package helm-gitignore
  :ensure t)

(use-package helm-ag
  :ensure t)

;;; COMPANY

(use-package company
  :diminish company-mode
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay              t
        company-minimum-prefix-length   1
        company-show-numbers            t
        company-echo-delay              0
        company-tooltip-limit           20))

(use-package company-quickhelp
  :ensure t
  :config (company-quickhelp-mode))

(use-package company-web
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

(use-package flycheck-inline
  :ensure t
  :config (with-eval-after-load 'flycheck
            (flycheck-inline-mode)))

;;; YASNIPPET

(use-package yasnippet
  :ensure t
  :config
  (add-hook 'snippet-mode-hook '(lambda ()
                                  (auto-fill-mode -1)))
  ;;(add-to-list 'company-backends 'company-yasnippet)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(defun check-expansion ()
  "Check if there is possible to expand."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t))
    (backward-char 1)
    (if (looking-at "->") t nil)))

(defun do-yas-expand ()
  "Expand yas snippet."
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  "Check if can indent or compete, then do whenever is possible."
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas-minor-mode))
        (null (do-yas-expand)))
    (if (check-expansion)
        (progn
          (company-manual-begin)
          (if (null company-candidates))
          (progn
            (company-abort)
            (indent-for-tab-command)))))))

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas-minor-mode))
      (null (do-yas-expand))
    (if company-candidates)
    (company-complete-selection)
    (if (check-expansion)
        (progn
          (company-manual-begin)
          (if (null company-candidates))
          (progn
            (company-abort)
            (yas-next-field)))
      (yas-next-field))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas-minor-mode))
      (null (do-yas-expand))
      (company-abort)
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

(global-set-key [tab] 'tab-indent-or-complete)
(global-set-key (kbd "TAB") 'tab-indent-or-complete)
(global-set-key [(control return)] 'company-complete-common)
(with-eval-after-load 'company
  (define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection))

;;; POPWIN

(use-package popwin
  :ensure t
  :config
  (setq display-buffer-function 'popwin:display-buffer)
  (setq helm-split-window-preferred-function 'ignore)
  (push '("^\*helm .+\*$" :regexp t :height 40) popwin:special-display-config)
  (push '("^\*helm-.+\*$" :regexp t :height 40) popwin:special-display-config)
  (push '("\*swiper.+\*" :regexp t :height 40) popwin:special-display-config)
  (push '("^\*magit.+\*$" :regexp t :height 40) popwin:special-display-config))

;;; ACE WINDOW

(use-package ace-window
  :ensure t
  :config (global-set-key (kbd "M-o") 'ace-window)
  (ace-window-display-mode 1))

;;; LSP

(use-package lsp-mode
  :ensure t)

(require 'lsp-clients)

(use-package lsp-ui
  :ensure t
  :config (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :config (push 'company-lsp company-backends))

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook #'lsp-java-enable)
  (add-hook 'java-mode-hook (lambda ()
                              (setq lsp-inhibit-message t
                                    lsp-eldoc-render-all nil)
                              (setq company-lsp-enable-snippet t
                                    company-lsp-cache-candidates t))))
;;; PROGRAMMING

(use-package lua-mode
  :ensure t)

(use-package gradle-mode
  :ensure t
  :config (gradle-mode 1))

(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package web-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-closing t))

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "C-.") 'avy-goto-char))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(defun my-flymd-browser-function (url)
  "Custom function to open browser.
URL to open in browser"
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))

(use-package flymd
  :ensure t
  :config
  (setq flymd-browser-open-function 'my-flymd-browser-function))

(use-package ensime
  :ensure t)

(use-package scala-mode
  :ensure t)

(use-package sbt-mode
  :ensure t)

(use-package dart-mode
  :ensure t)

;;; OTHER

(use-package iedit
  :ensure t)

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-pair "'" nil :unless '(sp-point-after-word-p)))

(use-package projectile
  :ensure t
  :config (projectile-mode)
  (setq projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-globally-ignored-directories '( "out" "bin" ".gradle" "gradle" "output" ".meghanada" ".idea" "build/"))
  (setq projectile-globally-ignored-files '("*~" "#(.+)#")))

(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 50))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "C-=") 'er/expand-region))
 
(use-package drag-stuff
  :ensure t
  :config (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package smart-tab
  :ensure t)

(use-package smart-yank
  :ensure t)

;;; OTHER

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; KEYBINDS

(global-set-key (kbd "C-3") (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-2") (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets challenger-deep-theme lsp-clients lsp-java company-lsp lsp-ui lsp-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
