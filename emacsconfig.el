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
(set-frame-font "Hack Nerd Font Mono-9" nil t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-linum-mode t)
(blink-cursor-mode 0)
(show-paren-mode 1)
(winner-mode 1)
(pending-delete-mode 1)
(global-visual-line-mode 1)
(scroll-bar-mode -1)

;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;(set-face-attribute 'default (selected-frame) :height 95)
;(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package klere-theme
  :ensure t)

(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (select-frame frame)
                                            (when (display-graphic-p frame)
                                              (load-theme 'klere t)
                                              (set-frame-font "Hack Nerd Font Mono-9" nil t)
                                              (set-cursor-color "#ffffff"))))
  (load-theme 'klere t))


(set-cursor-color "#ffffff")

(use-package telephone-line
  :ensure t
  :config (telephone-line-mode 1))

;;; ------ COMPANY

(use-package company
  :diminish company-mode
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay              t
        company-minimum-prefix-length   3
        company-show-numbers            t
        company-echo-delay              0
        company-tooltip-limit           20))

(use-package company-quickhelp
  :ensure t
  :config (company-quickhelp-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

;; (use-package company-capf
;;   :ensure t)

;;; ------ PROJECTILE

(use-package projectile
  :ensure t
  :config (projectile-mode)
  (setq projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD")))

;;; ------ HELM

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

;;; ------ POPWIN

(use-package popwin
  :ensure t
  :config
  (setq display-buffer-function 'popwin:display-buffer)
  (setq helm-split-window-preferred-function 'ignore)
  (push '("^\*helm .+\*$" :regexp t :height 40) popwin:special-display-config)
  (push '("^\*helm-.+\*$" :regexp t :height 40) popwin:special-display-config)
  (push '("\*swiper.+\*" :regexp t :height 40) popwin:special-display-config)
  (push '("^\*magit.+\*$" :regexp t :height 40) popwin:special-display-config))

;;; ------ MINOR THINGS

(use-package iedit
  :ensure t)

(use-package aggressive-indent
  :ensure t)
  ;; :config (global-aggressive-indent-mode 1))

(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-\"") 'avy-goto-char-2)
  (global-set-key (kbd "C-'") 'avy-goto-char))

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

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config (ace-window-display-mode t))

;;; ------ LSP

(setq lsp-keymap-prefix "C-l")

(use-package lsp-mode
  :ensure t
  :hook ((dart-mode . lsp)
         (php-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config (progn
            (with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))))

(use-package lsp-ui
  :ensure t
  :config (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :config (push 'company-lsp company-backends))

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

;;; ------ TREEMACS

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-width                         50)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;;; ------ LANGUAGES

(use-package dart-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package php-mode
  :ensure t)

;;; ------- MISC

(setq lsp-enable-indentation nil)

(setq gc-cons-threshold 500000000)

(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . "/mnt/data/buckup/.emacsbackupfiles")))

(setq-default frame-title-format '("emacs @ %b"))

;;; ------- KEYBINDS

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

;;;; ----- CUSTOM FUNCS

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line.
ARG is line to duplicate."
  (interactive "*p")
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (save-excursion (beginning-of-line) (point))) eol)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        (while (> count 0)
          (newline)
          (insert line)
          (setq count (1- count))))
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  (next-line arg))

(global-set-key (kbd "C-d") 'duplicate-line)

;;; -----  SHIT
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(sudo-save which-key php-mode company-capf aggressive-indent treemacs-magit treemacs-icons-dired treemacs-projectile iedit popwin swiper-helm helm aggressive-intent aggresive-intent aggresive-intent-mode use-package telephone-line smartparens rainbow-delimiters projectile multiple-cursors magit lsp-ui lsp-dart klere-theme json-mode flycheck expand-region drag-stuff company-quickhelp company-lsp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
