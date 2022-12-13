(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; For some reason LSP only works with this?
(add-to-list 'load-path "/Users/ramsay/.emacs.d/straight/repos/f.el")

(use-package emacs
  :init
  (server-start)
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (hl-line-mode 1)
  (set-frame-font "Roboto Mono 12" nil t)
  (global-display-line-numbers-mode)
  (setq frame-title-format "")
  (setq ns-use-proxy-icon nil))

;; Keys

(use-package evil
  :straight t
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>h") 'windmove-left)
  (evil-define-key 'normal 'global (kbd "<leader>j") 'windmove-down)
  (evil-define-key 'normal 'global (kbd "<leader>k") 'windmove-up)
  (evil-define-key 'normal 'global (kbd "<leader>l") 'windmove-right)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'counsel-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>b") 'counsel-switch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
  )

(use-package evil-collection
  :straight t
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package evil-goggles
  :straight t
  :config (evil-goggles-mode))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))

;;(use-package ivy-posframe
;;  :straight t
;;  :custom (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;;  :custom-face
;;  (ivy-posframe-border ((t (:background "#ffffff"))))
;;  :config(ivy-posframe-mode))

;; files

(use-package counsel
  :straight t
  :config (counsel-mode)
  :bind
  ("M-x" . counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-x b" . counsel-switch-buffer)
  ("C-x C-f" . counsel-find-file))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode +1))

(use-package projectile
  :straight t
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-prefix " ")
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map))

(use-package vterm
  :config
  (add-to-list 'display-buffer-alist
     '("\*vterm\*"
       (display-buffer-in-side-window)
       (window-height . 0.25)
       (side . bottom)
       (slot . 0)))

  :bind
  ("C-`" . vterm))

;; Programming


                                        ; git intergration
(use-package magit
  :straight t
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (define-key magit-mode-map (kbd "<f5>") #'(lambda ()
                                              (interactive)
                                              (magit-refresh)
                                              (message "Refreshing Magit...done"))))

                                        ; Lsp

(use-package which-key
  :init (which-key-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config (lsp-enable-which-key-integration t)
  (add-hook 'lsp-configure-hook (lambda ()
                         (lsp-headerline-breadcrumb-mode -1))))

                                        ; Company completions
(use-package company
  :straight t
  :hook (prog-mode-hook . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0))

(use-package all-the-icons
  :straight t)

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package company-lsp :straight t)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
    (setq lsp-ui-doc-enable t
	lsp-ui-doc-use-childframe t
	lsp-ui-doc-position 'top
	lsp-ui-doc-include-signature t
	lsp-ui-sideline-enable nil
	lsp-ui-flycheck-enable t
	lsp-ui-flycheck-list-position 'right
	lsp-ui-flycheck-live-reporting t
	lsp-ui-peek-enable t
	lsp-ui-peek-list-width 60
	lsp-ui-peek-peek-height 25)
    :hook (lsp-mode . lsp-ui-mode))

                                        ; Java
(use-package lsp-java
  :straight t
  :config (add-hook 'java-mode-hook #'lsp))

                                        ; Ts
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config (setq typescript-indent-level 2))

                                        ; C/C++
(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)

                                        ; scala

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :config
  )

(use-package lsp-metals
  :hook (scala-mode . lsp))
                                        ; go
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Go-Eldoc - really nice docs

(use-package go-eldoc
  :ensure t
  :requires go-mode
  :hook (go-mode . go-eldoc-setup))

;; press M-, to compile
(use-package go-mode
:defer t
:ensure t
:mode ("\\.go\\'" . go-mode)
:init
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")  
  (setq compilation-read-command nil)
:bind (("M-," . compile)
("M-." . godef-jump)))

(setq compilation-window-height 14)
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
	(let* ((w (split-window-vertically))
	       (h (window-height w)))
	  (select-window w)
	  (switch-to-buffer "*compilation*")
	  (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(setq compilation-scroll-output t)


                                        ; templates
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  (latex-mode . yas-minor-mode))
(use-package yasnippet-snippets)

;; spelling

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Tramp

(setenv "dice" "/ssh:s2065708@student.ssh.inf.ed.ac.uk:")

;; Org

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex)
  (latex-mode . turn-on-org-cdlatex))

(use-package org
  :straight nil
  :config
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; Visual stuff
(use-package doom-themes)

(load-theme 'doom-github-dark t)
(global-hl-line-mode)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;;(straight-use-package
;; '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
;;
;;(require 'nano)

(straight-use-package
 '(ns-auto-titlebar :type git :host github :repo "purcell/ns-auto-titlebar"))

(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

(use-package good-scroll ; scrolling
  :config
  (good-scroll-mode 1)
  (setq scroll-margin 7)
  (setq scroll-step 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-color-icon nil)
 '(company-box-enable-icon t)
 '(custom-safe-themes
   '("be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" default))
 '(warning-suppress-log-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
