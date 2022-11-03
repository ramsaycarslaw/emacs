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
  )

(use-package evil-collection
  :straight t
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

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


;; Visual stuff

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano)

(straight-use-package
 '(ns-auto-titlebar :type git :host github :repo "purcell/ns-auto-titlebar"))

(when (eq system-type 'darwin) (ns-auto-titlebar-mode))
