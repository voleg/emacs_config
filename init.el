
;; this is my Emacs configuration from scratch experience :D
;; note that: 
;; C-x means CONTROL-x
;; M-x means META-x (Command + x)
(set-frame-parameter nil 'fullscreen 'fullboth)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode -1)
(menu-bar-mode -1) 
;; (setq visible-bell t)
(set-face-attribute 'default nil :font "Fira Code" :height 150 :weight 'normal)

(load-theme 'ujelly t )

(add-hook 'text-mode-hook 'visual-line-mode)

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))
(setq use-package-always-ensure t)

;; (use-package command-log-mode)
(column-number-mode)

;; TODO: configure dired :) 
;; (use-package dired
;;   :ensure nil)

(use-package swiper :ensure t)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 32)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package magit)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 0))

(use-package org
  ;;:hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▼ ")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (setq org-babel-python-command "python3")
  (setq org-confirm-babel-evaluate nil)
  (setq org-support-shift-select t)
  )

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (file-truename "~/org-notes/roam"))
  (org-roam-db-autosync-mode)
;;  (org-roam-completion-everywhere t) 
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  
  :config
  (org-roam-setup))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package python-mode
  :ensure t
  :custom
  (python-shell-interpreter "python3"))

(use-package typescript-mode
  :ensure t
  :after (lsp-mode)
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))


(use-package lsp-pyright
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )

;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy)

(org-roam-node-find "week 45")

;; added here to be able to export from org to pdf with dicent fonts
;; "pdflatex" uses an old fixed size font ... 
(setq latex-run-command "xelatex")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2b3f1e6abe0f02ff73d95dca04901bdbc2ecebe80fa453eded34fa39c8b050cb" "0717ec4adc3308de8cdc31d1b1aef17dc61003f09cb5f058f77d49da14e809cf" "a00d7e35e213d38a8149a637d4e6b3a86b489df92243cae64c843eea78ca385c" "ca5770241443683049a9b95690b5f4ffb4322c75f429bf4e7c7b853e6c4be425" "a67b6cb65db241e033b6aed5eeaf0805a1b62e598cedc605c71d003a1d5c00c6" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" default))
 '(package-selected-packages
   '(helm-gitignore typescript-mode ujelly-theme reverse-theme hippo-themes flatland-black-theme cyberpunk-theme lsp-ivy markdown-mode lsp-mode python-mode org-roam magit counsel ivy-rich which-key rainbow-mode swiper rainbow-delimiters doom-modeline ivy use-package))
 '(warning-suppress-log-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vertical-border ((nil (:inherit mode-line-inactive)))))

