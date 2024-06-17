;; this is my Emacs configuration from scratch experience :D
;; note that: 
;; C-x means CONTROL-x
;; M-x means META-x (Command + x)
(set-frame-parameter nil 'fullscreen 'fullboth)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
;; (set-fringe-mode -1)   ;; hide git diff indication 
(menu-bar-mode -1)
;; (setq visible-bell t)
(delete-selection-mode 1)

(desktop-save-mode 1)
(setq-default tab-width 2)

;; (set-face-attribute 'default nil :font "Fira Code" :height 150 :weight 'normal)
(set-face-attribute 'default nil :font "Iosevka Curly" :height 130 :weight 'normal)
;; (set-face-attribute 'default nil :font "Operator Mono" :height 190 :weight 'demilight)
;; (set-face-attribute 'default nil :font "PragmataPro" :height 150 :weight 'light)


(custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "ETBookOT" :height 180 :weight thin))))
   '(fixed-pitch ((t (:family "Fira Code" :height 150 :weight thin)))))

(custom-theme-set-faces
 'user
 '(org-mode ((t (:inherit 'fixed-pitch))))
 )

;; (custom-theme-set-faces
;;  'user
;;   '(org-date ((t (:inherit 'fixed-pitch)))))
;; (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)

;; TODO you need to install it first ... then load :)
;; (load-theme 'ujelly t )
(load-theme 'tsdh-light t)

(add-hook 'text-mode-hook 'visual-line-mode)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

;; (setq package-archive-priorities '(("melpa"    . 5)
;;                                   ("jcs-elpa" . 0)))
;; for MacOS 
(use-package exec-path-from-shell
	:init (exec-path-from-shell-initialize))

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

;; Sort Directories first on top
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

;; TODO: configure dired :) 
;; (use-package dired
;;   :ensure nil)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

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
				 ("C-x C-b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 24)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; i had to (setq package-check-signature nil)
(use-package magit
	:ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
	:config (setq magit-save-repository-buffers 'dontask)
	)

;; (use-package forge
;; 	:ensure t
;;   :after magit)

;; (use-package diff-hl
;;   :ensure t
;;   :init (global-diff-hl-mode)
;;   )

(use-package git-gutter
  :ensure t
	:hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
	:config
	(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
	(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
	(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; (use-package mixed-pitch
;;   :ensure t
;;   :hook
;;   ;; If you want it in all text modes:
;;   (text-mode . mixed-pitch-mode)
;;   (org-date . fixed-pitch))

(defun helje/org-mode-rendered-image-inline ()
  (define-key org-mode-map (kbd "C-c C-c")
    (lambda () (interactive) (org-ctrl-c-ctrl-c)
      (org-display-inline-images)))
  )

(defun helje/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode -1)
  (auto-fill-mode 0)
  (visual-line-mode 0)
  (setq org-list-allow-alphabetical t)
  (setq org-todo-keyword-faces
	'(("WORKING" . "orange")
	  ("CANCELLED" . "grey")))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WORKING(n)" "WAITING(w)"
		    "POSTPONED(p)" "|" "DONE(d)" "CANCELLED(c)")))
  (helje/org-mode-rendered-image-inline)
  )

(use-package org
  :hook (org-mode . helje/org-mode-setup)
  :config
  (setq org-ellipsis " ▼ ")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (plantuml . t)
     (latex . t)
     ))
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

(use-package plantuml-mode
  :init
  (setq plantuml-default-exec-mode 'jar) ;; 'server
  ;; (setq plantuml-server-url "http://http://localhost:8080")
  (setq plantuml-jar-path "~/plantuml.jar")
    (setq org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))
    (setq org-startup-with-inline-images t)
    ;;(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; With this you can add #+begin_src ... :async 
(use-package ob-async
  :ensure t
  :after plantuml-mode
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration t)
  )

(use-package with-venv
  :ensure t)

(defun helje/dap-conf ()
	(dap-mode 1)
	;; The modes below are optional
	(dap-ui-mode 1)
	;; enables mouse hover support
	(dap-tooltip-mode 1)
	;; use tooltips for mouse hover
	;; if it is not enabled `dap-mode' will use the minibuffer.
	(tooltip-mode 1)
	;; displays floating panel with debug buttons
	;; requies emacs 26+
	(dap-ui-controls-mode 1)
  (load (expand-file-name "/Users/voleg/Projects/emacs_config/debug/python-debug.el"))
	)

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
  :config
	(require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))
  (helje/dap-conf)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
	
	)

(use-package python-mode
  :ensure t
  :custom
  ;; (python-shell-interpreter "python3")
	)

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package flycheck)
;; Load some flycheck custom stuff
(load (expand-file-name "/Users/voleg/Projects/emacs_config/tools/flycheck-ruff-mypy.el"))

(use-package prettier-js
  :ensure t
  :hook ((js-mode typescript-mode) . prettier-js-mode))

(use-package typescript-mode
  :ensure t
  :after (lsp-mode)
  :mode "\\.[tj]s\\'"
  :hook (typescript-mode . lsp)
  :config
  (setq typescript-indent-level 2))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package lsp-pyright
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :hook
	((python-mode . (lambda ()
                    (require 'lsp-pyright)
                    (lsp)))
   ;; This hack is necessary to make additional flycheck checkers work in lsp-mode
   (flycheck-mode . (lambda ()
                      (flycheck-add-next-checker 'lsp 'python-ruff)
                      (flycheck-add-next-checker 'python-ruff 'python-mypy)
                      (message "Added flycheck checkers."))))
	;; (python-mode . (lambda ()
	;; 								 (require 'lsp-pyright)
	;; 								 (lsp)))
	
	)  ; lsp or lsp-deferred

;; (use-package poetry
;;  :ensure t
;;  :hook
;;  (python-mode . poetry-tracking-mode))

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

(use-package yafolding
  :ensure t
  :hook
  (prog-mode . yafolding-mode)
  )

;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy)

(use-package dockerfile-mode
	:ensure t)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; for docker-wrapped development environment with LSP / DAP
;; https://emacs-lsp.github.io/lsp-mode/tutorials/docker-integration/
(use-package lsp-docker)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/")
    (setq projectile-project-search-path '("~/Projects/")))
  (setq projectile-switch-project-action #'projectile-dired))

(setq enable-local-variables :all)
;; Add a directory to the safe list
(defun add-to-safe-local-variable-values (directory)
  (let ((project-dir (expand-file-name directory)))
    (unless (assoc project-dir safe-local-variable-values)
      (push (cons 'project-directory project-dir) safe-local-variable-values))))

;; Example usage for a specific project
(add-to-safe-local-variable-values "~/Projects")


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-c C-y" . yas-expand))  
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt)))

(use-package codegpt :ensure t)
(setq codegpt-tunnel 'completion            ; The default is 'completion
      codegpt-model "gpt-3.5-turbo")  ; You can pick any model you want!

;; TODO Play with Anthropic API (Claude)

(use-package gptel
  :ensure t)

(gptel-make-anthropic "Claude"          ;Any name you want
  :stream t                             ;Streaming responses
  :key "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")

;; added here to be able to export from org to pdf with dicent fonts
;; "pdflatex" uses an old fixed size font ... 
(setq latex-run-command "xelatex")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("3a1a6a9cbff383a7122f7b2e5be7ca3c3951cab4705d2303c887368693c75fd3" "2b3f1e6abe0f02ff73d95dca04901bdbc2ecebe80fa453eded34fa39c8b050cb" "0717ec4adc3308de8cdc31d1b1aef17dc61003f09cb5f058f77d49da14e809cf" "a00d7e35e213d38a8149a637d4e6b3a86b489df92243cae64c843eea78ca385c" "ca5770241443683049a9b95690b5f4ffb4322c75f429bf4e7c7b853e6c4be425" "a67b6cb65db241e033b6aed5eeaf0805a1b62e598cedc605c71d003a1d5c00c6" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" default))
 '(package-selected-packages
	 '(git-gutter-fringe git-gutter forge gitlab-ci-mode rg flycheck pet gptel editorconfig prettier-js plantuml-mode docker lsp-docker hyperdrive yafolding mixed-pitch yasnippet poetry projectile helm-gitignore typescript-mode ujelly-theme reverse-theme hippo-themes flatland-black-theme cyberpunk-theme lsp-ivy markdown-mode lsp-mode python-mode org-roam magit counsel ivy-rich which-key rainbow-mode swiper rainbow-delimiters doom-modeline ivy use-package))
 '(warning-suppress-log-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Fira Code" :height 150 :weight thin))))
 '(org-mode ((t (:inherit 'fixed-pitch))))
 '(variable-pitch ((t (:family "ETBookOT" :height 180 :weight thin))))
 '(vertical-border ((nil (:inherit mode-line-inactive)))))

;; (org-roam-node-find "week 2024 22")
