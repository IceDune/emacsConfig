(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell t)            ; Flash when the bell rings

;; Turn off some unneeded UI elements
(menu-bar-mode -1)  ; Leave this one on if you're a beginner!
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)       ; Disable tooltips  
(set-fringe-mode 10)    ; Give some breathing room

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Line Numbers and random config:
;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Relative line numbers
(setq display-line-numbers 'relative)

;; Column numbers
(column-number-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; command log mode
(use-package command-log-mode
  :ensure t
  :config
  (command-log-mode 1)
  (global-command-log-mode)
  (clm/open-command-log-buffer))  ;; This line enables command log mode permanently

;; Fonts and Themes:
;; Set FiraCode Nerd Font Mono as the default font
(set-face-attribute 'default nil
                    :family "FiraCode Nerd Font Mono"
		    :weight 'bold
                    :height 120)

;; FiraCode Nerd Font Mono doesnt support all icons, so add a fallback to render unsupported icons.
(set-fontset-font t 'unicode "FiraCode Nerd Font Mono" nil 'prepend)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

;; On first Install, you need to install the fonts on the system manually
;; M-x all-the-icons-install-fonts
;; M-x nerd-icons-install-fonts
;; go to the folder and manually install the fonts or select the systems font folder
(use-package all-the-icons
  :ensure t)
  
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
  
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


;; Package Manager:
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Ivy, Counsel:
;; ivy package
(use-package ivy
  :ensure t
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
  
;; Ensure ivy-mode is enabled at startup
(ivy-mode 1)

;; Description for entries appearing while using ivy
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
		 ("C-x b" . counsel-ibuffer)
		 ("C-x C-f" . counsel-find-file)
		 :map minibuffer-local-map
		 ("C-r" . counsel-minibuffer-history))
  :config
  (counsel-mode 1)
  ;; Dont start search with ^
  (setq ivy-initial-inputs-alist nil))

;; Switch between buffers
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Load which-key explicitly and configure it
(use-package which-key
  :ensure t  ;; Ensures package is installed
  :diminish which-key-mode  ;; Optional: Hide from mode line
  :init
  (which-key-mode)  ;; Enable which-key globally
  :config
  (setq which-key-idle-delay 0.5  ;; Show suggestions after 0.5s
        which-key-idle-secondary-delay 0.1  ;; Faster for subsequent popups
        which-key-sort-order 'which-key-key-order-alpha  ;; Sort keys alphabetically
        which-key-max-description-length 40  ;; Adjust description width
        which-key-popup-type 'side-window))  ;; Display on the side window


;; Shows cross references for variables/functions while using help
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; set your own key bindings
(use-package general
  :config
  (general-create-definer rune/leader-keys
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

;; Evil Mode
;; theres a fourth mode in evil called emacs mode (C-z), theres an orange icon at the bottom
;; here vim bindings are disabled and emacs bindings are enabled.

;; if you want to remove evil bindings from a particular mode, remove it from the evil-collection-mode-list

(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Set initial state for specific modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Add the hook to 'evil-mode-hook' to ensure the modes are set to 'emacs' state
(add-hook 'evil-mode-hook 'rune/evil-hook)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; for cyclic keys
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; set the file-directory below to your project folder
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)  ;; Enable projectile-mode
  (setq projectile-completion-system 'ivy)  ;; Set completion system
  (setq projectile-switch-project-action #'projectile-dired)  ;; Set default project action
  ;; NOTE: Set this to the folder where you keep your Git repos!
  :init
  (when (file-directory-p "D:/Projects/")
    (setq projectile-project-search-path '("D:/Projects/")))  ;; TODO: Update search path
  (setq projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map))  

(use-package counsel-projectile
  :config (counsel-projectile-mode))
  
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
;; evil-magit, part of evil collection

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)


(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


;; AutoGenerated Content:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key forge magit counsel-projectile projectile hydra evil-collection evil general helpful counsel ivy-rich ivy command-log-mode rainbow-delimiters all-the-icons doom-themes doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
