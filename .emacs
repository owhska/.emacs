;;; === CRITICAL PERFORMANCE CONFIGURATIONS ===
;; RADICAL optimizations that must come BEFORE everything
(setq gc-cons-threshold 400000000)  ; 400MB during loading
(setq read-process-output-max (* 16 1024 1024))  ; 16MB for I/O
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq message-log-max 1000)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Radical UI performance
(setq idle-update-delay 2.0)
(setq redisplay-skip-fontification-on-input t)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq jit-lock-stealth-time 10)
(setq jit-lock-defer-time 1.0)
(setq jit-lock-stealth-verbose nil)

;; ULTRA radical minimal interface
(tool-bar-mode -1)
(menu-bar-mode -1) 
(scroll-bar-mode -1)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq inhibit-compacting-font-caches t)

;;; === PACKAGE INSTALLATION (MINIMAL) ===
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;;; === EVIL MODE (WITHOUT USE-PACKAGE) ===

;; Performance configurations BEFORE loading
(setq evil-want-integration nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-respect-visual-line-mode nil)
(setq evil-search-module 'evil-search)
(setq evil-ex-complete-emacs-commands nil)
(setq evil-ex-interactive-search-history nil)

;; Load evil mode in an organized way
(defun my-load-evil-mode ()
  "Load Evil Mode correctly."
  (package-initialize)  ; Initialize packages here
  
  ;; Install Evil if necessary
  (unless (package-installed-p 'evil)
    (package-refresh-contents)
    (package-install 'evil))
  
  ;; Load and activate Evil
  (when (require 'evil nil t)
    (evil-mode 1)
    
    ;; Configure keybindings
    (define-key evil-normal-state-map (kbd "C-s") 'my-enhanced-isearch)
    (define-key evil-insert-state-map (kbd "C-s") 'my-enhanced-isearch)
    
    ;; Add GC hooks only if Evil loaded
    (add-hook 'evil-insert-state-entry-hook 'my-evil-optimize-gc)
    (add-hook 'evil-insert-state-exit-hook 'my-evil-restore-gc)
    
    ))

;; Execute after 0.5 seconds
(run-with-idle-timer 0.5 nil 'my-load-evil-mode)

;;; === EVIL MODE SPECIFIC OPTIMIZATIONS ===

;; Optimized GC for Evil Mode operations
(defun my-evil-optimize-gc ()
  "Optimize GC for Evil Mode operations."
  (setq gc-cons-threshold 200000000))

(defun my-evil-restore-gc ()
  "Restore normal GC after Evil operations."
  (setq gc-cons-threshold 80000000))

;;; === NATIVE SEARCH/COMPLETION SYSTEM ===

;; Icomplete (native alternative to Vertico)
(icomplete-mode 1)
(setq completion-styles '(basic partial-completion emacs22))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; Native incremental search
(setq search-highlight t)
(setq search-whitespace-regexp ".*?")
(setq isearch-lazy-count t)
(setq isearch-allow-scroll t)

;;; === FAST SEARCH SYSTEM (CONSULT STYLE) ===

;; File search (similar to consult-find - C-c f)
(defun my-find-file-recursive ()
  "Search for files recursively in directory (similar to C-c f)."
  (interactive)
  (let ((default-directory (read-directory-name "Directory: ")))
    (find-file (completing-read "Search file: " 
                               (my-list-files-recursively default-directory)))))

(defun my-list-files-recursively (directory)
  "List all files in DIRECTORY recursively."
  (let (files)
    (dolist (file (directory-files directory t nil t))
      (unless (member (file-name-nondirectory file) '("." ".."))
        (if (file-directory-p file)
            (setq files (append files (my-list-files-recursively file)))
          (push file files))))
    files))

;; Find search (faster for many files)
(defun my-find-file-with-find ()
  "Search files using find command (fast)."
  (interactive)
  (let ((default-directory (read-directory-name "Directory: "))
        (pattern (read-string "Pattern (ex: *.el, *.js): " "*")))
    (find-file (completing-read "Select file: "
                               (split-string 
                                (shell-command-to-string 
                                 (format "find . -name \"%s\" -type f | head -100" pattern)) "\n" t)))))

;; Text search in files (similar to consult-ripgrep - C-c r)
(defun my-grep-in-project ()
  "Search text in project files (similar to C-c r)."
  (interactive)
  (let ((pattern (read-string "Text to search: "))
        (directory (read-directory-name "Directory: " default-directory)))
    (grep (format "grep -nH -r \"%s\" %s" pattern directory))))

(defun my-grep-in-current-dir ()
  "Search text in current directory (fast)."
  (interactive)
  (let ((pattern (read-string "Text to search: ")))
    (grep (format "grep -nH -r \"%s\" ." pattern))))

;; Enhanced incremental search (similar to consult-line - C-s)
(defun my-enhanced-isearch ()
  "Enhanced incremental search."
  (interactive)
  (isearch-forward)
  (isearch-yank-string (thing-at-point 'symbol)))

;; Improved occur (powerful buffer search)
(defun my-occur-symbol ()
  "Search current symbol in buffer with occur."
  (interactive)
  (occur (thing-at-point 'symbol)))

(defun my-occur-project ()
  "Search text in all open buffers."
  (interactive)
  (let ((pattern (read-string "Search in all buffers: ")))
    (multi-occur (buffer-list) pattern)))

;;; === PERSONAL CONFIGURATIONS (Minimal) ===
(defun display-warning (&rest _args) nil)
(setq warning-minimum-level :emergency)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-c c") 'compile)

;; SEARCH SHORTCUTS CONFIGURATION
(defun my-find-files ()
  "Search files recursively in directory."
  (interactive)
  (let ((dir (read-directory-name "Directory: "))
        (pattern (read-string "File pattern (ex: *.txt): " "*")))
    (find-name-dired dir pattern)))

(global-set-key (kbd "C-s") 'my-enhanced-isearch)        ; Incremental search
(global-set-key (kbd "C-c f") 'my-find-files)            ; File search
(global-set-key (kbd "C-c r") 'my-grep-in-current-dir)   ; Text search in files

;; Tab settings
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;;; === RADICAL PERFORMANCE CONFIGURATIONS ===

;; AGGRESSIVE intelligent GC
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold 800000000))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 400000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; ULTRA fast file system
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-list-file-prefix nil)

;;; === EDITING ESSENTIALS ===

;; Electric pair - lightweight
(electric-pair-mode 1)

;; Native show-paren (lighter than git-gutter for parentheses)
(show-paren-mode 1)
(setq show-paren-delay 0)

;;; === NATIVE GIT INTEGRATION ===

;; Native VC (Version Control) - much lighter
(setq vc-handled-backends '(Git))
(setq vc-follow-symlinks t)

;; Mode-line with native Git info
(setq mode-line-format
  '("%e"
    " "
    mode-line-buffer-identification
    " "
    (:eval (when (and vc-mode (not (string= vc-mode "")))
             (propertize (format " %s " vc-mode)
                         'face '(:foreground "#986801" :weight bold))))
    " "
    (:eval (propertize (format " %s " (capitalize (symbol-name major-mode)))
                       'face '(:foreground "#4078f2" :weight bold)))
    " "
    mode-line-misc-info))

;;; === LANGUAGE MODES (REAL on-demand) ===

;; Native TypeScript (if available)
(when (fboundp 'typescript-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode)))

;; Native Go  
(when (fboundp 'go-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))

;; Elixir - use fundamental mode if not available
(add-to-list 'auto-mode-alist '("\\.exs?\\'" . prog-mode))

;;; === APPEARANCE CONFIGURATIONS ===
(load-theme 'wombat t)

;; Fonts - DEFER
(run-with-idle-timer 5 nil
  (lambda ()
    (add-to-list 'default-frame-alist '(font . "Iosevka-14"))))

;; Minimalist mode-line
(set-face-attribute 'mode-line nil
                    :background "#f0f0f0"
                    :foreground "#333333"
                    :box '(:line-width 1 :color "#cccccc")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#f8f8f8"
                    :foreground "#888888"
                    :box '(:line-width 1 :color "#dddddd"))

;;; === INTELLIGENT LINE NUMBERING ===
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; Disable in unnecessary modes
(dolist (hook '(dired-mode-hook vterm-mode-hook shell-mode-hook 
              eshell-mode-hook compilation-mode-hook help-mode-hook 
              info-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

;;; === NATIVE MAGIT ALTERNATIVES ===

;; Basic Git commands via shell command
(defun my-git-status ()
  "Git status using native command."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (if default-directory
        (compile "git status")
      (message "Not a git repository"))))

(defun my-git-log ()
  "Git log using native command."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (if default-directory
        (compile "git log --oneline -n 20")
      (message "Not a git repository"))))

(global-set-key (kbd "C-x g") 'my-git-status)
(global-set-key (kbd "C-x l") 'my-git-log)

;;; === ADDITIONAL CONFIGURATIONS (DEFERRED) ===

;; C mode
(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-j") 'eval-print-last-sexp)))

;; Word wrap
(add-hook 'markdown-mode-hook (lambda () (toggle-word-wrap 1)))

;; File modes
(dolist (pair '(("Cask" . emacs-lisp-mode)
                ("\\.html\\'" . nxml-mode)
                ("\\.xsd\\'" . nxml-mode)
                ("\\.ant\\'" . nxml-mode)
                ("\\.ebi\\'" . lisp-mode)))
  (add-to-list 'auto-mode-alist pair))

;;; === FINAL CONFIGURATIONS ===

;; Automatic confirmations
(defun my-always-yes (&rest args) t)
(advice-add 'yes-or-no-p :override 'my-always-yes)
(advice-add 'y-or-n-p :override 'my-always-yes)

(setq confirm-nonexistent-file-or-buffer nil)
(setq confirm-kill-processes nil)
(setq confirm-kill-emacs nil)

;; Maximize - DEFER
(run-with-idle-timer 3 nil 'toggle-frame-maximized)

;; FINAL GC reset and re-enable auto-save
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 80000000)  ; 80MB after startup
    (setq auto-save-default t)  ; Re-enable auto-save
    (message "Emacs loaded in %.2f seconds" 
             (float-time (time-subtract after-init-time before-init-time)))))

;;; === CUSTOM FILE CONFIGURATION ===
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(defun my-save-custom-file ()
  "Save customizations to correct file."
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (custom-save-all))

(add-hook 'after-init-hook 'my-save-custom-file)

;; MINIMAL final message
(message "Starting ULTRA LIGHT Emacs...")
