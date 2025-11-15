;;; === CONFIGURAÇÕES CRÍTICAS DE PERFORMANCE ===
;; Otimizações RADICAIS que devem vir ANTES de tudo
(setq gc-cons-threshold 400000000)  ; 400MB durante carregamento
(setq read-process-output-max (* 16 1024 1024))  ; 16MB para I/O
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq message-log-max 1000)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Performance de UI radical
(setq idle-update-delay 2.0)
(setq redisplay-skip-fontification-on-input t)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq jit-lock-stealth-time 10)
(setq jit-lock-defer-time 1.0)
(setq jit-lock-stealth-verbose nil)

;; Interface mínima ULTRA radical
(tool-bar-mode -1)
(menu-bar-mode -1) 
(scroll-bar-mode -1)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq inhibit-compacting-font-caches t)

;;; === INSTALAÇÃO DE PACOTES (MÍNIMA) ===
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;;; === EVIL MODE (SEM USE-PACKAGE) ===

;; Configurações de performance ANTES do carregamento
(setq evil-want-integration nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-respect-visual-line-mode nil)
(setq evil-search-module 'evil-search)
(setq evil-ex-complete-emacs-commands nil)
(setq evil-ex-interactive-search-history nil)

;; Carregar evil de forma organizada
(defun my-load-evil-mode ()
  "Carregar Evil Mode de forma correta."
  (package-initialize)  ; Inicializar pacotes aqui
  
  ;; Instalar Evil se necessário
  (unless (package-installed-p 'evil)
    (package-refresh-contents)
    (package-install 'evil))
  
  ;; Carregar e ativar Evil
  (when (require 'evil nil t)
    (evil-mode 1)
    
    ;; Configurar keybindings
    (define-key evil-normal-state-map (kbd "C-s") 'my-enhanced-isearch)
    (define-key evil-insert-state-map (kbd "C-s") 'my-enhanced-isearch)
    
    ;; Adicionar hooks de GC apenas se Evil carregou
    (add-hook 'evil-insert-state-entry-hook 'my-evil-optimize-gc)
    (add-hook 'evil-insert-state-exit-hook 'my-evil-restore-gc)
    
    ))

;; Executar após 0.5 segundos
(run-with-idle-timer 0.5 nil 'my-load-evil-mode)

;;; === OTIMIZAÇÕES ESPECÍFICAS PARA EVIL ===

;; GC otimizado para operações do Evil Mode
(defun my-evil-optimize-gc ()
  "Otimizar GC para operações do Evil Mode."
  (setq gc-cons-threshold 200000000))

(defun my-evil-restore-gc ()
  "Restaurar GC normal após operações do Evil."
  (setq gc-cons-threshold 80000000))

;;; === SISTEMA DE BUSCA/COMPLETION NATIVO ===

;; Icomplete (alternativa nativa ao Vertico)
(icomplete-mode 1)
(setq completion-styles '(basic partial-completion emacs22))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; Busca incremental nativa
(setq search-highlight t)
(setq search-whitespace-regexp ".*?")
(setq isearch-lazy-count t)
(setq isearch-allow-scroll t)

;;; === SISTEMA DE BUSCA RÁPIDO (CONSULT STYLE) ===

;; Busca em arquivos (similar a consult-find - C-c f)
(defun my-find-file-recursive ()
  "Buscar arquivo recursivamente no diretório (similar a C-c f)."
  (interactive)
  (let ((default-directory (read-directory-name "Diretório: ")))
    (find-file (completing-read "Buscar arquivo: " 
                               (my-list-files-recursively default-directory)))))

(defun my-list-files-recursively (directory)
  "Lista todos os arquivos em DIRECTORY recursivamente."
  (let (files)
    (dolist (file (directory-files directory t nil t))
      (unless (member (file-name-nondirectory file) '("." ".."))
        (if (file-directory-p file)
            (setq files (append files (my-list-files-recursively file)))
          (push file files))))
    files))

;; Busca com find (mais rápido para muitos arquivos)
(defun my-find-file-with-find ()
  "Buscar arquivos usando comando find (rápido)."
  (interactive)
  (let ((default-directory (read-directory-name "Diretório: "))
        (pattern (read-string "Padrão (ex: *.el, *.js): " "*")))
    (find-file (completing-read "Selecionar arquivo: "
                               (split-string 
                                (shell-command-to-string 
                                 (format "find . -name \"%s\" -type f | head -100" pattern)) "\n" t)))))

;; Busca de texto em arquivos (similar a consult-ripgrep - C-c r)
(defun my-grep-in-project ()
  "Buscar texto em arquivos do projeto (similar a C-c r)."
  (interactive)
  (let ((pattern (read-string "Texto para buscar: "))
        (directory (read-directory-name "Diretório: " default-directory)))
    (grep (format "grep -nH -r \"%s\" %s" pattern directory))))

(defun my-grep-in-current-dir ()
  "Buscar texto no diretório atual (rápido)."
  (interactive)
  (let ((pattern (read-string "Texto para buscar: ")))
    (grep (format "grep -nH -r \"%s\" ." pattern))))

;; Busca incremental melhorada (similar a consult-line - C-s)
(defun my-enhanced-isearch ()
  "Busca incremental com melhorias."
  (interactive)
  (isearch-forward)
  (isearch-yank-string (thing-at-point 'symbol)))

;; Occur melhorado (busca poderosa no buffer)
(defun my-occur-symbol ()
  "Buscar símbolo atual no buffer com occur."
  (interactive)
  (occur (thing-at-point 'symbol)))

(defun my-occur-project ()
  "Buscar texto em todos os buffers abertos."
  (interactive)
  (let ((pattern (read-string "Buscar em todos buffers: ")))
    (multi-occur (buffer-list) pattern)))

;;; === CONFIGURAÇÕES PESSOAIS (Mínimas) ===
(defun display-warning (&rest _args) nil)
(setq warning-minimum-level :emergency)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-c c") 'compile)

;; CONFIGURAÇÃO DOS ATALHOS DE BUSCA
(defun my-find-files ()
  "Buscar arquivos recursivamente no diretório."
  (interactive)
  (let ((dir (read-directory-name "Diretório: "))
        (pattern (read-string "Padrão do arquivo (ex: *.txt): " "*")))
    (find-name-dired dir pattern)))

(global-set-key (kbd "C-s") 'my-enhanced-isearch)        ; Busca incremental
(global-set-key (kbd "C-c f") 'my-find-files)            ; Busca arquivos
(global-set-key (kbd "C-c r") 'my-grep-in-current-dir)   ; Buscar texto em arquivos

;; Configurações de tab
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;;; === CONFIGURAÇÕES DE PERFORMANCE RADICAIS ===

;; GC inteligente AGGRESSIVO
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold 800000000))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 400000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Sistema de arquivos ULTRA rápido
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-list-file-prefix nil)

;;; === EDITING ESSENTIALS ===

;; Electric pair - leve
(electric-pair-mode 1)

;; Show-paren nativo (mais leve que git-gutter para parênteses)
(show-paren-mode 1)
(setq show-paren-delay 0)

;;; === GIT INTEGRATION NATIVA ===

;; VC (Version Control) nativo - muito mais leve
(setq vc-handled-backends '(Git))
(setq vc-follow-symlinks t)

;; Mode-line com info do Git nativo
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

;;; === MODOS DE LINGUAGEM (Sob demanda REAL) ===

;; TypeScript nativo (se disponível)
(when (fboundp 'typescript-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode)))

;; Go nativo  
(when (fboundp 'go-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))

;; Elixir - usar modo fundamental se não disponível
(add-to-list 'auto-mode-alist '("\\.exs?\\'" . prog-mode))

;;; === CONFIGURAÇÕES DE APARÊNCIA ===
(load-theme 'wombat t)

;; Fontes - ADIAR
(run-with-idle-timer 5 nil
  (lambda ()
    (add-to-list 'default-frame-alist '(font . "Iosevka-14"))))

;; Mode-line minimalista
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

;;; === NUMERAÇÃO DE LINHAS INTELIGENTE ===
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; Desativar em modos desnecessários
(dolist (hook '(dired-mode-hook vterm-mode-hook shell-mode-hook 
              eshell-mode-hook compilation-mode-hook help-mode-hook 
              info-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

;;; === MAGIT NATIVO ALTERNATIVES ===

;; Comandos Git básicos via comando shell
(defun my-git-status ()
  "Git status usando comando nativo."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (if default-directory
        (compile "git status")
      (message "Not a git repository"))))

(defun my-git-log ()
  "Git log usando comando nativo."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (if default-directory
        (compile "git log --oneline -n 20")
      (message "Not a git repository"))))

(global-set-key (kbd "C-x g") 'my-git-status)
(global-set-key (kbd "C-x l") 'my-git-log)

;;; === CONFIGURAÇÕES ADICIONAIS (ADIADAS) ===

;; C mode
(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-j") 'eval-print-last-sexp)))

;; Word wrap
(add-hook 'markdown-mode-hook (lambda () (toggle-word-wrap 1)))

;; Modos de arquivo
(dolist (pair '(("Cask" . emacs-lisp-mode)
                ("\\.html\\'" . nxml-mode)
                ("\\.xsd\\'" . nxml-mode)
                ("\\.ant\\'" . nxml-mode)
                ("\\.ebi\\'" . lisp-mode)))
  (add-to-list 'auto-mode-alist pair))

;;; === CONFIGURAÇÕES FINAIS ===

;; Confirmações automáticas
(defun my-always-yes (&rest args) t)
(advice-add 'yes-or-no-p :override 'my-always-yes)
(advice-add 'y-or-n-p :override 'my-always-yes)

(setq confirm-nonexistent-file-or-buffer nil)
(setq confirm-kill-processes nil)
(setq confirm-kill-emacs nil)

;; Maximizar - ADIAR
(run-with-idle-timer 3 nil 'toggle-frame-maximized)

;; Reset FINAL do GC e re-ativar auto-save
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 80000000)  ; 80MB após startup
    (setq auto-save-default t)  ; Re-ativar auto-save
    (message "Emacs carregado em %.2f segundos" 
             (float-time (time-subtract after-init-time before-init-time)))))

;;; === CONFIGURAÇÃO DO ARQUIVO CUSTOM ===
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(defun my-save-custom-file ()
  "Salvar customizações no arquivo correto."
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (custom-save-all))

(add-hook 'after-init-hook 'my-save-custom-file)

;; Mensagem final MINIMAL
(message "Iniciando Emacs ULTRA LEVE...")
