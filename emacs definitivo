;;; === ULTRA LIGHT EMACS CONFIG ===
;; Vanilla + Evil (única exceção)
;; Cross-platform: Windows e Linux/WSL

;;; === RADICAL PERFORMANCE ===
(defvar my-emacs-start-time (current-time))

(setq gc-cons-threshold 400000000)
(setq read-process-output-max (* 16 1024 1024))
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq message-log-max 1000)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; UI performance
(setq idle-update-delay 1.0)
(setq redisplay-skip-fontification-on-input t)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)

(icomplete-mode 1)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix t)
(setq icomplete-prospects-height 1)

;; Minimal interface
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq inhibit-compacting-font-caches t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; === AJUSTES ESPECÍFICOS DE PLATAFORMA ===

(defconst my-on-windows (eq system-type 'windows-nt))
(defconst my-on-linux   (memq system-type '(gnu/linux gnu berkeley-unix)))

(when my-on-windows
  ;; Garantir o HOME no Windows
  (setenv "HOME" "C:/Users/mathe")
  (setq default-directory "C:/Users/mathe/")
  ;; Shell usado pelo compile / shell-command / M-x shell
  (setq shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (setq shell-command-switch "-c")
  (setq explicit-shell-file-name shell-file-name))

(when my-on-linux
  ;; No Linux o shell padrão do sistema já funciona bem
  (setq shell-file-name (or (getenv "SHELL") "/bin/bash"))
  (setq shell-command-switch "-c"))

;;; === PACKAGE.EL (só pro evil) ===
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

(setq evil-want-integration t)
(setq evil-want-keybinding nil) ;; evita conflito, mesmo sem evil-collection
(setq evil-undo-system 'undo-redo)

(require 'evil)
(evil-mode 1)

;;; === CONFIGURAÇÃO DE COMPLETION COM SUGESTÕES AUTOMÁTICAS ===

(setq completion-auto-help t)
(setq completion-auto-select 'next)
(setq completion-show-help t)
(setq completions-detailed t)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-min-length 1) ;; Mostra sugestões após 1 caractere
(setq completion-cycle-threshold 3)
(setq completion-styles '(flex basic partial-completion))

;; Habilita shadows de arquivos
(setq file-name-shadow-mode t)
(setq file-name-shadow-properties '(invisible t))

;;; === DIRED + EVIL (sem quebrar os binds nativos) ===

(evil-set-initial-state 'dired-mode 'motion)

(with-eval-after-load 'wdired
  (evil-set-initial-state 'wdired-mode 'normal))

(with-eval-after-load 'dired
  (evil-define-key 'motion dired-mode-map
    (kbd "RET") 'dired-find-file
    "l"         'dired-find-file
    "h"         'dired-up-directory))

;;; === DIRED: ls-lisp só no Windows ===
;; No Linux o dired já usa o `ls` real do sistema (dired-use-ls-dired t
;; por padrão), então nada disso é necessário nem desejável lá.

(when my-on-windows
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-verbosity nil)
  (setq dired-use-ls-dired nil))

(setq dired-listing-switches
      (if my-on-windows "-al" "-alh --group-directories-first"))
(setq dired-auto-revert-buffer t)
(put 'dired-find-alternate-file 'disabled nil)

;;; === GIT ===

(defun my-git-status ()
  "Git status."
  (interactive)
  (let ((git-dir (locate-dominating-file default-directory ".git")))
    (if git-dir
        (compile "git status")
      (message "Not a git repo"))))

(defun my-git-branch ()
  "Current git branch."
  (when-let ((git-dir (locate-dominating-file default-directory ".git")))
    (with-temp-buffer
      (cd git-dir)
      (when (zerop (call-process "git" nil t nil "branch" "--show-current"))
        (string-trim (buffer-string))))))

(defun my-git-log ()
  "Git log compacto."
  (interactive)
  (let ((git-dir (locate-dominating-file default-directory ".git")))
    (if git-dir
        (compile "git log --oneline --graph --decorate -30")
      (message "Not a git repo"))))

(defun my-grep-rn ()
  "Grep -rn com busca recursiva no diretório atual.
Mostra apenas 'Buscar: ' no prompt."
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (pattern (read-string "Grep: ")))
    (grep (format "grep -rn --color=always \"%s\" %s" pattern dir))))


;;; === HARPOON (equivalente ao harpoon.nvim, com persistência) ===
;; SPC a          -> marca o arquivo atual        (mark.add_file)
;; C-e            -> abre/fecha o quick-menu       (ui.toggle_quick_menu)
;; SPC 1/2/3/4    -> pula pro arquivo marcado N    (ui.nav_file)
;;
;; No quick-menu: RET visita o arquivo da linha e fecha o menu;
;; "d" apaga a linha (sem fechar); "C-c C-c" salva a ordem/edições
;; feitas no buffer de volta na lista (e no disco); "q" fecha sem salvar.
;;
;; Persistência: cada projeto (raiz do git, ou default-directory se não
;; houver .git) tem sua própria lista, igual o harpoon de verdade.
;; Tudo fica num único arquivo Elisp legível em
;; `my-harpoon-data-file', carregado uma vez por sessão e regravado a
;; cada mudança.

(defconst my-harpoon-max 4
  "Número de slots do harpoon (bate com os binds SPC 1-4).")

(defvar my-harpoon-buffer-name "*harpoon*")

(defvar my-harpoon-data-file
  (expand-file-name "harpoon-projects.el" user-emacs-directory)
  "Arquivo onde as listas de todos os projetos são persistidas.")

(defvar my-harpoon-db nil
  "Alist (raiz-do-projeto . lista-de-arquivos), espelho em memória do disco.")

(defvar my-harpoon-db-loaded nil
  "Não-nil depois que `my-harpoon-data-file' já foi lido nesta sessão.")

(defun my-harpoon-project-root ()
  "Raiz do projeto atual: diretório do .git, ou default-directory."
  (or (locate-dominating-file default-directory ".git")
      default-directory))

(defun my-harpoon-load-db ()
  "Carrega `my-harpoon-db' do disco, uma única vez por sessão."
  (unless my-harpoon-db-loaded
    (setq my-harpoon-db
          (when (file-exists-p my-harpoon-data-file)
            (with-temp-buffer
              (insert-file-contents my-harpoon-data-file)
              (ignore-errors (read (current-buffer))))))
    (setq my-harpoon-db-loaded t)))

(defun my-harpoon-save-db ()
  "Grava `my-harpoon-db' inteiro em `my-harpoon-data-file'."
  (make-directory (file-name-directory my-harpoon-data-file) t)
  (with-temp-file my-harpoon-data-file
    (prin1 my-harpoon-db (current-buffer))))

(defun my-harpoon-current-files ()
  "Lista de arquivos marcados no projeto atual."
  (my-harpoon-load-db)
  (cdr (assoc (my-harpoon-project-root) my-harpoon-db)))

(defun my-harpoon-set-current-files (files)
  "Substitui a lista do projeto atual por FILES e persiste no disco."
  (my-harpoon-load-db)
  (let ((root (my-harpoon-project-root)))
    (setf (alist-get root my-harpoon-db nil nil #'equal) files))
  (my-harpoon-save-db))

(defun my-harpoon-add-file ()
  "Marca o arquivo do buffer atual no projeto atual."
  (interactive)
  (let* ((file (buffer-file-name))
         (files (my-harpoon-current-files)))
    (cond
     ((not file) (message "Harpoon: buffer sem arquivo associado"))
     ((member file files) (message "Harpoon: já marcado (%s)" (file-name-nondirectory file)))
     ((>= (length files) my-harpoon-max)
      (message "Harpoon cheio (%d/%d) — abra o menu (C-e) e apague um slot antes"
               my-harpoon-max my-harpoon-max))
     (t
      (my-harpoon-set-current-files (append files (list file)))
      (message "Harpoon [%d/%d]: %s"
               (1+ (length files)) my-harpoon-max (file-name-nondirectory file))))))

(defun my-harpoon-nav-file (n)
  "Vai para o N-ésimo arquivo marcado (1-indexado) no projeto atual."
  (let ((file (nth (1- n) (my-harpoon-current-files))))
    (if file
        (find-file file)
      (message "Harpoon: slot %d vazio" n))))

(defvar my-harpoon-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")     'my-harpoon-menu-visit)
    (define-key map (kbd "q")       'my-harpoon-menu-quit)
    (define-key map (kbd "d")       'my-harpoon-menu-delete-line)
    (define-key map (kbd "C-c C-c") 'my-harpoon-menu-save)
    map)
  "Keymap do buffer de quick-menu do harpoon.")

(define-derived-mode my-harpoon-menu-mode fundamental-mode "Harpoon"
  "Modo do quick-menu do harpoon: uma linha por arquivo marcado."
  (use-local-map my-harpoon-menu-map))

(defun my-harpoon--line-file ()
  "Caminho absoluto do arquivo referenciado na linha atual, ou nil
se a linha estiver vazia ou for um comentário/instrução."
  (let ((line (string-trim (thing-at-point 'line t))))
    (unless (or (string-empty-p line) (string-prefix-p ";;" line))
      (expand-file-name line))))

(defun my-harpoon-render-menu ()
  "(Re)desenha a lista do projeto atual no buffer do menu."
  (let ((buf (get-buffer-create my-harpoon-buffer-name))
        (files (my-harpoon-current-files)))
    (with-current-buffer buf
      (my-harpoon-menu-mode)
      (erase-buffer)
      (if files
          (dolist (f files)
            (insert (format "%s\n" (abbreviate-file-name f))))
        (insert ";; nenhum arquivo marcado neste projeto — SPC a no arquivo desejado\n"))
      (goto-char (point-min)))
    buf))

(defun my-harpoon-toggle-menu ()
  "Abre/fecha a janela do quick-menu (toggle, como no nvim)."
  (interactive)
  (let* ((buf (get-buffer my-harpoon-buffer-name))
         (win (and buf (get-buffer-window buf))))
    (if win
        (delete-window win)
      (select-window (split-window-below -8))
      (switch-to-buffer (my-harpoon-render-menu)))))

(defun my-harpoon-menu-quit ()
  "Fecha a janela do quick-menu sem salvar edições pendentes."
  (interactive)
  (let* ((buf (get-buffer my-harpoon-buffer-name))
         (win (and buf (get-buffer-window buf))))
    (when win (delete-window win))))

(defun my-harpoon-menu-visit ()
  "Abre o arquivo da linha atual e fecha o menu."
  (interactive)
  (let ((file (my-harpoon--line-file)))
    (when file
      (my-harpoon-menu-quit)
      (find-file file))))

(defun my-harpoon-menu-delete-line ()
  "Remove a linha (arquivo) sob o ponto, sem fechar o menu."
  (interactive)
  (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position)))))

(defun my-harpoon-menu-save ()
  "Lê o buffer do menu (respeitando reordenação/edições manuais),
persiste a nova lista do projeto atual no disco e fecha o menu."
  (interactive)
  (let (files)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((f (my-harpoon--line-file)))
          (when f (push f files)))
        (forward-line 1)))
    (my-harpoon-set-current-files (nreverse files)))
  (my-harpoon-menu-quit)
  (message "Harpoon: lista salva (%d arquivo(s))" (length (my-harpoon-current-files))))

;;; === APPEARANCE ===

(custom-set-faces
 '(default ((t (:background "#1E1E1E" :foreground "#D4D4D4"))))
 '(cursor ((t (:background "#AEAFAD"))))
 '(font-lock-comment-face ((t (:foreground "#6A9955"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#6A9955"))))
 '(font-lock-string-face ((t (:foreground "#CE9178"))))
 '(font-lock-doc-face ((t (:foreground "#6A9955"))))
 '(font-lock-number-face ((t (:foreground "#B5CEA8"))))
 '(font-lock-keyword-face ((t (:foreground "#569CD6"))))
 '(font-lock-function-name-face ((t (:foreground "#DCDCAA"))))
 '(font-lock-type-face ((t (:foreground "#4EC9B0"))))
 '(font-lock-constant-face ((t (:foreground "#4FC1FF"))))
 '(font-lock-builtin-face ((t (:foreground "#DCDCAA"))))
 '(font-lock-variable-name-face ((t (:foreground "#9CDCFE"))))
 '(font-lock-preprocessor-face ((t (:foreground "#C586C0"))))
 '(font-lock-warning-face ((t (:foreground "#F44747" :background "#2D2D2D"))))
 '(hl-line ((t (:background "#2D2D2D"))))
 '(region ((t (:background "#264F78"))))
 '(highlight ((t (:background "#3A3D41"))))
 '(fringe ((t (:background "#1E1E1E" :foreground "#3A3D41"))))
 '(vertical-border ((t (:foreground "#3A3D41"))))
 '(mode-line ((t (:background "#2D2D2D" :foreground "#CCCCCC" :box (:line-width 1 :color "#3A3D41")))))
 '(mode-line-inactive ((t (:background "#1E1E1E" :foreground "#6A6A6A" :box (:line-width 1 :color "#3A3D41")))))
 '(isearch ((t (:background "#264F78" :foreground "#FFFFFF"))))
 '(lazy-highlight ((t (:background "#3A3D41" :foreground "#D4D4D4"))))
 '(match ((t (:background "#264F78" :foreground "#FFFFFF"))))
 '(minibuffer-prompt ((t (:foreground "#569CD6")))))

;; Line numbers
(when (fboundp 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'text-mode-hook 'display-line-numbers-mode))

;; Font — mesma família nas duas plataformas; ajuste o nome caso a fonte
;; não esteja instalada no Linux (ex.: "JetBrainsMono Nerd Font-12").
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-12"))

;;; === EDITING ===

(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default c-basic-offset 4)
(setq-default python-indent-offset 4)
(setq-default js-indent-level 4)
(add-hook 'python-mode-hook (lambda () (setq-local tab-width 4)))
(add-hook 'python-ts-mode-hook (lambda () (setq-local tab-width 4)))
(electric-indent-mode -1)

;;; === TAB "burro" estilo Vim no Insert ===
(defun my-insert-tab ()
  "Insere espaços fixos (tab-width) sem recalcular indentação sintática."
  (interactive)
  (insert (make-string tab-width ?\s)))

(evil-define-key 'insert 'global (kbd "TAB") 'my-insert-tab)

;;; === KEYBINDS ===

(global-set-key (kbd "C-c g") 'my-grep-rn)
(global-set-key (kbd "C-c s") 'my-git-status)
(global-set-key (kbd "C-c l") 'my-git-log)

;;; === FALLBACK KEYS ===
(defun display-warning (&rest _args) nil)
(setq warning-minimum-level :emergency)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-x C-s") 'save-buffer)
(global-set-key (kbd "C-x C-f") 'find-file)  ;; Completion nativa
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-c x") 'execute-extended-command)
(global-set-key (kbd "C-x d") 'dired)
(global-set-key (kbd "C-x C-d") 'dired-other-window)
(global-set-key (kbd "C-x C-c") (lambda () (interactive) (save-buffer) (kill-buffer)))

;;; === LEADER KEY (SPC) ===

(defvar my-leader-map (make-sparse-keymap)
  "Keymap para o leader (SPC), estilo which-key do nvim.")

(evil-define-key '(normal motion) 'global (kbd "SPC") my-leader-map)

;; --- Arquivo / buffer ---
(define-key my-leader-map (kbd "w w") 'save-buffer)
(define-key my-leader-map (kbd "w q") 'evil-quit)
(define-key my-leader-map (kbd "e")   'dired-jump)
(define-key my-leader-map (kbd "n")   (lambda () (interactive) (switch-to-buffer (generate-new-buffer "*new*"))))
(define-key my-leader-map (kbd "q")   'kill-current-buffer)
(define-key my-leader-map (kbd "f")   'find-file)   ;; <leader>f -> C-x C-f
(define-key my-leader-map (kbd "g")   'my-grep-rn)  ;; <leader>g -> C-c g

;; --- Splits ---
(define-key my-leader-map (kbd "w v") 'split-window-right)
(define-key my-leader-map (kbd "w s") 'split-window-below)

;; --- Navegação entre janelas ---
(define-key my-leader-map (kbd "w h") 'evil-window-left)
(define-key my-leader-map (kbd "w j") 'evil-window-down)
(define-key my-leader-map (kbd "w k") 'evil-window-up)
(define-key my-leader-map (kbd "w l") 'evil-window-right)

;; --- Harpoon (equivalente ao harpoon.nvim) ---
(define-key my-leader-map (kbd "a") 'my-harpoon-add-file)
(define-key my-leader-map (kbd "z") 'my-harpoon-toggle-menu)
(define-key my-leader-map (kbd "1") (lambda () (interactive) (my-harpoon-nav-file 1)))
(define-key my-leader-map (kbd "2") (lambda () (interactive) (my-harpoon-nav-file 2)))
(define-key my-leader-map (kbd "3") (lambda () (interactive) (my-harpoon-nav-file 3)))
(define-key my-leader-map (kbd "4") (lambda () (interactive) (my-harpoon-nav-file 4)))

;; --- Terminal embutido (leader t) ---
;; Windows: força bash "burro" (TERM=dumb) via Git Bash.
;; Linux: usa o shell padrão do usuário normalmente.
(setq explicit-bash-args '("--login" "-i"))

(define-key my-leader-map (kbd "t")
  (lambda ()
    (interactive)
    (let ((process-environment (if my-on-windows
                                    (cons "TERM=dumb" process-environment)
                                  process-environment)))
      (split-window-below -12)
      (other-window 1)
      (shell (generate-new-buffer-name "*bash*")))))

;;; === CLIPBOARD (sistema <-> Emacs) ===
(setq select-enable-clipboard t)   ;; sincroniza kill-ring com o clipboard do SO
(setq select-enable-primary t)     ;; suporte à seleção primária (X11/"* register)

;;; === OPTIMIZATIONS ===

(defun my-minibuffer-setup ()
  (setq gc-cons-threshold 800000000))

(defun my-minibuffer-exit ()
  (setq gc-cons-threshold 400000000))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(add-hook 'minibuffer-exit-hook 'my-minibuffer-exit)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))

(defun my-always-yes (&rest args) t)
(advice-add 'yes-or-no-p :override 'my-always-yes)
(setq confirm-nonexistent-file-or-buffer nil)

;;; === STARTUP ===

(defun my-show-startup-time ()
  (let ((elapsed (float-time (time-subtract (current-time) my-emacs-start-time))))
    (message "Emacs loaded in %.2f seconds" elapsed)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 80000000)
            (setq auto-save-default t)
            (my-show-startup-time)))

(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(message "Starting Emacs...")
