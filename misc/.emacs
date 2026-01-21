;; load package manager, add the Melpa package registry
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; load evil
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
  (evil-mode)

  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))
  
; Stop Emacs from losing undo information by
; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(global-hl-line-mode 1)
(set-face-background 'hl-line "deeper-blue")

; Turn off the toolbar
(tool-bar-mode 0)

(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

; Setup my find-files
(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)

(global-set-key (read-kbd-macro "\eb")  'ido-switch-buffer)
(global-set-key (read-kbd-macro "\eB")  'ido-switch-buffer-other-window)

(add-hook 'after-init-hook (lambda () (load-theme 'deeper-blue)))
(load-theme 'deeper-blue t)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

; Smooth scroll
(setq scroll-step 3)

; Clock
(display-time)

; Startup windowing
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

;; ---------- Keybinds ----------
;; M-f = open file
(global-set-key (kbd "M-f") #'find-file)

;; M-m = compile (uses your project build command)
;; We'll define a smart compile function below
(global-set-key (kbd "M-m") #'emilio-build)

;; M-w = next buffer
(global-set-key (kbd "M-w") #'next-buffer)


;; ---------- Compilation (Casey-like) ----------
(require 'compile)

(defvar emilio-build-script
  (cond ((eq system-type 'windows-nt) "build.bat")
        ((eq system-type 'gnu/linux) "./build.linux")
        ((eq system-type 'darwin) "./build.macosx")
        (t "./build"))
  "Build script to run with M-m.")

(defun emilio-find-project-root (start-dir)
  "Walk upward from START-DIR until emilio-build-script is found."
  (let ((dir (file-name-as-directory (expand-file-name start-dir)))
        (found nil)
        (prev nil))
    (while (and dir (not found) (not (equal dir prev)))
      (if (file-exists-p (expand-file-name emilio-build-script dir))
          (setq found dir)
        (setq prev dir
              dir (file-name-directory (directory-file-name dir)))))
    found))

(defun emilio-build ()
  "Run project build script from the project root."
  (interactive)
  (let* ((root (emilio-find-project-root default-directory))
         (cmd emilio-build-script))
    (unless root
      (error "Couldn't find build script (%s) in any parent directory" emilio-build-script))
    (let ((default-directory root))
      (compile cmd))))


;; ---------- Startup layout: left = last file, right = *compilation* ----------
(require 'desktop)

(setq desktop-path (list user-emacs-directory))
(setq desktop-dirname user-emacs-directory)
(setq desktop-base-file-name "emilio.desktop")

(desktop-save-mode 1)

(defun emilio-startup-layout ()
  "Two windows: left editing, right compilation buffer."
  (delete-other-windows)
  (split-window-right)
  (let ((left (selected-window))
        (right (next-window)))
    ;; Right window shows compilation log by default
    (set-window-buffer right (get-buffer-create "*compilation*"))
    ;; Keep focus on left
    (select-window left)))

(add-hook 'emacs-startup-hook #'emilio-startup-layout)


