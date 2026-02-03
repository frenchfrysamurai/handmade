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


;; ---------- Desktop + recentf (FORCED to use your config folder) ----------
(require 'desktop)
(require 'seq)
(require 'recentf)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; IMPORTANT: force the session + recentf files to live next to THIS .emacs
(defconst emilio-config-dir "C:/handmade/misc/")

(setq user-emacs-directory emilio-config-dir)

;; Desktop session file location
(setq desktop-dirname emilio-config-dir)
(setq desktop-path (list emilio-config-dir))
(setq desktop-base-file-name "emilio.desktop")

;; Make it restore quickly
(setq desktop-restore-eager 50)
(setq desktop-restore-frames nil)

(desktop-save-mode 1)

;; recentf file location
(setq recentf-save-file (concat emilio-config-dir "recentf"))
(recentf-mode 1)
(setq recentf-max-saved-items 200)

(defun emilio-first-file-buffer ()
  "Return the most recent buffer visiting a file, or nil."
  (seq-find (lambda (b) (buffer-file-name b))
            (buffer-list)))

(defun emilio-open-last-recent-file ()
  "Open the most recent file from recentf, if any."
  (when (and (boundp 'recentf-list) recentf-list)
    (let ((f (car recentf-list)))
      (when (and f (file-exists-p f))
        (find-file f)))))

(defun emilio-apply-layout ()
  "Two windows: left = last file, right = *compilation*."
  (delete-other-windows)
  (split-window-right)
  (let ((left (selected-window))
        (right (next-window)))

    ;; Right side: compilation buffer always exists
    (set-window-buffer right (get-buffer-create "*compilation*"))

    ;; Left side: prefer restored file buffers, otherwise open recent file
    (select-window left)
    (unless (emilio-first-file-buffer)
      (emilio-open-last-recent-file))

    ;; If we now have a file buffer, show it
    (let ((b (emilio-first-file-buffer)))
      (when b
        (set-window-buffer left b)))

    (select-window left)))

(defun emilio-setup-after-startup ()
  "Apply layout after init + desktop restore."
  (run-with-timer 0.3 nil #'emilio-apply-layout))

(add-hook 'window-setup-hook #'emilio-setup-after-startup)


;; M-w = switch focus to the other window (like clicking it)
(global-set-key (kbd "M-w") #'other-window)
