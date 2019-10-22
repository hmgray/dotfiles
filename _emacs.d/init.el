

;; APPEARANCE
(global-visual-line-mode)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode 0)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(transient-mark-mode 1)
(set-face-attribute 'default nil :height 140)
(setq visible-bell nil)
(blink-cursor-mode 0)
(global-font-lock-mode 1)
(show-paren-mode -1)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq system-uses-terminfo nil)


;; EXTERNAL PACKAGES AND VARIABLES
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq custom-file "~/.emacs.d/custom-set-variables.el")
(load custom-file)
(load-theme 'zenburn)


;; SAVING AND BACKUPS
(setq backup-directory-alist '(("." . "~/.bak")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; ALIASES
(defalias 'lk (lambda () (interactive) (shell-command "physlock")))
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'tp 'transpose-paragraphs)
(defalias 'ts 'transpose-sentences)
(defalias 'wc 'count-words)


;; FUNCTIONS
(defun scratch-buffer nil
  "Successively switch to or recreate the *scratch* buffer, maximize it, and set it to lisp-mode."
  (interactive)
  (if (string-equal (buffer-name) "*scratch*")
      (if (equal 1 (count-windows))
          (lisp-interaction-mode)
        (delete-other-windows))
    (progn (get-buffer-create "*scratch*")
           (switch-to-buffer "*scratch*"))))

(defun unfill-paragraph ()
  "Turn a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun init-file-please ()
  "Jump to Emacs config file."
  (interactive)
  (eval-expression
   (find-file user-init-file)))

(defun sink-region (start end)
  "Comment the current region and move it to the bottom of the buffer."
  (interactive "r")
  (comment-region start end)
  (kill-region start end)
  (bookmark-set "sink-region")
  (widen)
  (end-of-buffer)
  (newline)
  (yank)
  (bookmark-jump "sink-region")
  (pop-mark)
  (recenter)
  (message "Sank region."))

(defun w13 ()
  "Look up a word in Webster's 1913 dictionary."
  (interactive)
  (shell-command (concat "w13 " (read-string "Webster's 1913: ")) "*Webster's 1913*")
  (with-current-buffer "*Webster's 1913*" (view-mode)))

(defun wrt-directory ()
  "Show my writing project directory, please!"
  (interactive)
  (dired "~/wrt"))

(defun wrt-search (&optional arg)
  "Search my agenda files for matching REGEX."
  (interactive "P")
  (org-agenda arg "s"))


;; ESHELL
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)


;; FLYSPELL
(global-set-key (kbd "C-$") 'flyspell-mode)
(setq ispell-program-name "/usr/bin/aspell")
(setq ispell-list-command "--list")
(setq ispell-extra-args '("--dont-tex-check-comments"))


;; MIPS
(require 'mips-mode)


;; KEYBINDINGS
(defvar my/bindings-map (make-keymap) "My bindings' map.")

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(define-key my/bindings-map (kbd "C-!") 'eshell)
(define-key my/bindings-map (kbd "C-(") 'kmacro-start-macro-or-insert-counter)
(define-key my/bindings-map (kbd "C-)") 'kmacro-end-or-call-macro)
(define-key my/bindings-map (kbd "C--") 'text-scale-decrease)
(define-key my/bindings-map (kbd "C-<tab>") 'other-window)
(define-key my/bindings-map (kbd "C-=") 'text-scale-increase)
(define-key my/bindings-map (kbd "M-=") 'count-words)
(define-key my/bindings-map (kbd "M-T") 'transpose-paragraphs)
(define-key my/bindings-map (kbd "S-<down>") 'windmove-down)
(define-key my/bindings-map (kbd "S-<left>") 'windmove-left)
(define-key my/bindings-map (kbd "S-<right>") 'windmove-right)
(define-key my/bindings-map (kbd "S-<up>") 'windmove-up)

(global-set-key (kbd "C-S-o") 'insert-line-before)

(define-key my/bindings-map [f4] 'org-agenda-list)
(global-set-key [f5] 'narrow-to-region)
(define-key my/bindings-map [f6] 'widen)
(define-key my/bindings-map [f7] '(lambda ()
                                    (interactive)
                                    (find-file
                                     org-default-notes-file)))
(define-key my/bindings-map [f7] 'wrt-search)
(define-key my/bindings-map [f8] 'scratch-buffer)
(define-key my/bindings-map [f9] 'wrt-directory)
(define-key my/bindings-map [f12] 'init-file-please)

(define-minor-mode my/bindings
  "Preserve my key bindings across modes."
  t " BOUND" 'my/bindings-map)

(my/bindings 1)


;; ORG-MODE
(require 'org)

(setq org-directory "~/wrt")
(setq org-agenda-files (list org-directory))
(setq org-lowest-priority ?C)
(setq org-default-priority ?B)

(setq org-stuck-projects
      '("TODO={.+}/!-DONE|-CNCL" nil nil "SCHEDULED:\\|DEADLINE:"))

(setq org-fontify-whole-heading-line t)
(setq org-cycle-separator-lines 0)

(setq org-list-allow-alphabetical t)

(font-lock-add-keywords 'org-mode '(("\\[[0-9]+]" .
                                     font-lock-type-face)))
(font-lock-add-keywords 'org-mode '(("\\s-*[a-zA-Z]+[0-9]+[a-z]" .
                                     font-lock-constant-face)))
(font-lock-add-keywords 'org-mode '(("\\(qq\\)" .
                                     font-lock-preprocessor-face)))
(setq org-pretty-entities t)
(add-to-list 'org-entities-user
             '("from" "\\leftarrow" t "&larr;" "<-" "<-" "←"))
(add-to-list 'org-entities-user
             '("vs" "\\v{s}" t "š" "s" "š" "š"))
(add-to-list 'org-entities-user
             '("vc" "\\v{c}" t "č" "c" "č" "č"))
(add-to-list 'org-entities-user
             '("vj" "\\v{j}" t "ǰ" "j" "ǰ" "ǰ"))
(add-to-list 'org-entities-user
             '("ng" "\\ng" t "ŋ" "ng" "ŋ" "ŋ"))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c t") 'org-todo)
(add-hook 'org-load-hook
          (define-key org-mode-map [f5] 'org-narrow-to-subtree)
	  (define-key org-mode-map (kbd "C-M-i") 'org-global-cycle))

(setq org-todo-keywords
      '((type "TODO(t)" "WAIT(w)" "|" "DONE(d!)" "CNCL(c@)")))
