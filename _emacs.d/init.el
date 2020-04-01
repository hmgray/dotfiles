
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

(setq system-uses-terminfo nil)

;; Saving and Backups
(auto-save-visited-mode 1)
(setq backup-directory-alist '(("." . "~/.bak")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Appearance
(global-visual-line-mode)
;;(setq initial-scratch-message "")
;;(setq inhibit-startup-message t)
;;(blink-cursor-mode 0)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode 0)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 50))
(set-face-attribute 'default nil :height 140)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(column-number-mode 1)
(transient-mark-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(defadvice show-paren-function (after show-matching-paren-offscreen activate))
(load-theme 'leuven)

;; Decrease garbage collection frequency
(setq gc-cons-threshold 20000000)

;; Don't prompt to follow symlinks to files under version control
(setq vc-follow-symlinks t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Package Management ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(require 'package)
(package-initialize)

(defun my/package-install-and-update ()
  "Install packages and update those which are installed."
  (interactive)
  (ignore-errors
    (list-packages)
    (package-refresh-contents)
    (package-menu-mark-upgrades)
    (package-menu-execute 'no-query)
    (quit-window))
  (package-install 'flx-ido)
  (package-install 'ido-completing-read+)
  (package-install 'magit)
  (package-install 'markdown-mode)
  (package-install 'org)
  (package-install 'smex))

;; Open files in dired mode using 'xdg-open'
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "C-S-o")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (shell-command (concat "xdg-open " "'" fn "'")))))))

(setq doc-view-resolution 300)
(setq doc-view-continuous t)
(setq doc-view-cache-directory "~/.doc-view-cache")

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(eval-after-load "eww"
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)
          (define-key eww-mode-map "y" 'eww-copy-page-url)))

(global-set-key (kbd "C-$") 'flyspell-mode)
(setq ispell-program-name "/usr/bin/aspell")
(setq ispell-list-command "--list")
(setq ispell-extra-args '("--dont-tex-check-comments"))

(require 'grep)
;; Use `rg' for grep if it's installed
(if (= (shell-command "which rg") 0)
    (grep-apply-setting 'grep-command "rg ")
  (grep-apply-setting 'grep-command "grep -nHie "))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(autoload 'magit-status "magit")
(setq magit-completing-read-function 'magit-ido-completing-read)

;; Tramp
(setq tramp-default-method "ssh")
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Functions ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/arrayify (start end quote)
  "Turn strings on newlines into a quoted, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

(defun my/decrypt-region (start end)
  "Replace the selected region of ASCII-armoured text with its decrypted contents."
  (interactive "r")
  (shell-command-on-region start end
                           (concat "gpg --quiet -d") t t))

(defun my/scratch-buffer nil
  "Switch to or recreate the *scratch* buffer, maximizing it then
setting it to lisp-mode if it exists."
  (interactive)
  (if (string-equal (buffer-name) "*scratch*")
      (if (equal 1 (count-windows))
          (lisp-interaction-mode)
        (delete-other-windows))
    (progn (get-buffer-create "*scratch*")
           (switch-to-buffer "*scratch*"))))

(defun my/unfill-paragraph ()
  "Turn a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun my/insert-line-before(times)
  "Inserts newline(s) above point."
  (interactive "p") ; Callable from M-x
  (save-excursion   ; Save current position
    (move-beginning-of-line 1)
    (newline times)))

(defun my/isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun my/init-file-please ()
  "Jump to Emacs config file."
  (interactive)
  (eval-expression
   (find-file user-init-file)))

(defun my/sudo-edit (&optional arg)

  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun my/xdg-open-buffer-dir ()
  "Graphically browse the directory associated with the current buffer."
  (interactive)
  (call-process-shell-command "xdg-open . &" nil 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Input ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key isearch-mode-map (kbd "C-o") 'my/isearch-occur)
(global-set-key (kbd "C-S-o") 'my/insert-line-before)
(global-set-key (kbd "C-^") 'mode-line-other-buffer)

(global-set-key [f5] 'narrow-to-region)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Aliases
(defalias 'word-count 'count-words)

(fset 'yes-or-no-p 'y-or-n-p)

(defvar my/persistent-bindings-map (make-keymap) "My persistent keybindings map.")

(define-key my/persistent-bindings-map (kbd "S-<up>") 'windmove-up)
(define-key my/persistent-bindings-map (kbd "S-<down>") 'windmove-down)
(define-key my/persistent-bindings-map (kbd "S-<left>") 'windmove-left)
(define-key my/persistent-bindings-map (kbd "S-<right>") 'windmove-right)

(define-key my/persistent-bindings-map (kbd "C-<tab>") 'other-window)
(define-key my/persistent-bindings-map (kbd "C-=") 'text-scale-increase)
(define-key my/persistent-bindings-map (kbd "C--") 'text-scale-decrease)
(define-key my/persistent-bindings-map (kbd "C-!") 'eshell)

;; F5 is reserved for narrowing functions
(define-key my/persistent-bindings-map [f6] 'widen)
(define-key my/persistent-bindings-map [f8] 'my/scratch-buffer)
(define-key my/persistent-bindings-map [f9] 'kmacro-start-macro-or-insert-counter)
(define-key my/persistent-bindings-map [f10] 'kmacro-end-or-call-macro)
(define-key my/persistent-bindings-map [f12] 'my/init-file-please)

(define-minor-mode my/persistent-bindings
  "Preserve my key bindings across modes."
  t " BOUND" 'my/persistent-bindings-map)

(my/persistent-bindings 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Ido ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(require 'ido-completing-read+)
(ido-ubiquitous-mode t)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-ignore-buffers '("\\` "))

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." " [" "]" " [No match]"
                              " [Matched]" " [Not readable]" " [Too big]"
                              " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Standard M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; Substitute hyphens for spaces
(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Markdown ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files." t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))

;; Highlight the string `qq' in Markdown
(font-lock-add-keywords 'markdown-mode '(("\\(qq\\)" .
                                          font-lock-preprocessor-face)))
;; Highlight `TODO' strings in Markdown
(font-lock-add-keywords 'markdown-mode '(("\\(TODO\\)" .
                                          font-lock-warning-face)))
;; Highlight things enclosed in double brackets
(font-lock-add-keywords 'markdown-mode '(("\\(\\[\\[\\)\\(.+?\\)\\(\\]\\]\\)" .
                                     font-lock-preprocessor-face)))

(setq markdown-asymmetric-header t)

(eval-after-load "markdown-mode"
  '(progn
     (define-key markdown-mode-map (kbd "M-<up>") 'markdown-move-up)
     (define-key markdown-mode-map (kbd "M-<down>") 'markdown-move-down)
     (define-key markdown-mode-map (kbd "M-<left>") 'markdown-promote)
     (define-key markdown-mode-map (kbd "M-<right>") 'markdown-demote)
     (define-key markdown-mode-map (kbd "M-#") 'my/sink-region)
     (define-key markdown-mode-map (kbd "M-S-<up>") (lambda () (interactive)
                                                      (transpose-paragraphs -1)))
     (define-key markdown-mode-map (kbd "M-S-<down>") (lambda () (interactive)
                                                        (transpose-paragraphs 1)))
     (define-key markdown-mode-map [f5] 'markdown-narrow-to-subtree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Org Mode ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

(setq org-lowest-priority ?C)
(setq org-default-priority ?B)

(setq org-stuck-projects
      '("TODO={.+}/!-DONE|-CNCL" nil nil "SCHEDULED:\\|DEADLINE:"))

(setq org-src-fontify-natively t)
(setq org-fontify-whole-heading-line t)
(setq org-pretty-entities t)
(font-lock-add-keywords 'org-mode '(("\\[[0-9]+]" .
                                     font-lock-type-face)))
(font-lock-add-keywords 'org-mode '(("\\s-*[a-zA-Z]+[0-9]+[a-z]" .
                                     font-lock-constant-face)))
(font-lock-add-keywords 'org-mode '(("\\(qq\\)" .
                                     font-lock-preprocessor-face)))

(setq org-list-allow-alphabetical t)

(setq org-cycle-separator-lines 0)

(setq org-footnote-define-inline t)

(setq org-log-into-drawer t)

(setq org-goto-interface 'outline-path-completion)
(setq org-goto-interface 'outline)

(setq org-outline-path-complete-in-steps nil)

(setq org-agenda-restore-windows-after-quit nil)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-span 14)
(setq org-agenda-start-on-weekday 0)
(setq org-agenda-sticky t)
(setq org-agenda-include-diary t)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-exporter-settings
      '((htmlize-output-type 'font)))
(define-key org-mode-map (kbd "C-,") nil)
(with-eval-after-load "org-agenda"
  (define-key org-agenda-mode-map (kbd "-") 'org-agenda-do-date-earlier)
  (define-key org-agenda-mode-map (kbd "=") 'org-agenda-do-date-later)
  (define-key org-agenda-mode-map (kbd "n") 'org-agenda-next-item)
  (define-key org-agenda-mode-map (kbd "p") 'org-agenda-previous-item))
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(setq org-default-notes-file "notes.org")
(setq org-capture-templates '(("c" "Capture" entry
                               (file org-default-notes-file)
                               "* %? "
                               :prepend t)
                              ("t" "Capture Task" entry
                               (file org-default-notes-file)
                               "* TODO %? "
                               :prepent t)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(add-to-list 'org-entities-user
             '("from" "\\leftarrow" t "&larr;" "<-" "<-" "←"))
(add-to-list 'org-entities-user
             '("pos" "\\Diamond" t "&#x25C7;" "<>" "<>" "◇"))
(add-to-list 'org-entities-user
             '("nec" "\\Box" t "&#x25A1;" "[]" "[]" "□"))
(add-to-list 'org-entities-user
             '("bot" "\\bot" t "⊥" "_|_" "_|_" "⊥"))
(add-to-list 'org-entities-user
             '("vs" "\\v{s}" t "š" "s" "š" "š"))
(add-to-list 'org-entities-user
             '("vc" "\\v{c}" t "č" "c" "č" "č"))
(add-to-list 'org-entities-user
             '("vj" "\\v{j}" t "ǰ" "j" "ǰ" "ǰ"))
(add-to-list 'org-entities-user
             '("ng" "\\ng" t "ŋ" "ng" "ŋ" "ŋ"))

(setq org-export-mark-todo-in-toc t)
(setq org-export-with-tags nil)
(setq org-export-dispatch-use-expert-ui t)

(setq org-export-with-toc nil)
(setq org-export-with-drawers nil)
(setq org-export-html-postamble nil)
(setq org-export-latex-hyperref-format "\\ref{%s}")

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c t") 'org-todo)
(add-hook 'org-load-hook
          (define-key org-mode-map [f5] 'org-narrow-to-subtree))

(setq org-todo-keywords
      '((type "TODO(t)" "ACTV(a)" "WAIT(w)" "|" "DONE(d!)" "CNCL(c@)")))
