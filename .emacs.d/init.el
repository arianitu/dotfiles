(setq user-full-name "Arianit Uka")
(setq user-mail-address "arianitu@gmail.com")

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   2)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 2)) )

(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(add-to-list 'load-path
	     "~/.emacs.d/use-package")
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(if window-system
	(use-package zenburn-theme
	  :ensure t
	  :init
	  (load-theme 'zenburn t)))
    
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config 
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH"))
  
  )

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  )

(use-package json-mode
  :ensure t
  )

(menu-bar-mode -1)
(tool-bar-mode -1)

(use-package tramp
  :config
  (setq tramp-default-method "sshx")
 )

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (bind-key "C-t" 'ace-jump-mode))

(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-M-i") 'helm-select-action)
  (setq helm-quick-update t
		helm-autoresize-max-height 20
		helm-autoresize-min-height 20
		helm-split-window-in-side-p t
		helm-split-window-default-side 'below
		helm-idle-delay 0.01
		helm-input-idle-delay 0.01)
  )


(use-package projectile
  :ensure t
  :commands projectile-commander
  :init
  (bind-key "M-m" 'projectile-commander)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien)
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on))

(use-package autopair
  :ensure t
  :init
  (autopair-global-mode))

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  (progn (require 'js2-imenu-extras) (js2-imenu-extras-setup))
  (add-hook 'js2-mode-hook '(lambda ()
			      (setq indent-tabs-mode nil)
			      (font-lock-add-keywords nil 
						      '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 
							 1 font-lock-warning-face prepend)))
			      (c-set-offset 'case-label '+)
			      )))

(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode) ("\\.erb\\'" . web-mode)
         ("\\.jsp\\'" . web-mode) ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode) ("\\.html\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode) ("\\.mustache\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  )

(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  (ac-config-default)
  (define-key ac-mode-map (kbd "<s-tab>") 'auto-complete)
  )

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (use-package go-autocomplete
    :ensure t))

(use-package yaml-mode
  :ensure t) 

(use-package php-mode
  :ensure t)

;; Linum mode
(global-linum-mode t)

;; Offset the number by two spaces to work around some weird fringe glitch
(setq linum-format "  %d ")

(defalias 'yes-or-no-p 'y-or-n-p)

;; holy crap, original mark is awesome
;; check this out: http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
   Case is ignored if `case-fold-search' is non-nil in the current buffer.
   Goes backward if ARG is negative; error if CHAR not found.
   Ignores CHAR at point."

  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))


(global-set-key (kbd "s-a") 'align-regexp)
(global-set-key (kbd "s-f") 'find-name-dired)
(global-set-key (kbd "s-g") 'find-grep-dired)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-%") 'replace-string)

; --------------------------- Global Snippets Section ---------------------------

; tramp sudo
(require 'dired)
(defun sudo-edit-current-file ()
  (interactive)
  (let ((my-file-name) ; fill this with the file to open
        (position))    ; if the file is already open save position
    (if (equal major-mode 'dired-mode) ; test if we are in dired-mode 
        (progn
          (setq my-file-name (dired-get-file-for-visit))
          (find-alternate-file (prepare-tramp-sudo-string my-file-name)))
      (setq my-file-name (buffer-file-name); hopefully anything else is an already opened file
            position (point))
      (find-alternate-file (prepare-tramp-sudo-string my-file-name))
      (goto-char position))))


(defun prepare-tramp-sudo-string (tempfile)
  (if (file-remote-p tempfile)
      (let ((vec (tramp-dissect-file-name tempfile)))

        (tramp-make-tramp-file-name
         "sudo"
         (tramp-file-name-user nil)
         (tramp-file-name-host vec)
         (tramp-file-name-localname vec)
         (format "ssh:%s@%s|"
                 (tramp-file-name-user vec)
                 (tramp-file-name-host vec))))
    (concat "/sudo:root@localhost:" tempfile)))

(define-key dired-mode-map [s-return] 'sudo-edit-current-file)

;; revert all buffers opened
;; this is good for when you do a git checkout
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

; http://stackoverflow.com/questions/5194417/how-to-mark-the-text-between-the-parentheses-in-emacs
(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp)

; --------------------------- Global Settings Section ---------------------------

; Change the cursor recstangle, to a bar.
(setq-default cursor-type 'bar)

; Stop the beeping!
(setq visible-bell t)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq c-default-style "linux"
      c-basic-offset 4)

(setq default-tab-width 4)
(setq debug-on-error nil)
(put 'upcase-region 'disabled nil)

(setq server-use-tcp t)
(server-start)

(put 'downcase-region 'disabled nil)
(setq redisplay-dont-pause t)
(put 'dired-find-alternate-file 'disabled nil)
(setq system-uses-terminfo nil)

(defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0123456789")
      (or (looking-at "[0123456789]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
