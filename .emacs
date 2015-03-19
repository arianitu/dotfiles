; --------------------------- Global Section ---------------------------
(package-initialize)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(menu-bar-mode -1)
(tool-bar-mode -1)

;; Linum mode
(global-linum-mode t)

;; Tramp default ssh method
(require 'tramp)
(setq tramp-default-method "sshx")

; undo-tree
(add-hook 'after-init-hook (lambda() (global-undo-tree-mode 1)))

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
(global-set-key (kbd "M-m") 'projectile-commander)

; --------------------------- Keybindings Section   ---------------------------
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(define-key global-map (kbd "C-t") 'ace-jump-mode)
(define-key global-map (kbd "C-c j") 'ace-jump-line-mode)

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
(global-set-key (kbd "s-i") 'magit-status)
(global-set-key (kbd "C-%") 'replace-string)

(fset 'nodejs-global-require
      [?\C-  ?\M-b ?\M-w ?v ?a ?r ?  ?\M-f ?  ?= ?  ?r ?e ?q ?u ?i ?r ?e ?\( ?\' ?\C-y ?\' ?\) ?\; tab])

(fset 'nodejs-local-require
      [?\M-b ?\C-  ?\M-f ?\M-w ?\M-b ?v ?a ?r ?  ?\M-f ?  ?= ?  ?r ?e ?q ?u ?i ?r ?e ?\( ?\' ?. ?/ ?\C-y ?\' ?\) ?\; tab])

(fset 'freyja-gen-validation
   [?\C-x ?r ?m ?f ?r ?e ?y ?j ?a ?- ?d ?o ?c ?g ?e ?n ?1 ?0 ?1 ?7 ?6 ?4 return ?\C-e ?\C-  ?\M-b ?\M-w ?\C-r ?\{ ?\C-f ?\C-  ?\M-f ?\M-w ?\C-s ?\) ?  ?\{ ?\C-m return return tab ?i ?f ?  ?\( ?! ?\S-  ?V ?a ?l ?i ?d ?a ?t ?i ?o ?n ?. ?i ?s ?\C-y ?\( ?\C-y ?\M-y ?\) ?\) ?  ?\{ return tab return ?\} tab ?\C-x ?r ?b ?f ?r ?e ?y ?j ?a ?- ?d ?o ?c ?g ?e ?n ?1 ?0 ?1 ?7 ?6 ?4 return ?\M-x ?b ?o ?o ?k ?m ?a ?r ?k ?- ?d ?e ?l ?e ?t ?e return ?f ?r ?e ?y ?j ?a ?- ?d ?o ?c ?g ?e ?n ?1 ?0 ?1 ?7 ?6 ?4 return ?\C-p])

(global-set-key (kbd "s-v") 'freyja-gen-validation)
(global-set-key (kbd "s-R") 'nodejs-global-require)
(global-set-key (kbd "s-r") 'nodejs-local-require)

(add-hook 'after-init-hook (lambda()
                             (require 'multiple-cursors)
                             (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                             (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                             (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                             (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
                             (projectile-global-mode)                            
                             ))

; --------------------------- Global Module Section ---------------------------

(require 'auto-complete)
(require 'go-autocomplete)
(define-key ac-mode-map (kbd "<s-tab>") 'auto-complete)

(require 'auto-complete-config)
(ac-config-default)

(require 'yasnippet)
 (yas/global-mode 1)

(defun ac-yasnippet-candidate ()
  (let ((table (yas/get-snippet-tables major-mode)))
    (if table
      (let (candidates (list))
            (mapcar (lambda (mode)
              (maphash (lambda (key value)
                (push key candidates))
              (yas/snippet-table-hash mode)))
            table)
        (all-completions ac-prefix candidates)))))

(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate.")

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground xs"white")))
  "Face for the yasnippet selected candidate.")

(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (limit . 3)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face))
  "Source for Yasnippet.")

(provide 'auto-complete-yasnippet)

;; auto pair
;; https://github.com/capitaomorte/autopair
(add-to-list 'load-path
              "~/.emacs.d/plugins/autopair")

(require 'autopair)

(add-to-list 'load-path
             "~/.emacs.d/plugins/goflymake")
(require 'go-flycheck)


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

; ido fuzzy limit
(defvar af-ido-flex-fuzzy-limit (* 2000 5))
(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
  (let ((ido-enable-flex-matching (< (* (length (ad-get-arg 0)) (length ido-text))
                                     af-ido-flex-fuzzy-limit)))
    ad-do-it))

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

; ido-mode!
(ido-mode 1)
(setq ido-enable-flex-matching t)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))


; --------------------------- Organization Section ---------------------------
(setq org-log-done 'time)
(setq org-agenda-files (list "~/Organization/General/todo.org"
                              ))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


; --------------------------- TypeScript Section ---------------------------

(add-to-list 'load-path
             "~/.emacs.d/plugins/typescript")

(require 'typescript)

; --------------------------- JavaScript Section ---------------------------

;; https://github.com/mooz/js2-mode
(add-to-list 'load-path
              "~/.emacs.d/plugins/js2")

(autoload 'js2-mode "js2-mode" nil t)

(eval-after-load 'js2-mode
   '(progn
      (require 'js2-imenu-extras)
      (js2-imenu-extras-setup)))

;; js-comint
;; http://js-comint-el.sourceforge.net/
(add-to-list 'load-path
              "~/.emacs.d/plugins/js-comint")

(require 'js-comint)
(setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
(add-hook 'js2-mode-hook '(lambda ()
                (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                (local-set-key "\C-cb" 'js-send-buffer)
                (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                (local-set-key "\C-cl" 'js-load-file-and-go)
                (setq indent-tabs-mode nil)
                ; (whitespace-mode t)

                ;; smart tabs
                (smart-tabs-mode t)
                (smart-tabs-advice js2-indent-line js2-basic-offset)
                (font-lock-add-keywords nil 
                                        '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 
                                           1 font-lock-warning-face prepend)))
                (c-set-offset 'case-label '+)
                
                ))

; --------------------------- Golang Section ---------------------------        

(put 'dired-find-alternate-file 'disabled nil)

; --------------------------- Java Section ---------------------------
(require 'eclim)
(global-eclim-mode)

;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)

; --------------------------- C/C++ Section ---------------------------

(setq c-default-style "linux"
      c-basic-offset 4)
; --------------------------- Scala Section ---------------------------
; scala-mode was installed via package-list

;; load the ensime lisp code...
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


; --------------------------- Python Section ---------------------------
(setq default-tab-width 4)

; --------------------------- HTML Section ---------------------------

; --------------------------- END OF CUSTOMIZATIONS ---------------------------


(add-hook 'after-init-hook (lambda()
                             (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))))
(setq debug-on-error nil)

(add-hook 'after-init-hook (lambda()
                             (global-set-key "\M-n"  (lambda () (interactive) (scroll-up   2)) )
                             (global-set-key "\M-p"  (lambda () (interactive) (scroll-down 2)) )
                             ))

(put 'upcase-region 'disabled nil)


(setq server-use-tcp t)

(server-start)
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules" "lib-cov" "native" ))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "#*" "*.class"))))
(put 'downcase-region 'disabled nil)
(setq redisplay-dont-pause t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "936e5cac238333f251a8d76a2ed96c8191b1e755782c99ea1d7b8c215e66d11e" default)))
 '(eclim-auto-save nil)
 '(eclim-eclipse-dirs (quote ("/usr/local/eclipse")))
 '(eclim-executable "/usr/local/eclipse/eclim")
 '(fci-rule-color "#383838")
 '(fringe-mode 6 nil (fringe))
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(quote (main-line-color2 "#111111"))
 '(scss-compile-at-save nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Source Code Pro"))))
 '(isearch ((t (:background "#503450")))))

(setq system-uses-terminfo nil)


(defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0123456789")
      (or (looking-at "[0123456789]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)

