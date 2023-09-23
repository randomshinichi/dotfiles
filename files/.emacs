;; Enable MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; use-package-always-ensure installs a required package if it isn't already installed
;; However we cannot go (use-package use-package)
;; Therefore, if use-package is not yet installed, refresh the package list from MELPA and install it.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'use-package)

;; (setq ergoemacs-theme nil)
;; (setq ergoemacs-keyboard-layout "us")
;; (require 'ergoemacs-mode)
;; (use-package ergoemacs-mode)
;; (ergoemacs-mode 1)

;; yes or no prompt -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; don't show Emacs splash screen anymore
(setq inhibit-splash-screen t)

;; emacs GUI toolbar is useless
(tool-bar-mode -1)

;; turn on line wrap by default
(global-visual-line-mode t)

;; overwrite selected text
(delete-selection-mode t)

;; put backups elsewhere
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; multiple cursors
(use-package multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "<M-S-down>") 'mc/mark-next-like-this)
(global-set-key (kbd "<M-S-up>") 'mc/mark-previous-like-this)

; which-mode
(use-package which-key)
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right-bottom)

;; spacemacs theme
;; from http://pragmaticemacs.com/emacs/get-that-spacemacs-look-without-spacemacs/
;; but without his customizations
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;; really nice modeline from spacemacs
(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))


;; org-mode settings
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "<f6>") 'org-capture)
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO" "WAIT" "|" "DONE" "DELEGATED")))
;; when using org-babel-examplize-region, ALWAYS use #+begin_example instead of : (by default it will only do that if you're examplifying > 10 lines
(setq org-babel-min-lines-for-block-output 0)
(setq org-capture-templates
      '(
	("d" "dance.org" plain (file "~/Documents/org/dance.org") "* %?\n%u\n  %i\n" :empty-lines-before 1)
	("l" "daily_lessons.org" plain (file+datetree "~/Documents/org/daily_lessons.org") "*One Big Thing Of The Day*\n*Today's Thoughts*\n%?" :time-prompt 1)
	("r" "rolodex.org" entry (file "~/Documents/org/rolodex.org") "* %?\n:significance: %^{SIGNIFICANCE}\n:how_we_met: %^{HOW_WE_MET}\n:contact: %^{CONTACT}" :empty-lines-before 1)
	("f" "food.org" plain (file+datetree "~/Documents/org/food.org") "%U %?")
	)
      )

;; HELM not only has fuzzy searches but has helm-M-x which is way better than ido
(use-package helm)
(setq helm-net-prefer-curl t
      helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      helm-move-to-line-cycle-in-source t
      ; workaround
      helm-follow-mode nil)
(helm-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key (kbd "<C-S-up>")
                'move-line-up)

(global-set-key (kbd "<C-S-down>")
                'move-line-down)

;; Enable shift selecting even in .org files
(setq org-support-shift-select t)

;; Alt-Shift-arrow keys
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; Smarter Home (ignores indentation whitespace)
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap Home to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

;; Make C-Tab switch between buffers. org-mode uses Ctrl-Tab too but this doesn't interfere with normal Tab
(global-set-key (kbd "<C-tab>") 'helm-mini)
(define-key org-mode-map (kbd "<C-tab>") 'helm-mini) ; needs to be after require 'org

;; Find auto-wraps if it didn't find anything
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))
(define-key (current-global-map) [remap isearch-repeat] 'isearch-repeat)

;; Auto open files on startup
(find-file "~/Documents/org/daily_lessons.org")
(find-file "~/Documents/org/life.org")
(find-file "~/Documents/org/computers.org")
(find-file "~/Documents/org/work.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (multiple-cursors which-key use-package spacemacs-theme spaceline helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
