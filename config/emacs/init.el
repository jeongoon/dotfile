;;; init.el --- emacs setup for myougjin
;;; Author: Myoungjin Jeon <jeongoon@g...>
;;; Commentary:
;;; this is heavily copied from other emacs users's configuration
;;; my own code is indicated by "By Myoungjin Jeon"
;;; and feel free to use.
;;; Code:
(provide 'init)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" .   "http://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; Using Org Mode to organise initializing
(org-babel-load-file (expand-file-name (concat user-emacs-directory
                                               (convert-standard-filename "myinit.org"))))

;; Frame and Buffer Setup (size, theme)
(setq inhibit-startup-message t)
(if (display-graphic-p) ;; or (window-system)
    ; THEN
    (progn
      (set-scroll-bar-mode 'left)
      (set-face-attribute 'default nil :font "Fira Code Retina" :height 100)

      (require 'uim-leim) ;; this is not on melpha
      ;; set default IM
      (setq default-input-method "korean-byeoru-uim")
      (add-hook 'after-change-major-mode-hook
                (lambda () (set-input-method "korean-byeoru-uim")))

      ;; use specific font for Korean charset.
      ;; if you want to use different font size for specific charset,
      ;; add :size POINT-SIZE in the font-spec.
      (set-fontset-font t 'hangul (font-spec :name "Noto Sans CJK KR" :size 12))
      ;; you may want to add different for other charset in this way.

      (setq frame-default-left (- (x-display-pixel-width) 698)) ;; 700 when font size is 14
      (if (< (x-display-pixel-height) 698)
          (setq frame-default-height 30)
        (setq frame-default-height 58))
      (setq default-frame-alist
            '((top . 0) (width 80)
              ))
      (add-to-list 'default-frame-alist (cons 'left frame-default-left))
      (add-to-list 'default-frame-alist (cons 'height frame-default-height))
      (setq initial-frame-alist default-frame-alist)
      )
  ;; ELSE
  ;;; Apply Some theme if on terminal
  (use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox t)))

;;; iBuffer
;; note: if you're using screen and your escape key is "C-[Bb]",
;; you need to type "C-x C-b b"
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org"   (name . "^.*org"))
               ("perl"  (or
                         (mode . raku-mode)
                         (mode . cperl-mode)))
               ("programing" (or
                               (mode . python-mode)
                               (mode . c++-mode)
                               (mode . shellscript-mode)
                               (mode . fish-mode)))
               ("emacs" (or
                         (filename . "/\\bemacs\\b*/")
                         (name . "^\\*.*\\*$"))) ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show if name starts with double asterik "**blah~"
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*\\*")
;; don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Dont ask for firmation to delete marked buffers
(setq ibuffer-expert t)

(setq indo-enable-flex-match t)
(setq ido-everywhere t)
(ido-mode 1)

(defalias 'list-buffers 'ibuffer)
; or change the binding
;(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Usseful Packages
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package tabbar
  :ensure t
  :config (progn
            (tabbar-mode 1)
            (global-set-key (kbd "C-c C-j") 'tabbar-backward)
            (global-set-key (kbd "C-c C-k") 'tabbar-forward)
            (global-set-key (kbd "C-c C-p") 'tabbar-backward-group)
            (global-set-key (kbd "C-c C-n") 'tabbar-forward-group)))

;; not so minibuffer: make it a bit taller
(setq resize-mini-windows nil) ; set nil to keep size after resizing minibuffer
(defun resize-minibuffer-window (&optional greeting-message)
  (interactive) ; needed because we will use inside global-set-key as well
  (let* ((minibuffer-orig-height (window-size (minibuffer-window)))
         (minibuffer-new-height 6)
         (delta (- minibuffer-new-height minibuffer-orig-height))
         )

    (window-resize (minibuffer-window) delta)
    (when greeting-message (message "Have a nice one. ;^]"))))
(add-hook 'window-setup-hook (lambda ()
                               (resize-minibuffer-window t)))
(add-hook 'window-configuration-change-hook (lambda ()
                                              (resize-minibuffer-window)))

(global-set-key (kbd "C-l") (lambda()
                              (interactive) ; without this emacs will complain
                              (redraw-display)
                              (resize-minibuffer-window)))

;; neo-tree
(use-package neotree
  :ensure t
  :config (progn
            (setq neo-smart-open t)
            (setq neo-window-fixed-size nil)
            (global-set-key [f8] 'neotree-toggle)))
;;    (evil-leader/set-key
;;     "tt" 'neotree-toggle
;;           "tp" 'neotree-projectile-action)))

;; nov-mode
;(require 'nov)
;(add-to-list 'auto-mode-alist '("\\.epub\'" . nov-mode))

;;(setq tramp-ssh-controlmaster-options
;;      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
;;(require 'tramp)
; before (helm-mode 2)

(require 'epa-file)
(epa-file-enable)

(use-package raku-mode :ensure t :defer t)
(use-package fish-mode :ensure t :defer t)

(use-package flycheck
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package flycheck-perl6 :ensure t)

(global-set-key (kbd "C-X C-X") 'save-buffers-kill-emacs)

;; powerline
(use-package powerline :ensure t)
(use-package airline-themes
  :ensure t
  :config (load-theme 'airline-gruvbox-dark t))


;; https://cestlaz.github.io/posts/using-emacs-8-autocomplete/
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t) ))

(add-to-list 'ac-modes 'raku-mode)

;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :config
  (progn
    ; I use emacs in termial many times but `C-:' doesn't seem to work
    (global-set-key (kbd "M-:") 'avy-goto-char-timer)
    (setq avy-timeout-seconds 0.35)
    ; "You can actually replace the M-g g binding of goto-line,
    ; since if you enter a digit for avy-goto-line, it will switch to
    ; goto-line with that digit already entered."
    (global-set-key (kbd "M-g g") 'avy-goto-line)))

;; https://cestlaz.github.io/posts/using-emacs-6-swiper/
(use-package counsel :ensure t )

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 7)
    (setq ivy-fixed-height-minibuffer nil)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;;; automatically save last edit place
(require 'saveplace)
(setq save-place-file "~/.config/emacs/places")
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)


;;; Hide Show
;; fold-dwim
(use-package fold-dwim :ensure t
  :config (progn
            (global-set-key (kbd "C-]")     'fold-dwim-toggle)
            (global-set-key (kbd "C-x [")    'fold-dwim-hide-all)
            (global-set-key (kbd "C-x ]")    'fold-dwim-show-all) ))

;;(hideshowvis-symbols)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "2f26d251e2b0d11e0a5f16b21785ab42192374259cfe41eed67262869c1b387f" default)))
 '(file-name-shadow-mode (quote on))
 '(font-lock-mode t t (font-lock))
 '(global-font-lock-mode (quote on))
 '(global-hl-line-mode (quote on))
 '(hi-lock-mode (quote on) t)
 '(highlight-current-line-whole-line nil)
 '(iswitchb-mode (quote on))
 '(nil nil t)
 '(package-selected-packages
   (quote
    (which-key avy tabbar w3m raku-mode fold-dwim-org fold-dwim gruvbox-theme auctex fish-mode counsel ivy auto-complete magit use-package nov flycheck-perl6 cl-lib-highlight cl-generic cl-format airline-themes)))
 '(paren-mode (quote sexp) nil (paren))
 '(query-user-mail-address nil)
 '(safe-local-variable-values
   (quote
    ((indent-level . 8)
     (cperl-indent-parens-as-block . t)
     (cperl-close-paren-offset . -4))))
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(size-dindication-mode t)
 '(transient-mark-mode t))

;;; init.el ends here
