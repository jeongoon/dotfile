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

 ; useful when you're using symlinked configuratio file
(setq vc-follow-symlinks t)
;;; Using Org Mode to organise initializing
(org-babel-load-file (expand-file-name (concat user-emacs-directory
                                               (convert-standard-filename "myinit.org"))))

;; not so minibuffer: make it a bit taller

;; nov-mode
;(require 'nov)
;(add-to-list 'auto-mode-alist '("\\.epub\'" . nov-mode))

;;(setq tramp-ssh-controlmaster-options
;;      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
;;(require 'tramp)
; before (helm-mode 2)

(require 'epa-file)
(epa-file-enable)

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
; '(highlight-current-line-whole-line nil)
 '(iswitchb-mode (quote on))
 '(package-selected-packages
   (quote
    (beacon which-key avy tabbar w3m raku-mode fold-dwim-org fold-dwim gruvbox-theme auctex fish-mode counsel ivy auto-complete magit use-package nov flycheck-perl6 cl-lib-highlight cl-generic cl-format airline-themes)))
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
