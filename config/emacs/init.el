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
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(airline-utf-glyph-branch 57504)
 '(airline-utf-glyph-linenumber 57505)
 '(airline-utf-glyph-readonly 57506)
 '(airline-utf-glyph-separator-left 57520)
 '(airline-utf-glyph-separator-right 57522)
 '(airline-utf-glyph-subseparator-left 57521)
 '(airline-utf-glyph-subseparator-right 57523)
 '(bookmark-bmenu-toggle-filenames nil)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "2f26d251e2b0d11e0a5f16b21785ab42192374259cfe41eed67262869c1b387f" default)))
 '(iswitchb-mode (quote on))
 '(org-agenda-files
   (quote
    ("~/gtd/inbox.org" "~/gtd/gtd.org" "~/gtd/tickler.org")))
 '(org-refile-targets
   (quote
    (("~/gtd/gtd.org" :maxlevel . 3)
     ("~/gtd/someday.org" :level . 1)
     ("~/gtd/tickler.org" :maxlevel . 2))))
 '(package-selected-packages
   (quote
    (beacon which-key avy tabbar w3m raku-mode fold-dwim-org fold-dwim gruvbox-theme auctex fish-mode counsel ivy auto-complete magit use-package nov flycheck-perl6 cl-lib-highlight cl-generic cl-format airline-themes)))
 '(powerline-default-separator (quote utf-8))
 '(powerline-gui-use-vcs-glyph t)
 '(powerline-height 16)
 '(powerline-utf-8-separator-left 57542)
 '(powerline-utf-8-separator-right 57543)
 '(safe-local-variable-values
   (quote
    ((indent-level . 8)
     (cperl-indent-parens-as-block . t)
     (cperl-close-paren-offset . -4))))
 '(spaceline-all-the-icons-clock-always-visible nil)
 '(spaceline-all-the-icons-highlight-file-name t)
 '(spaceline-all-the-icons-icon-set-eyebrowse-slot (quote square))
 '(spaceline-all-the-icons-icon-set-git-ahead (quote commit))
 '(spaceline-all-the-icons-icon-set-modified (quote chain))
 '(spaceline-all-the-icons-separator-type (quote arrow))
 '(spaceline-all-the-icons-separators-invert-direction nil)
 '(spaceline-all-the-icons-slim-render t)
 '(spaceline-all-the-icons-window-number-always-visible t)
 '(spaceline-show-default-input-method t)
 '(work-mode-modeline-preference (quote spaceline))
 '(work-mode-modeline-preference1 (quote powerline+airline)))

 ; useful when you're using symlinked configuration file
(setq vc-follow-symlinks t)
;;; Using Org Mode to organise initializing
(org-babel-load-file (expand-file-name
                      (concat user-emacs-directory
                              (convert-standard-filename "myoungjin-init.org"))))

;;(setq tramp-ssh-controlmaster-options
;;      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
;;(require 'tramp)
; before (helm-mode 2)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(airline-visual-center ((t (:background "#b88853" :foreground "#000000" :weight normal))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#839496" :family "Fira Sans Compressed" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#839496" :family "Fira Sans Compressed" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#839496" :family "Fira Sans Compressed" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#839496" :family "Fira Sans Compressed" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#839496" :family "Fira Sans Compressed" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#839496" :family "Fira Sans Compressed"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#839496" :family "Fira Sans Compressed"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#839496" :family "Fira Sans Compressed"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#839496" :family "Fira Sans Compressed")))))

;;; init.el ends here
