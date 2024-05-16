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
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

;; 16/Feb/2022 after installing gcc-emacs
(setq package-native-compile t)
(setq comp-deferred-compilation t)


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
   '("1d89fcf0105dd8778e007239c481643cc5a695f2a029c9f30bd62c9d5df6418d" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "1bdc49116a77e52aaea740cd8e54b93e0bae6c0895dcc36d5c8d1a493e89c78d" "8c01cb4cf9ee298d30a0456b1e90c575d8f5a047e35a5380a5f955c59ed17d2f" "f95e22498a1a22af94542c796f0c330312181cacdcd403ab32e9fe074051c5da" "5912c255e7e46432d6c1c057a2124cce807ad4b901a99bc43e838db0754dff91" "1544daa051637c2f2de972757e5387d20f3d51c3bec375af9b2b43a0899d436f" "0b0d189e2393d17e30d5101ba53f6798712a415b26de4f164b3fc878f54a5521" "47e6f8c23eaea064b89ed1361b5824ee4f9562a8c4a30774ee9ee69f9b9d4f69" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "e7ba99d0f4c93b9c5ca0a3f795c155fa29361927cadb99cfce301caf96055dfd" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "ae88c445c558b7632fc2d72b7d4b8dfb9427ac06aa82faab8d760fff8b8f243c" "ba72dfc6bb260a9d8609136b9166e04ad0292b9760a3e2431cf0cd0679f83c3a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "41098e2f8fa67dc51bbe89cce4fb7109f53a164e3a92356964c72f76d068587e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "da53c5d117ebada2664048682a345935caf8e54094a58febd5f021462ef20ba2" "c2e1201bb538b68c0c1fdcf31771de3360263bd0e497d9ca8b7a32d5019f2fae" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "2f26d251e2b0d11e0a5f16b21785ab42192374259cfe41eed67262869c1b387f" default))
 '(flycheck-haskell-ghc-executable "stack ghc --")
 '(flycheck-haskell-stack-ghc-executable "stack ghc")
 '(global-prettify-symbols-mode t)
 '(haskell-process-args-stack-ghci
   '("--ghci-options=-ferror-spans -dynamic" "--no-build" "--no-load"))
 '(iswitchb-mode 'on)
 '(org-agenda-files
   '("/home/myoungjin/.emacs.d/myoungjin-init.org" "/home/myoungjin/gtd/inbox.org" "/home/myoungjin/gtd/gtd.org" "/home/myoungjin/gtd/tickler.org" "/home/myoungjin/brainstorming/Future.org" "/home/myoungjin/brainstorming/WhatYourCoffee.org" "/home/myoungjin/brainstorming/test.org" "/home/myoungjin/brainstorming/test2.org" "/home/myoungjin/brainstorming/org-roam/20240122154803-clojure_intro.org" "/home/myoungjin/brainstorming/org-roam/20240122155103-study_log.org" "/home/myoungjin/brainstorming/org-roam/20240122185422-조직구조.org" "/home/myoungjin/brainstorming/org-roam/20240122185941-경영및리더십.org" "/home/myoungjin/brainstorming/org-roam/20240122190326-생산.org" "/home/myoungjin/brainstorming/org-roam/20240122190642-운영.org" "/home/myoungjin/brainstorming/org-roam/20240122190912-소비.org" "/home/myoungjin/brainstorming/org-roam/20240122191617-화폐의가치.org" "/home/myoungjin/brainstorming/org-roam/20240122191941-부의분배.org" "/home/myoungjin/brainstorming/org-roam/20240122192208-온라인매매.org" "/home/myoungjin/brainstorming/org-roam/20240122192346-디지털마케팅.org" "/home/myoungjin/brainstorming/org-roam/20240122192402-sns.org" "/home/myoungjin/brainstorming/org-roam/20240122192516-판매.org" "/home/myoungjin/brainstorming/org-roam/20240122192622-전자상거래.org" "/home/myoungjin/brainstorming/org-roam/20240122192826-결재.org" "/home/myoungjin/brainstorming/org-roam/20240122192903-블럭체인.org" "/home/myoungjin/brainstorming/org-roam/20240122193529-분산_원장_기술.org" "/home/myoungjin/brainstorming/org-roam/20240122194403-분산_금융_defi.org" "/home/myoungjin/brainstorming/org-roam/20240310110021-openapi.org" "/home/myoungjin/brainstorming/org-roam/20240310115400-clojurescript.org" "/home/myoungjin/brainstorming/org-roam/20240310134646-web_frame_work의_주안점.org" "/home/myoungjin/brainstorming/org-roam/20240310140141-clojurescript_reagent.org" "/home/myoungjin/brainstorming/org-roam/20240310142809-shadow_cljs.org" "/home/myoungjin/brainstorming/org-roam/20240311080240-css_button_stories.org" "/home/myoungjin/brainstorming/org-roam/20240311080800-login_with_google_oauth2.org" "/home/myoungjin/brainstorming/org-roam/20240311094126-font.org"))
 '(org-refile-targets
   '(("~/gtd/gtd.org" :maxlevel . 3)
     ("~/gtd/someday.org" :level . 1)
     ("~/gtd/tickler.org" :maxlevel . 2)))
 '(package-selected-packages
   '(beacon which-key avy tabbar w3m raku-mode fold-dwim-org fold-dwim gruvbox-theme auctex fish-mode counsel ivy auto-complete magit use-package nov flycheck-perl6 cl-lib-highlight cl-generic cl-format airline-themes))
 '(powerline-default-separator 'utf-8)
 '(powerline-gui-use-vcs-glyph t)
 '(powerline-height 16)
 '(powerline-utf-8-separator-left 57542)
 '(powerline-utf-8-separator-right 57543)
 '(safe-local-variable-values
   '((hamlet/basic-offset . 4)
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (indent-level . 8)
     (cperl-indent-parens-as-block . t)
     (cperl-close-paren-offset . -4)))
 '(spaceline-all-the-icons-clock-always-visible nil)
 '(spaceline-all-the-icons-highlight-file-name t)
 '(spaceline-all-the-icons-icon-set-eyebrowse-slot 'square)
 '(spaceline-all-the-icons-icon-set-git-ahead 'commit)
 '(spaceline-all-the-icons-icon-set-modified 'chain)
 '(spaceline-all-the-icons-separator-type 'arrow)
 '(spaceline-all-the-icons-separators-invert-direction nil)
 '(spaceline-all-the-icons-slim-render t)
 '(spaceline-all-the-icons-window-number-always-visible t)
 '(spaceline-show-default-input-method t)
 '(work-mode-modeline-preference 'spaceline)
 '(work-mode-modeline-preference1 'powerline+airline))

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
 '(org-document-title ((t (:inherit default :weight bold :foreground "#333333" :font "FantasqueSansMono Nerd Font Mono" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#333333" :font "FantasqueSansMono Nerd Font Mono" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#333333" :font "FantasqueSansMono Nerd Font Mono" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#333333" :font "FantasqueSansMono Nerd Font Mono" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#333333" :font "FantasqueSansMono Nerd Font Mono" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#333333" :font "FantasqueSansMono Nerd Font Mono"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#333333" :font "FantasqueSansMono Nerd Font Mono"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#333333" :font "FantasqueSansMono Nerd Font Mono"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#333333" :font "FantasqueSansMono Nerd Font Mono")))))

;;; init.el ends here
