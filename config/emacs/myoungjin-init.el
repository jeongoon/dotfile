;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; this should be inside init.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'straight)

(defun myjin/hungry-delete-backward ()
  "Work around for hungry-delete-backward"
  (interactive)
  (hungry-delete-backward 1)
  )

(defun myjin/hungry-delete-forward ()
  "Work around for hungry-delete-forward"
  (interactive)
  (hungry-delete-forward 1)
  )

(defun myjin/org-insert-todo-heading ()
    "Work around for org-insert-todo-heading"
  (interactive)
  (org-insert-todo-heading t)) ;; IDK :( what ARG means
(defun myjin/load-file (a-file-name &optional prefix-or-t)
  "Load File A-FILE-NAME.
use `user-emacs-directory' if PREFIX-OR-T is t.
or prepend prefix."
  (interactive)
  (setq a-file-path (myjin/expand-file-name a-file-name prefix-or-t))
  (unless (find-file a-file-path)
    (message "myjin/load-file: %s failed: full-path was: %s"
             a-file-name a-file-path)))

(defun myjin/expand-file-name (file-name
                               &optional prefix)
  "Expand FILE-NAME by default, prepend user-emacs-dir if prefix is 't, prepend `PREFIX' if given."

  (if (eq prefix 't)
      (setq file-name (concat user-emacs-directory
                              (convert-standard-filename file-name)))
    (unless (eq prefix 'nil)
      (setq file-name (expand-file-name file-name prefix))))
  (setq file-name (expand-file-name file-name)))

(defun myjin/revert-buffer-without-confirm ()
  "Reverting buffer without annoying confirmation everytime."
  (interactive)
  (let ((ignore-buffer nil) (no-confirm t))
    (revert-buffer ignore-buffer no-confirm)
    (message "myjin/revert-buffer-without-confirm: Buffer reverted.")))

(defun myjin/create-fontset (fontset-family-ascii fontset-name fontset-size)
  "Create a fontset from fontspec.
use FONTSET-FAMILY-DEFAULT as default family name.
You can refer the set as FONTSET-NAME."
  ;; FIXME: 16/Feb/2022 use-package
  (require 's)
  (if (member fontset-family-ascii (font-family-list))
      (progn
        (when (s-match "[-[:blank:]]+" fontset-name)
          (user-error (concat "%s contains blank or a dash(`-')"
                              "which is not allowed for fontset name")
                      fontset-name))
        (let (fsl fspec-str)
          (setq fsl (split-string (car (x-list-fonts
                                        fontset-family-ascii t nil 1)) "-"))
          (setf (nth 3 fsl) "*" (nth 4 fsl) "*" (nth 5 fsl) "*"
                ;; note: 13th elemets must be "fontset"
                (nth 13 fsl) "fontset" (nth 14 fsl) fontset-name)
          (setq fspec-str (string-join fsl "-"))
          (create-fontset-from-fontset-spec fspec-str)
          (set-fontset-font (concat "fontset" "-" fontset-name) 'ascii
                            (font-spec :family fontset-family-ascii
                                       :size fontset-size :height 1.0))))
    (user-error "%s is not found in (font-family-list)" fontset-family-ascii)))

(use-package diminish :ensure t)
(use-package s :ensure t)

(require 'bind-key)

(column-number-mode 1)
(global-font-lock-mode 1)
(global-hl-line-mode 1)

(defcustom work-mode-modeline-preference 'powerline
  "What is your favourite modeilne ('powerline 'powerline+airline
'doom-modeline 'spaceline 'spaceline+all-the-icons 'spaceline+eyeliner)"

  :type '(choice
          (const :tag "default" powerline) ;; STUDY what is the meaning of :tag??
          (const powerline+airline)
          (const doom-modeline)
          (const spaceline)
          (const spaceline+all-the-icons)
          (const spaceline+eyeliner))
  :group 'work-mode)

(defcustom work-mode-powerline+airline-theme 'airline-gruvbox-dark
  "what is your airline theme to use?"
  :type 'string
  :group 'work-mode)

(defcustom work-mode-modeline-fallback 'doom-modeline
  "What is your fallback when your preffered modeline is not going to work"
  :type 'string
  :group 'work-mode)

(require 'diminish)
  (let (package-string mode-string)
    (dolist (package-symbol '(auto-revert
                              beacon which-key
                              ivy hs-minor whitespace
                              highlight-parentheses
                              undo-tree
                              highlight-indent-guides
                              projectile projectile-rails
                              flycheck))
      (setq package-string (symbol-name package-symbol))
      (setq mode-string (concat package-string "-mode"))

;; XXX: not working
;;      `(eval-after-load ,package-symbol
;;        (lambda () (diminish (quote ,mode-string))) "")
  ;; WORKAROUND
      (add-hook (intern (concat mode-string "-hook"))
                `(lambda () (diminish (intern ,mode-string))))
      ))
;; use alternative names ...

(add-hook 'paredit-mode-hook
          (lambda () (diminish 'paredit-mode
                          (if (display-graphic-p) " ⸨✓" " ⸨PE"))))

(add-hook 'all-the-icons-dired-mode-hook
          (lambda () (diminish 'all-the-icons-dired-mode
                          (if (display-graphic-p) " 📁" " iDired"))))

;; but this one works with `eval-after-load'
;; need to check how use-package deal with this.
;;  (eval-after-load 'highlight-indent-guides
;;    (lambda () (diminish 'highlight-indent-guides-mode (if (display-graphic-p ) " ⛙" "|{"))))
;;
(setq eldoc-minor-mode-string (if (display-graphic-p) " 📚" " Ed"))

(show-paren-mode 1)
(setq show-paren-style 'parenthesis) ; or expression
(setq show-paren-delay 0)

(use-package highlight-parentheses :ensure t)
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode))))

(global-highlight-parentheses-mode)

(use-package paredit :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package tabbar
  :ensure t
  :config (progn
            (tabbar-mode 1)))

(setq-default major-mode 'text-mode)
(setq-default indent-tabs-mode nil) ; I prefer not to use indent-tabs-mode

(transient-mark-mode 1)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))

(use-package all-the-icons :ensure t :straight t)

(defvar myjin/korean-font-family "KoPub Batang"
  "Default Korean font for my setting") ;; or Noto Sans CJK KR"
(setq inhibit-startup-message t)
(if (display-graphic-p) ;; or (window-system)
    ;; THEN
    (progn
      (set-scroll-bar-mode nil) ; I used to use 'left
      (tool-bar-mode -1)
      (add-to-list 'load-path (concat user-emacs-directory
                                      (convert-standard-filename "our-lisp/")))
      (use-package leuven-theme
        :ensure t
        :config
        (load-theme 'leuven t))
      ;; ^ you also need to check '(custom-set-variables section on your init.el
      ;; where '(custom-enabled-themes '(...))

      ;; FantasqueSansMono Nerd Font Mono has better metric matched with
      ;; other unicode fonts than original "Fantasque Sans Mono" does.
      (myjin/create-fontset "FantasqueSansMono Nerd Font"
                            "fantasque_kr" 15)

      ;; https://github.com/domtronn/all-the-icons.el
      ;; and I modifed a little to use dolist function
      (dolist (fmname '("Symbola"
                        "FreeSerif"  ;; GNU Font; has a variety of unicodes
                        "Segoe UI Emoji"
                        ))
        ;; note : not fantasque_kr but fontset-fantasque_kr
        (set-fontset-font "fontset-fantasque_kr" 'unicode
                          (font-spec :family fmname) nil 'append))

      ;; use specific font for Korean charset.
      ;; if you want to use different font size for specific charset,
      ;; add :size POINT-SIZE in the font-spec.

      (set-fontset-font "fontset-fantasque_kr" 'hangul
                        (font-spec :name myjin/korean-font-family))

      ;; HACKING: Still testing on it.
      ;; seems to work for icon-dired-mode (file-icons; I guess there is something more
      ;; hangul(한글) in table look at `testing-hangul-alignment-in-table'
      (setq face-font-rescale-alist `(("Material Icons" . 0.8)
                                      ;; `-> test:
                                      ;; <Pictures>
                                      ("FontAwesome" . 0.8)    ;; ???
                                      ("github-octicons" . 0.8)
                                      ;; `-> test:
                                      ;; <any directory>  .vim something.txt~ README.md
                                      ("all-the-icons" . 0.8)
                                      ;; `-> test:
                                      ;; .bashrc  .gitconfig  perl.pl shell.sh
                                      ("file-icons" . 0.85) ;; these are wide
                                      ;; javascript.js rakudo.pl6
                                      (,myjin/korean-font-family . 1.175)))
                                      ;; ^ matched with leuven-theme
                                      ;; don't know why ascii font is smaller than usual

      ;; FIXME: find better way to find the width of window
      (setq frame-default-left (- (x-display-pixel-width) 100)) ;; 700 when font size is 14
      (if (< (x-display-pixel-height) 698)
          (setq frame-default-height 30)
          (setq frame-default-height 70))
      (setq default-frame-alist
            '((top . 1) (width . 100)
              ))
      (add-to-list 'default-frame-alist (cons 'font "fontset-fantasque_kr"))
      (add-to-list 'default-frame-alist (cons 'left 1000))
      (add-to-list 'default-frame-alist (cons 'height frame-default-height))
      (setq window-system-default-frame-alist default-frame-alist)
      )
  ;; ELSE
  ;;; Apply Some theme if on terminal - if your terminal color scheme is
  ;;; not good for editing under terminal
  (use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox t)))

(defvar work-mode-airline-theme-fallback 'airline-gruvbox-dark)

(let ((setting-modeline? t) (curr-ml work-mode-modeline-preference)
      (fallback-ml work-mode-modeline-fallback) (max-try 10))
  (while (and setting-modeline? (> max-try 0))
    (setq max-try (1- max-try))
    (catch 'modeline-switch
      (cond
       ((eq curr-ml 'powerline)
        (use-package powerline :ensure t :straight t
          :config (powerline-default-theme))
        (setq setting-modeline? nil))

       ((eq curr-ml 'powerline+airline)
        (require 's)
        (use-package airline-themes
          :ensure t
          :config
          (progn
            (let (atheme uts) ;; uts: u ser t heme s ymbol
              (setq uts work-mode-powerline+airline-theme) ;; copy
              (if (s-starts-with? "airline-" (symbol-name uts));; FIXME correct?
                  (setq atheme uts) ;; or
                ((setq atheme work-mode-airline-theme-fallback)
                 (message (concat
                           "[work-mode] please set correct value of %s: "
                           "reverting to %s") uts theme)))
            (load-theme atheme t)
              (setq setting-modeline? nil)))))

       ((eq curr-ml 'doom-modeline)
        (use-package doom-modeline
          :ensure t
          :defer t
          :hook (after-init . doom-modeline-mode))
        (setq setting-modeline? nil))

       ((eq curr-ml 'spaceline)
        (use-package spaceline :ensure t :straight t
          :config (progn (require 'spaceline-config)
                         (spaceline-emacs-theme)))
        (setq setting-modeline? nil))

       ((eq curr-ml 'spaceline+all-the-icons)
        (use-package spaceline-all-the-icons :ensure t :straight t
          :config (progn
                    (require 'spaceline)
                    (spaceline-all-the-icons-theme)))
        (setq setting-modeline? nil))

       ((eq curr-ml 'spaceline+eyeliner)
        (unless (display-graphic-p)
          (message "your preffered modelines 'spaceline is not working on terminal: going back to: %s" fallback-ml)
          (setq curr-ml fallback-ml)
          (throw 'modeline-switch fallback-ml))

          (use-package eyeliner
            :ensure t
            :straight (eyeliner :type git
                                :host github
                                :repo "dustinlacewell/eyeliner")
            :config
            (progn
              ; spaceline + eyeliner will complain without it
              (autoload 'projectile-project-p "projectile")
              (require 'eyeliner)
              (eyeliner/install)))
          (setq setting-modeline? nil))))))

(setq resize-mini-windows nil) ;; set `nil' to keep size after resizing minibuffer
                               ;; and do something else
(defun resize-minibuffer-window (&optional greeting-message)
  (interactive) ; needed because we will use inside global-set-key as well
  (let* ((minibuffer-orig-height (window-size (minibuffer-window)))
         (minibuffer-new-height 7)
         (delta (- minibuffer-new-height minibuffer-orig-height))
         )
    (if (= 0 delta)
        nil
        (progn
          (window-resize (minibuffer-window) delta)
          (when greeting-message (message "Have a nice one. ;^]"))
          )
        )))

  ;; resize minibuffer on terminal and window-system when initializing
(add-hook 'window-state-change-hook (lambda ()
                                      (resize-minibuffer-window t)))

(global-set-key (kbd "C-l") (lambda()
                              (interactive) ; without this emacs will complain
                              (redraw-display)
                              (resize-minibuffer-window)))

;; copyright: https://github.com/zamansky/using-emacs/blob/master/myinit.org
(use-package ace-window :ensure t
  :init
  (progn
    (setq aw-scope 'global) ;; was frame
    (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

(require 'saveplace)
(setq save-place-file "~/.config/emacs/places")
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)

(require 'display-line-numbers)
(setq display-line-numbers t)
(setq display-line-numbers-type t) ; setting display-line-numbers isn't enough

(setq display-line-numbers-current-absoulte t)

(defcustom display-line-numbers-allowed-on-starred-buffers 'nil
  "Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'display-line-numbers)

(defun display-line-numbers--turn-on ()
  "turn on line numbers in `display-line-numbers-allowed-modes' but excluding
certain major modes defined in `display-line-numbers-exempt-modes'"
  (if (or display-line-numbers-allowed-on-starred-buffers
          (not string-match "*" (buffer-name)))
      (display-line-numbers-mode 1)
    (display-line-numbers-mode 0)))

(use-package raku-mode :ensure t :defer t)
(use-package fish-mode :ensure t :defer t)
(use-package flycheck-haskell :ensure t :defer t
  :after haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))
(use-package rust-mode :ensure t)
(use-package haskell-mode :ensure t)
(use-package go-mode :ensure t)
(use-package elm-mode :ensure t :defer t)
;;(use-package hamlet-mode :ensure t :defer t)
(use-package shakespeare-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
;; https://robert.kra.hn/posts/rust-emacs-setup
(use-package rustic :ensure t :defer t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; (use-package mmm-mode
;;   :config
;;   (progn
;;     (use-package hamlet-mode :ensure t)
;;     (setq mmm-global-mode t)
;;     (require 'mmm-auto)
;;     (mmm-add-classes
;;      '((hamlet-quasiquote
;;         :submode hamlet-mode
;;         :delimiter-mode nil
;;         :front "\\[.*hamlet|"
;;         :back "|\\]")))
;;     (mmm-add-mode-ext-class 'haskell-mode nil 'hamlet-quasiquote)))

(use-package flycheck
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook 'flycheck-mode))

;;(use-package flycheck-perl6 :ensure t)

;; https://cestlaz.github.io/posts/using-emacs-8-autocomplete/
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t) ))

(add-to-list 'ac-modes 'raku-mode)

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

(define-prefix-command 'myjin-map)
(global-set-key (kbd "C-c m") 'myjin-map)

(define-key myjin-map "r" 'myjin/revert-buffer-without-confirm)

(let ((shortcuts-data '(("Keymap" "Key" "Binding Type" "Link" "Extra Info" "Note") ("t" "m" "file" "~/proj/.code-memo.org" "nil" "nil for no prefix") ("t" "b" "file" "~/gtd/inbox.org" "nil" "") ("t" "g" "file" "~/gtd/gtd.org" "nil" "") ("t" "i" "file" "myoungjin-init.org" "t" "t for using user-emacs-directory") ("t" "s" "buff" "*scratch*" "nil" "it looks bold when type **scratch**") ("t" "e" "file" "~/gtd/english.org" "nil" "") ("t" "p" "toggle" "paredit-mode" "" "toggle mode") ("g" "C-c d" "func" "paredit-forward-down" "" "") ("g" "C-c s" "func" "paredit-splice-sexp" "" "for terminal compatibility") ("g" "C-c <left>" "func" "paredit-backward-slurp-sexp" "" "..") ("g" "C-c <right>" "func" "paredit-backward-barf-sexp" "" "..") ("g" "C-]" "func" "fold-dwim-toggle" "" "") ("g" "C-x [" "func" "fold-dwim-hide-all" "" "") ("g" "C-x ]" "func" "fold-dwim-show-all" "" "") ("t" "j" "func" "tabbar-backward" "" "") ("t" "k" "func" "tabbar-forward" "" "") ("t" "h" "func" "tabbar-backward-group" "" "") ("t" "l" "func" "tabbar-forward-group" "" "") ("g" "C-c DEL" "func" "myjin/hungry-delete-backward" "" "") ("g" "C-c C-d" "func" "myjin/hungry-delete-forward" "" "") ("g" "C-c a" "func" "org-agenda" "" "") ("g" "C-c c" "func" "org-capture" "" "") ("org-mode" "M-n" "func" "org-next-link" "" "") ("org-mode" "M-p" "func" "org-previous-link" "" "") ("org-mode" "C-c m RET" "func" "myjin/org-insert-todo-heading" "" "WORKAROUND") ("org-mode" "C-c m \\" "func" "org-insert-todo-heading-respect-content" "" "") ("rust-mode" "C-c C-c" "func" "rust-run" "" ""))))
(dolist (r shortcuts-data)
  (let (key-after-map binding-type link extra-info)
    (setq key-map       (nth 0 r)
          key-after-map (nth 1 r)
          binding-type  (nth 2 r)
          link          (nth 3 r)
          extra-info    (nth 4 r))

    (cond ((or (string= key-map "") (string= key-map "g"))
           (setq key-map 'global-map))
          ((string= key-map "t")
           (setq key-map 'myjin-map))
            (t (setq key-map (intern (concat key-map "-map")))))

    (cond ((equal binding-type "file")
           (setq extra-info  (if (string= extra-info "t") t nil))
           (define-key (symbol-value key-map) (kbd key-after-map)
             `(lambda () "open a file"
                (interactive)
                (require 'which-key)
                (myjin/load-file ,link ,extra-info))))

          ((equal binding-type "buff")
           (define-key (symbol-value key-map) (kbd key-after-map)
             `(lambda () "open a link"
                (interactive) (switch-to-buffer ,link))))

          ((equal binding-type "toggle")
           (define-key (symbol-value key-map) (kbd key-after-map)
             `(lambda () "toggle major mode"
                (interactive)
                (let* ((s (intern ,link))
                       (on? (symbol-value s))
                       (inverse-on (if on? -1 1)))
                       (funcall s inverse-on)))))

          ((equal binding-type "func")
           (define-key (symbol-value key-map) (kbd key-after-map)
             `(lambda () "call a function"
                (interactive) (funcall (intern ,link))))))))
)

(setq default-input-method "korean-hangul3")

(use-package projectile
  :ensure t
  :straight t
  :bind (("C-c p f" . projectile-find-file)
         ("C-c p p" . projectile-switch-project)
         ("C-c p t" . projectile-find-test-file))
  :config
  (progn
    (setq projectile-enable-caching t)
    (add-hook 'prog-mode-hook 'projectile-mode)))

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

(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "my-lisp/")))
(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "our-lisp/")))

(require 'common-allow-deny-rule) ; my-lisp
(require 'hungry-delete)
;;(global-hungry-delete-mode)

(defcustom work-mode-allowed-modes '(prog-mode emacs-lisp-mode text-mode conf-mode)
  "Major modes on which to enable the display-line-numbers mode and whitespace mode and so on"
  :group 'work-mode
  :type 'list
  :version "green")

(defcustom work-mode-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable the work-mode"
  :group 'work-mode
  :type 'list
  :version "green")

(defcustom work-mode-allowed-modes-include-derived-mode 't
  "Extends enabling work-mode through all the derived mode from work-mode-allowed mode"
  :group 'work-mode
  :type 'boolean
  :version "green")

(defcustom work-mode-enabled-major-mode
  '(display-line-numbers-mode
    whitespace-mode
    prettify-symbols-mode
    highlight-indent-guides-mode
    paredit-mode)

  "Which minor mode will be allowed when work-mode is activated"
  :group 'work-mode
  :type 'list
  :version "green")

(defun work-mode ()
  "Turn on some usuful minor mode like display-line-numbers and whitespace"
  (let (work-mode-ready? res on-or-off derived-mode-check-function)
    (setq work-mode-ready? nil)
    (setq derived-mode-check-function
          (if work-mode-allowed-modes-include-derived-mode
              (lambda (candi given-mode) ; candi is actually not used here
                (derived-mode-p given-mode))
            nil))

    (setq res (common-allow-deny-rule-apply major-mode
                                            work-mode-allowed-modes
                                            work-mode-exempt-modes
                                            derived-mode-check-function))
    (setq work-mode-ready? (if (equal (car res) 'allowed) t nil))
      ;;(let (status stage)
      ;;  (setq status (car res))
      ;;  (setq stage  (car (cdr res))) ;; not used
      ;;  (setq work-mode-ready? (if (eq status 'allowed) t nil)))

    ;; Do real configuration goes here
    (setq on-or-off (if work-mode-ready? 1 0))
    (dolist (mode-name work-mode-enabled-major-mode nil)
      ;;(message "%s is %s" mode-name on-or-off)
      (funcall mode-name on-or-off))))

  (add-hook 'after-change-major-mode-hook 'work-mode)

(use-package fold-dwim :ensure t)
;;(hideshowvis-symbols)

(if (display-graphic-p)
    (progn
      (add-hook
       'raku-mode-hook
       (lambda()
         (mapc (lambda (pair) (push pair prettify-symbols-alist))
               '(("->"       . 8594)      ; →
                 ("=>"       . 8658)      ; ⇒
                 ("~~"       . #x2248)    ; ≈
                 ("eq"       . #xff1d)    ; ＝
                 ("le"       . #x2266)    ; ≦
                 ("ge"       . #x2267)    ; ≧
                 ("gt"       . #xff1e)    ; ＞
                 ("lt"       . #xff1c)    ; ＜
                 ;;("==="      . #x2a76)    ; ⩶

                 ("return"   . #x2b6e)   ; ⭮
                 ;;("-->"      . #x27fc)   ; ⟼
                 ("returns"  . #x27fc)   ; ⟼

                 ("say"      . #x1f3a4)   ; 🎤
                 ("print"    . #x2399)    ; ⎙

                 ("use"      . #x271b)    ; ✛
                 ("sub"      . #x2658)    ; ♘;;  ("sub" . #x1d4e2) ; 𝓢
                 ("my"       . #x1d707 )  ; 𝜇
                 ("our"      . #x2127 )   ; ℧
                 ("with"     . #x293a)    ; ⤺

                 ("Any"      . #x1f914)   ; 🤔
                 ("Int"      . #x24be)    ; Ⓘ
                 ("List"     . #x24c1)    ; Ⓛ
                 ("Array"    . #x24b6)    ; Ⓐ
                 ("Str"      . #x24c8)    ; Ⓢ
                 ("Bool"     . #x24b7)    ; Ⓑ

                 ("class"    . #x2656)    ; ♖
                 ("self"     . #x1f60a)   ; 😊
                 ("method"   . #x2657)    ; ♗

                 ("for"      . #x2200)    ; ∀
                 ("loop"     . #x267a)    ; ♺
                 ("if"       . #x26b3)    ; ⚳
                 ("elsif"    . #x1fbc4)   ; 🯄
                 ("else"     . #x2325)    ; ⌥
                 ("last"     . #x21f2)    ; ⇲
                 ("next"     . #x21f1)    ; ⇱
                 ("proceed"  . #x2346)    ; ⍆
                 ("when"     . #x2646)    ; ♆
                 ("given"    . #x2bd5)    ; ⯕
                 ("default"  . #x1f3c1)   ; 🏁

                 ("and"      . #x2227 )   ; ∧
                 ("or"       . #x2228 )   ; ∨
                 ("not"      . #x00ac )   ; ¬

                 ("Nil"      .  #x2205)   ; ∅
                 ("True"     . #x1d54B)   ; 𝕋
                 ("False"    . #x1d53d)   ; 𝔽
                 ;;("contains" . #x220b)    ; ∋

                 ("??" . #x2047) ; ⁇
                 ("!!" . #x203c) ; ‼
                 ("||" . #x2016) ; ‖
                 )))
       (add-hook
        'haskell-mode-hook
        (lambda()
          (mapc (lambda (pair) (push pair prettify-symbols-alist))
                '(("->"         . #x2192)    ; →
                  ("<-"         . #x2190)    ; ←
                  ("=>"         . 8658)      ; ⇒
                  ("=="         . #x2261)    ; ≡
                  ("/="         . #x2262)    ; ≢
                  (">"          . #xff1e)    ; ＞
                  (">="         . #x2267)    ; ≧
                  ("<"          . #xff1c)    ; ＜
                  ("<="         . #x2266)    ; ≦
                  (".."         . #x2026)    ; …

                  ("|"          . #x23aa)    ; ⎪
                  ("&&"         . #x2227 )   ; ∧
                  ("||"         . #x2228 )   ; ∨
                  ("not"        . #x00ac )   ; ¬
                  ("!!"         . #x203c )   ; ‼

                  ("()"         . #x2205)    ; ∅
                  ("True"       . #x1d54B)   ; 𝕋
                  ("False"      . #x1d53d)   ; 𝔽
                  ;;("IO"         . #x21f5)    ; ⇵

                  ("."          . #x22c5)    ; ⋅
                  ("*"          . #x00d7)    ; ×
                  ("div"        . #x00f7)    ; ÷
                  ("sqrt"       . #x221a)    ; √
                  ("elem"       . #x2208)    ; ∈
                  ("foldl"      . #x2945)    ; ⥅
                  ("foldr"      . #x2946)    ; ⥆
                  ("import"     . #x27fd)    ; ⟽
                  ("return"     . #x27fc)    ; ⟼

                  ("let"        . #x261f)   ; ☟
                  ("in"         . #x2b78)    ; ⤷

                  ("forall"     . #x2200)    ; ∀
                  ("if"         . #x26b3)    ; ⚳
                  ("then"       . #x2971)    ; ⥱
                  ("else"       . #x2325)    ; ⌥
                  ("do"         . #x2907)    ; ⤇
                  ("where"      . #x261d)    ; ☝
                  )))))))

(use-package highlight-indent-guides
    :ensure t
;    :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
    :init

    (setq highlight-indent-guides-method 'character)
    :config
    (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
    ;; `highlight-indent-guides' breaks in these modes
    (add-hook 'org-indent-mode-hook
      (defun +indent-guides-disable-maybe-h ()
        (when highlight-indent-guides-mode
          (highlight-indent-guides-mode -1)))))

;; note: if you're using screen and your escape key is "C-[Bb]",
;; you need to type "C-x C-b b"
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Hakyll" (filename . ".*/jeongoon\\.github\\.io"))
               ("dired" (mode . dired-mode))
               ("org"   (name . "^.*org"))
               ("haskell" (or
                           (mode . haskell-mode)
                           (mode . haskell-lterate-mode)))
               ("elm"  (mode . elm-mode))
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

(add-to-list 'org-structure-template-alist '("hs" .  "src haskell"))
(add-to-list 'org-structure-template-alist '("lhs" . "src haskell-lterate"))



(setq org-todo-keywords
      '((sequence "TODO(o)" "|" "DONE(e)")
        ;; For General
        (sequence "TODO(t)" "LEARNING(l)" "HACKING(h)" "WAITING(w)"
                  "|"
                  "DONE(d)" "DELEGATED(g)" "CANCELLED(c)")
        ;; For Study
        (sequence "✎(p)" "➤(n)" "♘(j)" "☕(s@)" "|" "✔(f@)" "✖(a@)")))

;; https://www.youtube.com/watch?v=nUvdddKZQzs
 (setq org-log-into-drawer t)
 ;; https://www.youtube.com/watch?v=R4QSTDco_w8
 (setq org-log-done 'note)
 (setq org-log-reschedule 'note)

;; information to record when the deadline date of a tasks is modified.
(setq org-log-redeadline 'note)

(setq org-archive-location "~/gtd/gtd.archive.org::datetree/* Finished from %s")

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"
                         "~/.config/emacs/myoungjin-init.org")) ;; or ~/.emacs.d/ ~~~

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/gtd.org"      :maxlevel . 2)
                           ("~/gtd/someday.org"  :level    . 1)
                           ("~/gtd/tickler.org"  :maxlevel . 2)
                           ;; or simply same level
                           ;; (org-agenda-files  :level . 1 )
                           ;; nil for current file
                           ;; put line bellow at last so that it shows
                           ;; at the bottom when searching
                           (nil :maxlevel . 9)))

;; copied from https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
;; I'm using ivy but still useful to search the tree to where I refile
(setq org-outline-path-complete-in-steps nil) ; Refile in a single go
(setq org-refile-use-outline-path t)    ; Show full paths for refiling

(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-agenda-custom-commands
      '(("h" "at MJ home" tags-todo "@home"
        ;; ((org-agenda-overriding-header "home")))))
        ((org-agenda-overriding-header "home")
         (org-agenda-skip-function #'our/org-aenda-skip-all-siblings-but-first)))))

;; CREDIT: https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(defun our/org-aenda-skip-all-siblings-but-first ()
  "Skip all but the first undone entry"
  (let (skip-entry?)
    (unless (our/is-current-org-todo?)
      (setq skip-entry? t))

    (save-excursion
      (while (and (not skip-entry?) (org-goto-sibling t))
        (when (our/is-current-org-todo?)
          (setq skip-entry? t))))
    (when skip-entry?
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun our/is-current-org-todo? ()
  (string= "TODO" (org-get-todo-state)))

(use-package org-bullets :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-hide-emphasis-markers t)

(defvar myjin/org-bullets-bullet-list-common
  '("❂" "⊛" "✪" "✵" "✼"  "✧" "⁕" )
  "rxvt-unicode can display those chars with nerd font; It is actually can be drawed with GNU FreeSerif also")

(defvar myjin/org-bullets-bullet-list)
(defvar myjin/org-ellipsis " »")
(setq myjin/org-bullets-bullet-list (cons (if (display-graphic-p) "⚝" "❃")
                                              myjin/org-bullets-bullet-list-common))

(setq org-bullets-bullet-list myjin/org-bullets-bullet-list)
(setq org-ellipsis (if (display-graphic-p) "⮯"  " »"))

(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-link-frame-setup
      (quote
       ((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame))))

;; From http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
(when (window-system)
  (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                               ((x-family-fonts "Fira Sans Compressed")   '(:family "Fira Sans Compressed"))
                               ((x-list-fonts "Fantasque Sans Mono")   '(:font "Fantasque Sans Mono"))
                               ((x-list-fonts "Verdana")         '(:font "Verdana"))
                               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-tuple))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
  )

;;; open bookmark when emacs is running withougt visiting a file.
;;  note: it is not working when emacs is running as daemon
(defun make-initial-buffer-as-bookmark-if-no-file-visited ()
  (let ((no-file-visited t)
        (args command-line-args))
    (dolist (arg (cdr args))
      (progn
        (if (file-exists-p arg)
            (setq no-file-visited nil))))
    (when no-file-visited
      (bookmark-bmenu-list) ;; create a book mark buffer first
      (setq initial-buffer-choice (lambda ()(get-buffer "*Bookmark List*"))))))

(add-hook 'after-init-hook
          (lambda () (make-initial-buffer-as-bookmark-if-no-file-visited)))

(use-package all-the-icons-dired :ensure t
  :config
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(use-package treemacs
    :ensure t
    :defer t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :config
    (progn
      (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
            treemacs-deferred-git-apply-delay      0.5
            treemacs-directory-name-transformer    #'identity
            treemacs-display-in-side-window        t
            treemacs-eldoc-display                 t
            treemacs-file-event-delay              5000
            treemacs-file-extension-regex          treemacs-last-period-regex-value
            treemacs-file-follow-delay             0.2
            treemacs-file-name-transformer         #'identity
            treemacs-follow-after-init             t
            treemacs-git-command-pipe              ""
            treemacs-goto-tag-strategy             'refetch-index
            treemacs-indentation                   2
            treemacs-indentation-string            " "
            treemacs-is-never-other-window         nil
            treemacs-max-git-entries               5000
            treemacs-missing-project-action        'ask
            treemacs-move-forward-on-expand        nil
            treemacs-no-png-images                 nil
            treemacs-no-delete-other-windows       t
            treemacs-project-follow-cleanup        nil
            treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
            treemacs-position                      'left
            treemacs-recenter-distance             0.1
            treemacs-recenter-after-file-follow    nil
            treemacs-recenter-after-tag-follow     nil
            treemacs-recenter-after-project-jump   'always
            treemacs-recenter-after-project-expand 'on-distance
            treemacs-show-cursor                   nil
            treemacs-show-hidden-files             t
            treemacs-silent-filewatch              nil
            treemacs-silent-refresh                nil
            treemacs-sorting                       'alphabetic-asc
            treemacs-space-between-root-nodes      t
            treemacs-tag-follow-cleanup            t
            treemacs-tag-follow-delay              1.5
            treemacs-user-mode-line-format         nil
            treemacs-user-header-line-format       nil
            treemacs-width                         35)

      ;; The default width and height of the icons is 22 pixels. If you are
      ;; using a Hi-DPI display, uncomment this to double the icon size.
      (treemacs-resize-icons 18)

      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode t)
      (pcase (cons (not (null (executable-find "git")))
                   (not (null treemacs-python-executable)))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'simple))))
    :bind
    (:map global-map
          ("M-0"       . treemacs-select-window)
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("C-x t t"   . treemacs)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag)))

;  (use-package treemacs-evil
;    :after treemacs evil
;    :ensure t)

  (use-package treemacs-projectile
    :after treemacs projectile
    :ensure t)

  (use-package treemacs-magit
    :after treemacs magit
    :ensure t)

  (use-package treemacs-persp
    :after treemacs persp-mode
    :ensure t
    :config (treemacs-set-scope-type 'Perspectives))

(use-package dockerfile-mode :ensure t)

(unless (display-graphic-p) ;; it is buggy with my X-window setup
    (use-package beacon :ensure t
      :config
      (progn
      (beacon-mode 1)
      (setq beacon-blink-when-buffer-changes t)
      (setq beacon-blink-when-focused t))))

;;(use-package neotree
;;  :ensure t
;;  :config (progn
;;            (setq neo-smart-open t)
;;            (setq neo-window-fixed-size nil)
;;            (global-set-key [f8] 'neotree-toggle)))
;;    (evil-leader/set-key
;;     "tt" 'neotree-toggle
;;           "tp" 'neotree-projectile-action)))

(use-package nov
  :ensure t
  :init
  (defun my-nov-font-setup ()
    (face-remap-add-relative
    'variable-pitch '(:family "Bookerly" :height 1.3)))
  :config
  (progn
    (setq nov-text-width t)
    (setq visual-fill-column-center-text t)
    (add-hook 'nov-mode-hook (lambda () (visual-line-mode)))
    (if (display-graphic-p)
        (add-hook 'nov-mode-hook 'my-nov-font-setup))
    (add-to-list 'auto-mode-alist '("\\.epub\$" . nov-mode))))

(setq max-image-size "no limit??")

(defvar visual-wrap-column nil)

(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
to current buffer) by setting the right-hand margin on every
window that displays BUFFER.  A value of NIL or 0 for
NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
    (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
    (set-window-margins nil nil)
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
        (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available visual-wrap-column))))))

(defvar hidden-minor-modes ; example, write your own list of hidden
  '(abbrev-mode            ; minor modes
    auto-fill-function
    auto-complete-mode
;    flycheck-mode
;    flyspell-mode
;    inf-haskell-mode
;    haskell-indent-mode
;    haskell-doc-mode
    smooth-scroll-mode))

(defun purge-minor-modes ()
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))

(add-hook 'after-change-major-mode-hook 'purge-minor-modes)
