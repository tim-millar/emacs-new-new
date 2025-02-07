;; -*- lexical-binding: t; -*-

;; Startup & UI
;; ==============================

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f")
		     gcs-done)))

(setq inhibit-splash-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(show-paren-mode t)
(blink-cursor-mode -1)
(global-hl-line-mode t)
(winner-mode t)

(set-face-attribute 'default nil :font "Fira Code-14")

;; set package archives
;; ==============================

(setq package-enable-at-startup nil)

(setq package-archives '(
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 )
      package-archive-priorities '(("melpa" . 1)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(require 'json)

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("~/.rvm" "~/.nvm" "~/.pyenv"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Set general keybindings
;; ==============================

(use-package general
  :ensure t
  :init
  (setq general-override-states '(insert
				  emacs
				  hybrid
				  normal
				  visual
				  motion
				  operator
				  replace))
  :config
  (general-create-definer tyrant-def
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   )

  (tyrant-def
    ""     nil
    "c"   (general-simulate-key "C-c" :which-key "C-c")
    "g"   goto-map
    "h"   help-map
    "s"   search-map
    "t"   tab-bar-map
    "u"   'universal-argument
    "x"   (general-simulate-key "C-x" :which-key "C-x")

   "SPC" '(execute-extended-command :which-key "M-x")
   "TAB" 'next-buffer
   "DEL" 'previous-buffer

   "b" '(:ignore t :which-key "buffers")
   "bb" 'consult-buffer
   "bi" 'ibuffer
   "bk" 'kill-this-buffer
   "bs" 'save-buffer
   "bS" 'save-some-buffer
   "b-" 'split-window-vertically
   "b/" 'split-window-horizontally
   "bd" 'dired-jump
   "br" 'my/rubocop-autocorrect
   ;; "by" 'show-buffer-file-name

   "f" '(:ignore t :which-key "files")
   "ff" 'find-file
   "fo" 'find-file-other-window
   "fr" 'consult-recent-file

   "a" '(:ignore t :which-key "completion")
   "ap" 'completion-at-point
   "at" 'complete-tag   ; etags
   "ad" 'cape-dabbrev   ; basically `dabbrev-completion'
   "af" 'cape-file
   "ak" 'cape-keyword
   "as" 'cape-symbol
   "aa" 'cape-abbrev
   "ai" 'cape-ispell
   "al" 'cape-line
   "aw" 'cape-dict
   "a\\" '\cape-tex
   "a_" 'ca_pe-tex
   "a^" 'cape-tex
   "a&" 'cape-sgml
   "ar" 'cape-rfc1345
  )

  ;; (general-define-key
  ;;  :keymaps 'override
  ;;  :states '(normal visual insert emacs)
  ;;  :prefix "SPC"
  ;;  :non-normal-prefix "C-SPC"

  ;;  ;; "," 'evilnc-comment-operator
  ;;  ;; "." 'evilnc-copy-and-comment-operator

  ;;  ;; "g" '(:ignore t :which-key "git")
  ;;  ;;	 "gf" 'magit-log-buffer-file
  ;;  ;; "gs" 'magit-status
  ;;  ;; "gg" 'counsel-git-grep
  ;;  ;; "gt" 'git-timemachine-toggle
  ;;  ;; "gb" 'magit-blame

  ;;  ;; "j"  '(:ignore t :which-key "jump")
  ;;  ;; "jj" 'dumb-jump-go
  ;;  ;; "jx" 'xref-find-definitions
  ;;  ;; "jf" 'lsp-ivy-workspace-symbol

  ;;  ;; "p" '(projectile-command-map :which-key "projectile")
  ;;  ;; "r" '(projectile-rails-command-map :which-key "projectile-rails")
  ;;  ;; "l" '(lsp-mode-map :which-key "lsp-mode") ;; why does this not work?

  ;;  ;; "xi" 'tm/iterm-focus
  ;;  ;; "xd" 'tm/iterm-goto-filedir-or-home
  ;;  ;; "xx" 'eshell-here
  ;;  ;; "xu" 'sp-unwrap-sexp
  ;;  ;; "xc" 'org-capture
  ;;  ;; "xa" 'org-agenda
  ;;  ;; "xl" 'org-store-link
  ;;  ;; "xs" 'scratch

  ;;  ;; "xr" '(:ignore t :which-key "ruby")
  ;;  ;; "xri" 'bundle-install
  ;;  ;; "xrr" '(rspec-mode-keymap :which-key "rspec")
  ;;  ;; "xrb" 'ruby-toggle-block
  ;;  ;; "xrs" 'ruby-toggle-string-quotes
  ;;  ;; "xry" 'ruby-tools-to-symbol
  ;;  )
  )

;; Themes and appearance
;; ==============================

(use-package doom-themes
  :config
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :defer t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))

(use-package all-the-icons
  :init
  (setq all-the-icons-color-icons t))

(use-package all-the-icons-dired
  :diminish all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :after (all-the-icons marginalia)
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :bind
  (("<f5>" . which-key-show-top-level))
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.75))

;; evil config
;; ==============================

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-minibuffer nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'inf-ruby-mode 'emacs)
  ;; (evil-set-initial-state 'commint-mode 'normal)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'erc-mode 'emacs)
  (evil-set-initial-state 'custom-mode 'emacs)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-escape
  :diminish evil-escape-mode
  :config
  (evil-escape-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (with-eval-after-load 'compilation
    (evil-define-key 'normal compilation-mode-map "q" 'quit-window))
  )

;; ==============================
;; vertico, consult etc
;; ==============================

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Configure Vertico behavior
  (setq vertico-resize t          ; Grow and shrink minibuffer
	vertico-cycle t)          ; Enable cycling through candidates

  ;; Workaround for `tramp` hostname completions
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
	 (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
	 (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
	       '(basic-remote kb/basic-remote-try-completion kb/basic-remote-all-completions nil))

  :bind
  (("C-c c" . vertico-repeat)
   :map vertico-map
   ("C-j" . vertico-next)
   ("C-k" . vertico-previous)
   ("<tab>" . vertico-insert)
   ("C-M-n" . vertico-next-group)
   ("C-M-p" . vertico-previous-group)
   ("RET" . vertico-directory-enter)
   ("C-i" . vertico-quick-insert)
   ("C-o" . vertico-quick-exit)
   ("C-w" . vertico-directory-delete-word)
   ("M-o" . kb/vertico-quick-embark))  ; Use quick keys with embark

  :hook
  (minibuffer-setup . vertico-repeat-save)  ; Save state for repeat
  (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Tidy up file paths

  :config
  ;; Enable multiform mode for flexible layouts
  (vertico-multiform-mode)
  (setq vertico-multiform-categories
	'((consult-grep buffer)
	  (org-roam-node reverse indexed))
	vertico-multiform-commands
	'((consult-yank-pop indexed)
	  (org-refile grid reverse)))
  )


;; Use vertico-directory for file navigation
(use-package vertico-directory
  :straight nil  ; Part of vertico, no need to install separately
  :after vertico
  :bind (:map vertico-map
	      ("<backspace>" . vertico-directory-delete-char)
	      ("C-w" . vertico-directory-delete-word)
	      ("RET" . vertico-directory-enter))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion))))

  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
	'(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
	'(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  :custom
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     ))
  )

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)                  ;; Alternative: consult-fd
	 ("M-s c" . consult-locate)
	 ("M-s G" . consult-grep)
	 ("M-s g" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)

  (advice-add #'project-find-regexp :override #'consult-ripgrep)
)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	 ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :bind
  (("C-," . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :general
  (:keymaps 'corfu-map
   :states 'insert
   "C-j" #'corfu-next
   "C-k" #'corfu-previous
   "<escape>" #'corfu-quit
   "<return>" #'corfu-insert
   "<TAB>" #'corfu-complete
   "C-i" #'corfu-quick-insert
   "M-d" #'corfu-info-documentation
   "M-l" #'corfu-info-location)
  :config
  (setq corfu-auto t
	corfu-auto-prefix 1
	corfu-count 20)
  )

;; Install and configure cape
(use-package cape
  :after corfu
  :config
  ;; Add desired cape sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; Additional cape sources can be added here
  )

;; Optional: Add corfu-popupinfo
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay 0.5))

;; ==============================
;; Dired
;; ==============================

(use-package dired
  :straight nil
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'top
	dired-dwim-target t
	dired-use-ls-dired nil)
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (dired-hide-details-mode t)))
  (evil-collection-define-key 'normal 'dired-mode-map
    ;; "h" 'dired-single-up-directory   # <=====
    ;; "l" 'dired-single-buffer         # <=====
    " " 'nil
    ))

;; ==============================
;; Magit
;; ==============================

(use-package magit
  :ensure t
  :config
  ;; (setq magit-completing-read-function 'ivy-completing-read)
  ;; (setq magit-refs-local-branch-format "%4c %-25n %h %U%m\n")
  (magit-wip-mode 1))

(use-package git-timemachine
  :ensure t
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

;; ==============================
;; Writing
;; ==============================

(use-package writeroom-mode
  :defer t)

;; ==============================
;; Software Development / Programming General
;; ==============================

(use-package add-node-modules-path
  :config
  (add-hook 'js-mode-hook 'add-node-modules-path))

(use-package flycheck
  :after add-node-modules-path
  :init
  (setq flycheck-ruby-rubocop-executable "docker compose exec web bundle exec rubocop")
  (global-flycheck-mode))

(use-package direnv
  :config
  (direnv-mode))

;; lsp mode
;; lsp-mode for language server integration
(use-package lsp-mode
  :hook ((ruby-mode . lsp)
	 (python-mode . lsp)
	 (js-mode . lsp))
  :commands lsp lsp-deferred
  :init
  ;; Enable LSP features
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers nil) ;; Optional: Disable file watchers for large projects
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; Configure Ruby language server
  (setq lsp-solargraph-use-bundler nil
	lsp-solargraph-diagnostics t)
  (setq lsp-completion-provider :none
	lsp-idle-delay 0.0
	lsp-enable-snippet t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-solargraph-server-command '("docker" "compose" "exec" "-T" "web" "bundle" "exec" "solargraph" "stdio")))

;; Optional: Use `lsp-ui` for better UI integrations
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
	lsp-ui-peek-enable t
	lsp-ui-doc-enable t))

;; Install and configure Copilot
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . copilot-accept-completion)  ;; Bind Ctrl+Tab to accept Copilot suggestions
	 ("C-<tab>" . copilot-accept-completion))
  :config
  (setq copilot-no-tab-map t)  ;; Prevent Copilot from overriding TAB

  ;; Disable Copilot in ruby-mode to prevent conflicts with Solargraph
  (add-hook 'ruby-mode-hook (lambda () (copilot-mode -1))))

(use-package dumb-jump
  :commands (dumb-jump-go)
  :init
  (setq dumb-jump-aggressive nil)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-force-searcher 'rg)
  ;; (setq dump-jump-debug t)
  (setq dumb-jump-disable-obsolete-warnings t)
  (setq dumb-jump-selector 'completing-read)
  :config
  ;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-to-list 'xref-backend-functions 'dumb-jump-xref-activate)
  (dumb-jump-mode))

;; eglot doesn't work!
;; ;; Install and configure eglot
;; (use-package eglot
;;   :hook
;;   (ruby-mode . eglot-ensure)
;;   :config
;;   (setq eglot-inlay-hints-mode t)
;;   ;; Using solargraph directly
;;   ;; (add-to-list 'eglot-server-programs '((ruby-mode) . ("solargraph" "socket" "--port" "0")))
;;   ;; (add-to-list 'eglot-server-programs
;;   ;;		       '(ruby-mode . ("/Users/timmillar/code/palace/running-tings/debug.sh")))
;;   (add-to-list 'eglot-server-programs
;;	       '(ruby-mode . ("/Users/timmillar/.gem/ruby/2.7.5/bin/solargraph" "socket" "--port" "0")))

;;   (setq eglot-connect-timeout 60)

;;   ;; Optional: Increase eglot's logging level for more detailed logs
;;   ;; (setq eglot-events-buffer-size 5000)
;; )

;; (use-package flycheck-eglot
;;   :after (eglot flycheck)
;;   :config
;;   (add-hook 'eglot-managed-mode-hook #'flycheck-eglot-setup))

;; ==============================
;; Software Development / Programming Language Specific
;; ==============================

(use-package ruby-mode
  :interpreter "ruby"
  :mode
  ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode))

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter-and-focus)))

(use-package terraform-mode
  :mode ("\\.tf\\'" "\\.tfvars\\'")
  :custom (terraform-format-on-save t))

(use-package php-mode
  :mode "\\.php\\'")

(use-package dockerfile-mode)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :init
  (setq typescript-indent-level 2))

;; ==============================
;; Stuff
;; ==============================

(use-package ansi-color)

(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(defun my/compile-in-environments-directory ()
  (interactive)
  (let ((default-directory
	  (if (string= (file-name-extension buffer-file-name) "tf")
	      (concat default-directory "./aws/terraform/environments")
	    default-directory))))
  (call-interactively #'compile))

(defun my/rubocop-autocorrect ()
  "Run Rubocop autocorrect on the current buffer, handling containerized projects.

If the project contains a docker-compose.yml or docker-compose.yaml file and defines a service named 'web',
Rubocop will be run inside the 'web' container using Docker Compose.
Otherwise, Rubocop will be run locally, either via Bundler if available,
or directly if not."
  (interactive)
  (when (eq major-mode 'ruby-mode)
    (save-buffer)  ;; Ensure buffer is saved before running Rubocop
    (let* (
	   ;; Define the names of docker-compose files to look for
	   (docker-compose-files '("docker-compose.yml" "docker-compose.yaml"))

	   ;; Find the project root by locating a docker-compose file or Gemfile
	   (project-root
	    (or
	     (locate-dominating-file buffer-file-name "docker-compose.yml")
	     (locate-dominating-file buffer-file-name "docker-compose.yaml")
	     (locate-dominating-file buffer-file-name "Gemfile")
	     default-directory))

	   ;; Set the default directory to the project root
	   (default-directory (or project-root default-directory))

	   ;; Get the absolute path of the current file
	   (file (buffer-file-name))

	   ;; Get the path of the current file relative to the project root
	   (relative-file (file-relative-name file default-directory))

	   ;; Determine the Rubocop command based on the project setup
	   (rubocop-command
	    (cond
	     ;; If a docker-compose file is present, assume containerized project
	     ((or
	       (file-exists-p (expand-file-name "docker-compose.yml" project-root))
	       (file-exists-p (expand-file-name "docker-compose.yaml" project-root)))
	      ;; Construct the Docker Compose command to run Rubocop inside the 'web' container
	      (format "docker compose exec web bundle exec rubocop -A %s"
		      (shell-quote-argument relative-file)))

	     ;; Else if Bundler is available, use it to run Rubocop
	     ((executable-find "bundle")
	      (format "bundle exec rubocop -A %s" (shell-quote-argument relative-file)))

	     ;; Else, run Rubocop directly
	     (t
	      (format "rubocop -A %s" (shell-quote-argument relative-file))))))
      ;; Start the Rubocop process in a compilation buffer named "*Rubocop Autocorrect*"
      (compilation-start rubocop-command
			 'compilation-mode
			 (lambda (mode-name) "*Rubocop Autocorrect*")))))

;; ==============================
;; Emacs Auto-gen
;; ==============================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dumb-jump lsp-mode eglot flycheck-eglot direnv yaml-mode dockerfile-mode add-node-modules-path all-the-icons-completion all-the-icons-dired cape consult corfu doom-modeline doom-themes embark embark-consult evil-collection evil-escape exec-path-from-shell flycheck general git-timemachine magit marginalia orderless php-mode rvm terraform-mode vertico which-key writeroom-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
