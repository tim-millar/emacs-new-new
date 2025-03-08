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

(setq column-number-mode t
      sentence-end-double-space nil
      fill-column 80
      ispell-program-name "aspell")

(defalias 'list-buffers 'ibuffer)

(defun tm/setup-line-numbers ()
  "Setup line numbers."
  (progn
    (display-line-numbers-mode t)
    (setq display-line-numbers-type t
          display-line-numbers-current-absolute t
          display-line-numbers-width 3)))

(add-hook 'prog-mode-hook 'tm/setup-line-numbers)

;; file backups
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t))
      backup-by-copying t)

;; set package archives
;; ==============================

;; (setq package-enable-at-startup nil)

;; (setq package-archives '(
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ("melpa-stable" . "https://stable.melpa.org/packages/")
;; 			 ("gnu" . "https://elpa.gnu.org/packages/")
;; 			 ("org" . "https://orgmode.org/elpa/")
;; 			 )
;;       package-archive-priorities '(("melpa" . 1)))

;; (package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (setq use-package-always-ensure t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(require 'json)

;; Use straight.el by default in use-package
(setq straight-use-package-by-default t)

;; Install use-package via straight.el
(straight-use-package 'use-package)

(setq straight-cache-autoloads t)

(use-package exec-path-from-shell
  :config
  (dolist (var '("~/.rvm" "~/.nvm" "~/.pyenv"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(setq auth-sources '("~/.authinfo"))

;; General setup
;; ==============================

(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'kill-emacs-query-functions
	  (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
	  'append)

(setq byte-compile-warnings '(not docstrings))

(use-package diminish
  :hook (after-init . (lambda ()
                        (mapc #'diminish minor-mode-list))))

;; Set general keybindings
;; ==============================

(use-package general
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
   "bK" 'kill-buffer-and-window
   "bs" 'save-buffer
   "bS" 'save-some-buffer
   "b-" 'split-window-vertically
   "b/" 'split-window-horizontally
   "bd" 'dired-jump
   "br" 'my/rubocop-autocorrect
   ;; "by" 'show-buffer-file-name
   "be" 'my/eshell-here
   "bf" 'fill-region

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
   "a^" 'cape-tex
   "a&" 'cape-sgml
   "ar" 'cape-rfc1345

   "p" '(:keymap project-prefix-map :which-key "project")

   "gg" 'dumb-jump-go
   "gG" 'dumb-jump-back

   "oa" '(org-agenda :which-key "org-agenda")
   "oc" '(org-capture :which-key "org-capture")
   "on" '(my/org-ql-ai-notes :which-key "org-ql-ai-notes")
   "oN" '(my/org-ql-general-notes :which-key "org-ql-general-notes")
   "oj" '(my/org-ql-journal-entries :which-key "org-ql-journal-entries")
   "ot" '(my/org-ql-outstanding-todos :which-key "org-ql-outstanding-todos")
  )

  (general-define-key
   :keymaps 'tab-prefix-map
   "t" 'tab-switch
   "k" 'tab-bar-close-tab
   "x" 'other-tab-prefix
   "<tab>" 'tab-next
   "<backspace>" 'tab-previous
   )
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

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator)
  :after evil
  :config
  (evilnc-default-hotkeys))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

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
        '((org-roam-node reverse indexed))
        vertico-multiform-commands
        '((consult-yank-pop indexed) (org-refile grid reverse))))

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
   ("C-c y" . consult-yank-from-kill-ring)
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g l" . consult-goto-line)             ;; orig. goto-line
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
	 ("M-s s" . consult-ripgrep)
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
   ("M-i" . my/consult-insert-word-at-point)
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

(defun my/consult-insert-word-at-point ()
  "Insert the word or symbol at point from the original buffer into the minibuffer."
  (interactive)
  (let ((word (with-current-buffer (window-buffer (minibuffer-selected-window))
                (or (thing-at-point 'symbol t)
                    (thing-at-point 'word t)))))
    (when word
      (insert word))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
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
  :init
  (global-corfu-mode)
  :general
  (:keymaps 'corfu-map
   :states 'insert
   "C-j" #'corfu-next
   "C-k" #'corfu-previous
   "<escape>" #'corfu-quit
   "C-g" #'corfu-quit
   "<return>" #'corfu-insert
   "<S-SPC>" #'corfu-insert-separator
   "<TAB>" #'corfu-complete
   "C-i" #'corfu-quick-insert
   "M-d" #'corfu-info-documentation
   "M-l" #'corfu-info-location)
  :config
  (setq corfu-auto t
	corfu-auto-prefix 1
	corfu-count 20))

;; Install and configure cape
(use-package cape
  :after corfu
  :config
  ;; Add desired cape sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-line)
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
;; Shells and utilities
;; ==============================

(use-package eat
  :straight (:type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  :init
  (setq eat-term-name "xterm-256color")
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode)))

(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell
  :init
  (setq eshell-history-size 10000
        eshell-hist-ignoredups t
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input t
        eshell-term-name "eat")
  ;; (setq eshell-aliases-file "~/.emacs.vertico/eshell/alias")
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setenv "TERM" "xterm-256color")
              ;; (eshell/alias "ecs_exec_rt_rake" "eshell/ecs_exec_rt_rake $1")
              (eshell/alias "ecs-exec-rt-rake" "my/ecs-exec-rt-console")
              (eshell/alias "aws-set-creds" "my/aws-set-credentials")
              (define-key eshell-mode-map (kbd "C-d")
                          'my/eshell-quit-or-delete-char))))

(defun my/eshell-quit-or-delete-char (arg)
  "Quits the eshell or deletes ARG (a char)."
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much) ; Why not? (eshell/exit)
        (delete-window))
    (delete-forward-char arg)))

(defun my/eshell-here ()
  "Opens up a new shell in the directory associated with the current
buffer's file. The eshell is renamed to match that directory to make
multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

;; (defun eshell/ecs_exec_rt_rake (task_name)
;;   "Run ECS exec rake command from Eshell."
;;   (let* ((region "eu-west-1")
;;          (cluster "internal-production")
;;          (container_name "running-tings")
;;          (command (format "bundle exec rake %s" task_name))
;;          (task_arns (shell-command-to-string
;;                      (format "aws ecs list-tasks --region %s --cluster %s --family %s --desired-status RUNNING --query 'taskArns' --output text"
;;                              region cluster container_name)))
;;          (task_arn (car (split-string task_arns))))
;;     (if (string-empty-p task_arn)
;;         (progn
;;           (message "No running tasks found for container: %s in cluster: %s" container_name cluster)
;;           nil)
;;       (shell-command
;;        (format "aws ecs execute-command --region %s --cluster %s --task %s --container %s --command '%s' --interactive"
;;                region cluster task_arn container_name command)))))

;; (defun eshell/ecs_exec_rt_console ()
;;   "Run ECS exec rake environment:console command from Eshell."
;;   (let* ((region "eu-west-1")
;;          (cluster "internal-production")
;;          (container-name "running-tings")
;;          (command "bundle exec rake environment:console")
;;          (interactive "--interactive")
;;          (aws-env (format "AWS_ACCESS_KEY_ID=%s AWS_SECRET_ACCESS_KEY=%s AWS_SESSION_TOKEN=%s"
;;                           (getenv "AWS_ACCESS_KEY_ID")
;;                           (getenv "AWS_SECRET_ACCESS_KEY")
;;                           (getenv "AWS_SESSION_TOKEN")))
;;          (task-arns (string-trim
;;                      (shell-command-to-string
;;                       (format "%s aws ecs list-tasks \
;;                                --region %s \
;;                                --cluster %s \
;;                                --family %s \
;;                                --desired-status RUNNING \
;;                                --query 'taskArns' \
;;                                --output text"
;;                               aws-env region cluster container-name))))
;;          (task-arn (if (string-match "\\(arn:aws:ecs:[^[:space:]]+\\)" task-arns)
;;                        (match-string 1 task-arns)
;;                      "")))
;;     (message "Raw task ARNs: %s" task-arns)
;;     (if (string-empty-p task-arn)
;;         (progn
;;           (message "No running tasks found for container: %s in cluster: %s" container-name cluster)
;;           nil)
;;       (message "Executing rake task on container: %s with task ARN: %s" container-name task-arn)
;;       (async-shell-command
;;        (format "%s aws ecs execute-command \
;;                 --region %s \
;;                 --cluster %s \
;;                 --task %s \
;;                 --container %s \
;;                 --command \"%s\" \
;;                 %s"
;;                aws-env region cluster task-arn container-name command interactive)))))

(defun my/ecs-exec-rt-console ()
  "Run ECS exec rake environment:console command from Eshell using a comint buffer."
  (interactive)
  (let* ((region "eu-west-1")
         (cluster "internal-production")
         (container-name "running-tings")
         (command "bundle exec rake environment:console")
         (interactive-flag "--interactive")
         (buffer-name "*ecs-pry*")
         
         ;; AWS credentials from environment variables
         (aws-env (format "AWS_ACCESS_KEY_ID=%s AWS_SECRET_ACCESS_KEY=%s AWS_SESSION_TOKEN=%s"
                          (getenv "AWS_ACCESS_KEY_ID")
                          (getenv "AWS_SECRET_ACCESS_KEY")
                          (getenv "AWS_SESSION_TOKEN")))

         ;; Retrieve the running task ARN
         (task-arns (string-trim
                     (shell-command-to-string
                      (format "%s aws ecs list-tasks \
                               --region %s \
                               --cluster %s \
                               --family %s \
                               --desired-status RUNNING \
                               --query 'taskArns' \
                               --output text"
                              aws-env region cluster container-name))))
         (task-arn (if (string-match "\\(arn:aws:ecs:[^[:space:]]+\\)" task-arns)
                       (match-string 1 task-arns)
                     "")))

    ;; Display retrieved task ARNs for debugging
    (message "Raw task ARNs: %s" task-arns)

    ;; Check if task ARN was found
    (if (string-empty-p task-arn)
        (progn
          (message "No running tasks found for container: %s in cluster: %s" container-name cluster)
          nil)
      (let ((ecs-command (format "%s aws ecs execute-command \
                                   --region %s \
                                   --cluster %s \
                                   --task %s \
                                   --container %s \
                                   --command \"%s\" \
                                   %s"
                                 aws-env region cluster task-arn container-name command interactive-flag)))

        ;; Create a comint buffer and run the command
        (with-current-buffer (get-buffer-create buffer-name)
          (unless (comint-check-proc (current-buffer))
            (make-comint-in-buffer "ecs-pry" (current-buffer) "sh" nil "-c" ecs-command))
          (display-buffer (current-buffer)))
        
        (message "Connected to ECS Pry session on container: %s" container-name)))))

(defun my/aws-set-credentials ()
  "Prompt to paste AWS credentials and set them in the environment."
  (interactive)
  (let ((input (read-string "Paste AWS credentials: ")))
    (when (string-match "export AWS_ACCESS_KEY_ID=\"\\(.*?\\)\"" input)
      (setenv "AWS_ACCESS_KEY_ID" (match-string 1 input)))
    (when (string-match "export AWS_SECRET_ACCESS_KEY=\"\\(.*?\\)\"" input)
      (setenv "AWS_SECRET_ACCESS_KEY" (match-string 1 input)))
    (when (string-match "export AWS_SESSION_TOKEN=\"\\(.*?\\)\"" input)
      (setenv "AWS_SESSION_TOKEN" (match-string 1 input))))
  (message "AWS credentials set!"))

(set-register ?e (cons 'file "~/.emacs.vertico/init.el"))
(set-register ?z (cons 'file "~/.zshrc"))
(set-register ?g (cons 'file "~/Documents/org/general.org"))
(set-register ?n (cons 'file "~/Documents/org/notes.org"))
(set-register ?a (cons 'file "~/Documents/org/ai-learning-plan.org"))

;; ==============================
;; Magit
;; ==============================

(use-package magit
  :commands (magit-status magit-blame)
  :config
  ;; (setq magit-completing-read-function 'ivy-completing-read)
  ;; (setq magit-refs-local-branch-format "%4c %-25n %h %U%m\n")
  (magit-wip-mode 1))

(use-package git-timemachine
  :commands (git-timemachine-toggle)
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

(add-hook 'prog-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)))

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
  )

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
;; Software Development / LLMs and Gen AI
;; ==============================

(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :init
  (setq gptel-api-key
        (let ((entry (car (auth-source-search :host "openai.com" :max 1 :require '(:key)))))
          (when entry
            (let ((api-key (plist-get entry :key)))
              (when (stringp api-key) api-key)))))
  (setq gptel-default-model "gpt-4"
        gptel-temperature 0.7)
  :general
  (:keymaps 'gptel-mode-map
            :states '(normal insert visual motion)
            "C-<return>" 'gptel-send)  ;; Send input with Ctrl+Enter
  (tyrant-def
    "jg" '(gptel :which-key "Open GPT chat")
    "js" '(gptel-send :which-key "Send to GPT")
    "jm" '(gptel-menu :which-key "GPT Menu")))

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
;; org-mode
;; ==============================

(use-package org
  :straight t

  :config
  (setq org-directory "~/Documents/org")

  (setq org-agenda-files '("~/Documents/org/agenda.org"
                           "~/Documents/org/notes.org"
                           "~/Documents/org/ai-notes.org"
                           "~/Documents/org/tasks.org"
                           "~/Documents/org/ai-tasks.org"
                           "~/Documents/org/ai-learning-plan.org"
                           "~/Documents/org/journal.org"))

  (setq org-log-done 'time)  ;; Log timestamp when a task is marked DONE
  (setq org-startup-indented t)  ;; Pretty indentation
  (setq org-insert-heading-respect-content t)
  (setq org-fold-catch-invisible-edits 'show-and-error)
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)

  (setq org-agenda-start-on-weekday nil
        org-agenda-span 'week
        org-deadline-warning-days 7)

  (setq org-agenda-custom-commands
        '(("c" "Custom Agenda"
           ((agenda "")
            (todo "NEXT")
            (tags "project")))

          ;; View All TODOs (General + AI, Scheduled + Unscheduled)
          ("T" "All TODOs"
           ((todo "TODO")))

          ;; General TODOs Only (Excludes AI)
          ("g" "General TODOs (Non-AI)"
           ((tags-todo "-ai")))

          ;; AI-related TODOs Only
          ("a" "AI TODOs"
           ((tags-todo "ai")))

          ;; Scheduled Tasks Only
          ("s" "Scheduled Tasks"
           ((agenda "" ((org-agenda-entry-types '(:scheduled))))))

          ;; Unscheduled Tasks Only
          ("u" "Unscheduled Tasks"
           ((todo "TODO"
                  ((org-agenda-todo-ignore-scheduled 'all)
                   (org-agenda-todo-ignore-deadlines 'all)))))))

  (setq org-habit-graph-column 60)

  ;; Org Capture
  (setq org-capture-templates
        '(;; General Tasks
          ("t" "Task" entry
           (file+headline "~/Documents/org/tasks.org" "Backlog")
           "* TODO %?\n  %u\n  %a")

          ;; Scheduled Tasks
          ("s" "Scheduled Task" entry
           (file+headline "~/Documents/org/tasks.org" "Scheduled Tasks")
           "* TODO %?\nSCHEDULED: %^t\n  %u\n  %a")

          ;; Notes
          ("n" "Note" entry (file+headline "~/Documents/org/notes.org" "Notes")
           "* %?\n  %u\n  %a")

          ;; Journal
          ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
           "* %?\nEntered on %U")

          ;; AI Tasks
          ("a" "AI Task" entry
           (file+headline "~/Documents/org/ai-tasks.org" "AI Learning Tasks")
           "* TODO %? :ai:\n  %u\n  Linked to: [[file:ai-learning-plan.org::*%^{Topic}]]\n  %a")
          
          ;; Scheduled AI Tasks
          ("A" "Scheduled AI Task" entry
           (file+headline "~/Documents/org/ai-tasks.org" "Scheduled AI Learning Tasks")
           "* TODO %? :ai:\nSCHEDULED: %t\n:PROPERTIES:\n:ID: %U\n:END:\nLinked to: [[file:ai-learning-plan.org::*%^{Topic}]]\n")

          ("l" "AI Note" entry
           (file+headline "~/Documents/org/ai-notes.org" "AI Notes")
           "* %? :ai:\nEntered on %U\n\nRelated to: [[file:ai-learning-plan.org::*%^{Topic}]]\n")

          ("p" "AI Journal Entry" entry
           (file+datetree "~/Documents/org/journal.org")
           "* %? :ai:\nEntered on %U\n\nProgress on AI Learning: [[file:ai-learning-plan.org::*%^{Topic}]]\n")
          ))

  ;; Org Refile Settings
  (setq org-refile-targets '((("~/org/ai-learning-plan.org") :maxlevel . 2)
                             (("~/org/ai-tasks.org") :maxlevel . 2)
                             (("~/org/tasks.org") :maxlevel . 2)
                             (("~/org/ai-notes.org") :maxlevel . 2)
                             (("~/org/notes.org") :maxlevel . 2)
                             (("~/org/journal.org") :maxlevel . 2))))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-ql
  :after org)

(defun my/org-ql-ai-notes ()
  "Display second-level AI-related notes from ai-notes.org using org-ql."
  (interactive)
  (org-ql-search "~/Documents/org/ai-notes.org"
    '(or (parent "AI Notes")
         (tags "ai"))  ;; Only include direct subheadings of "AI Notes"
    ;; '(path "AI Notes")  ;; Match headings under "AI Notes"
    :sort '(date priority)
    :super-groups '((:name "AI Notes"
                           :auto-map (lambda (_) "AI Notes"))  ;; Override default category
                    (:name "Important AI Notes"
                           :tag "important")
                    (:name "Recent AI Notes"
                           :date t))))

(defun my/org-ql-general-notes ()
  "Display second-level general notes from notes.org using org-ql."
  (interactive)
  (org-ql-search "~/Documents/org/notes.org"
    '(or (parent "Notes")
         (tags "notes"))  ;; Include direct subheadings of "Notes" or tagged notes
    :sort '(date priority)
    :super-groups '((:name "Notes"
                           :auto-map (lambda (_) "Notes"))
                    (:name "Important Notes"
                           :tag "important")
                    (:name "Recent Notes"
                           :date t))))

(defun my/org-ql-journal-entries ()
  "Display journal entries from journal.org using org-ql."
  (interactive)
  (org-ql-search "~/Documents/org/journal.org"
    '(ts)  ;; Use 'ts' for all timestamps, or 'ts-active' for only active timestamps
    :sort '(date)
    :super-groups '((:name "Recent Entries" :date t))))

(defun my/org-ql-outstanding-todos ()
  "Search for outstanding TODOs in the org folder, sorted by most recently created first if they have a date."
  (interactive)
  (org-ql-search (directory-files "~/Documents/org/" t "\\.org$")
    '(todo)  ;; Select headings with TODO status
    :sort '(date)
    :super-groups '((:name "Outstanding TODOs"
                           :todo t)
                    (:name "Recently Created"
                           :date t))))

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
