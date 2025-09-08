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

;; Globally disable tabs for indentation
(setq-default indent-tabs-mode nil)

;; Set default tab width and indentation level to 2 spaces
(setq-default tab-width 2)
(setq-default standard-indent 2)

;; Ensure all programming modes use 2-space indentation
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 2)))
(setq js-indent-level 2)

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
  )

  (tyrant-def
    ;; Org Agenda and Capture
    "oa" '(org-agenda :which-key "org-agenda")
    "oc" '(org-capture :which-key "org-capture")
    ;; Org-QL commands
    "oq" '(:ignore t :which-key "org-queries")
    "oqn" '(my/org-ql-ai-notes :which-key "org-ql-ai-notes")
    "oqN" '(my/org-ql-general-notes :which-key "org-ql-general-notes")
    "oqj" '(my/org-ql-journal-entries :which-key "org-ql-journal-entries")
    "oqt" '(my/org-ql-outstanding-todos :which-key "org-ql-outstanding-todos")
    "oqg" 'org-tags-view
    ;; Org Roam commands grouped under "or"
    "or" '(:ignore t :which-key "org-roam")
    "orc" '(org-roam-capture :which-key "org-roam capture")
    "orf" '(consult-org-roam-file-find :which-key "org-roam file find")
    "ori" '(org-roam-node-insert :which-key "org-roam node insert")
    "ort" '(org-roam-buffer-toggle :which-key "org-roam buffer toggle")
    "orb" '(consult-org-roam-backlinks :which-key "org-roam backlinks")
    "orB" '(consult-org-roam-backlinks-recursive :which-key "org-roam backlinks recursive")
    "orl" '(consult-org-roam-forward-links :which-key "org-roam forward links")
    "orr" '(consult-org-roam-search :which-key "org-roam search")
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
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
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
(set-register ?d (cons 'file "~/Downloads"))

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

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-keep-variants nil)

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
  :hook ((js-ts-mode tsx-ts-mode typescript-ts-mode json-ts-mode)
         . add-node-modules-path))

;; flycheck
;; ==============================
(use-package flycheck
  :after add-node-modules-path
  :init
  (setq flycheck-ruby-rubocop-executable "docker compose exec web bundle exec rubocop")
  (global-flycheck-mode))

;; flycheck debugging functions
;; ==============================
(defun my/--expand-tilde (path)
  (if (and path (string-prefix-p "~" path))
      (expand-file-name path)
    path))

(defun my/eslint--exec ()
  (let ((exe (or (bound-and-true-p flycheck-javascript-eslint-executable)
                 (executable-find "eslint_d")
                 (executable-find "eslint")
                 (user-error "No eslint/eslint_d found"))))
    (my/--expand-tilde exe)))

(defun my/project-root-or-default ()
  (or (when-let ((pr (project-current))) (project-root pr))
      default-directory))

(defun my/eslint--file-arg ()
  "File arg for ESLint: relative to project root (works in containers)."
  (unless buffer-file-name (user-error "Buffer not visiting a file"))
  (let* ((root (file-truename (my/project-root-or-default)))
         (file (file-truename buffer-file-name)))
    (if (string-prefix-p root file)
        (file-relative-name file root)
      ;; fall back if not in project
      buffer-file-name)))

(defun my/eslint-version ()
  (interactive)
  (let* ((exe (my/eslint--exec))
         (cmd (format "%s --version" (shell-quote-argument exe))))
    (compilation-start cmd 'compilation-mode (lambda (_) "*eslint-version*"))))

(defun my/eslint-print-config ()
  (interactive)
  (let* ((exe (my/eslint--exec))
         (file (my/eslint--file-arg))
         (cmd (format "%s --print-config %s"
                      (shell-quote-argument exe)
                      (shell-quote-argument file))))
    (compilation-start cmd 'compilation-mode (lambda (_) "*eslint-print-config*"))))

(defun my/eslint-config-trace ()
  (interactive)
  (let* ((exe (my/eslint--exec))
         (file (my/eslint--file-arg))
         (process-environment (cons "DEBUG=eslint:config:*" process-environment))
         (cmd (format "%s --print-config %s"
                      (shell-quote-argument exe)
                      (shell-quote-argument file))))
    (compilation-start cmd 'compilation-mode (lambda (_) "*eslint-config-trace*"))))

;; select right node version
;; ==============================
;; (require 'seq)

;; (defun my/closest-node-bin (exe &optional start-dir)
;;   "Return the closest node_modules/.bin/EXE from START-DIR upward, or nil."
;;   (let* ((dir (file-truename (or start-dir
;;                                  (when-let ((pr (project-current))) (project-root pr))
;;                                  default-directory)))
;;          (root (locate-dominating-file dir "node_modules/.bin"))
;;          (candidate (and root (expand-file-name (concat "node_modules/.bin/" exe) root))))
;;     (when (and candidate (file-exists-p candidate)) candidate)))

;; (defun my/flat-config-present-p (&optional start-dir)
;;   "Non-nil if a flat ESLint config exists at/above START-DIR."
;;   (let* ((dir (file-truename (or start-dir default-directory)))
;;          (root (or (when-let ((pr (project-current))) (project-root pr)) dir)))
;;     (seq-some (lambda (n) (file-exists-p (expand-file-name n root)))
;;               '("eslint.config.js" "eslint.config.mjs" "eslint.config.cjs"))))

;; (defun my/js-pick-eslint ()
;;   "Prefer project-local eslint(_d); only add --no-eslintrc for flat config."
;;   (let* ((dir (or (when-let ((pr (project-current))) (project-root pr)) default-directory))
;;          ;; Prefer local > global
;;          (exe (or (my/closest-node-bin "eslint_d" dir)
;;                   (my/closest-node-bin "eslint" dir)
;;                   (executable-find "eslint_d")
;;                   (executable-find "eslint"))))
;;     (setq-local flycheck-javascript-eslint-executable exe)
;;     ;; Only forbid .eslintrc when a flat config exists
;;     (setq-local flycheck-javascript-eslint-args
;;                 (when (my/flat-config-present-p dir) '("--no-eslintrc")))
;;     ;; no container path rewriting on host
;;     (setq-local flycheck-substitute-paths nil)))

;; ;; Ensure node_modules/.bin is on PATH, then pick eslint (picker runs *after*)
;; (dolist (h '(js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook json-ts-mode-hook))
;;   (add-hook h #'my/js-pick-eslint 'append))
;; run after add-node-modules-path
;; ==============================

;; select right eslint version
;; ==============================
(require 'seq)

(defcustom my/eslint-prefer-container 'when-running
  "Prefer docker wrapper in dockerized repos."
  :type '(choice (const always) (const when-running) (const never)))

(defun my/docker-project-root-p (&optional dir)
  (let* ((d (file-truename (or dir default-directory)))
         (root (or (when-let ((pr (project-current nil d))) (project-root pr)) d)))
    (seq-some (lambda (n) (file-exists-p (expand-file-name n root)))
              '("docker-compose.yml" "docker-compose.yaml" "compose.yml" "compose.yaml"))))

(defun my/docker-service-running-p (&optional service)
  (eq 0 (call-process "bash" nil nil nil "-lc"
                      (format "docker compose ps -q %s 2>/dev/null | grep -q ." (or service "web")))))

(defun my/closest-node-bin (exe &optional dir)
  (let* ((d (file-truename (or dir default-directory)))
         (root (or (when-let ((pr (project-current nil d))) (project-root pr)) d))
         (p (expand-file-name (concat "node_modules/.bin/" exe) root)))
    (when (file-exists-p p) p)))

(defun my/flat-config-present-p (&optional dir)
  (let* ((d (file-truename (or dir default-directory)))
         (root (or (when-let ((pr (project-current nil d))) (project-root pr)) d)))
    (seq-some (lambda (n) (file-exists-p (expand-file-name n root)))
              '("eslint.config.js" "eslint.config.mjs" "eslint.config.cjs"))))

(defun my/js-pick-eslint ()
  "Order: container > project eslint(_d) > global. Also set env/path mapping."
  (let* ((d (file-truename (or (and (project-current) (project-root (project-current))) default-directory)))
         (wrapper (expand-file-name "~/bin/eslint-in-docker"))
         (docker-ok (and (file-exists-p wrapper) (my/docker-project-root-p d)
                         (pcase my/eslint-prefer-container
                           ('always t) ('when-running (my/docker-service-running-p "web")))))
         (exe (or (and docker-ok wrapper)
                  (my/closest-node-bin "eslint_d" d)
                  (my/closest-node-bin "eslint" d)
                  (executable-find "eslint_d")
                  (executable-find "eslint"))))
    (setq-local flycheck-javascript-eslint-executable exe)
    ;; Only add --no-eslintrc for flat-config projects
    (setq-local flycheck-javascript-eslint-args (when (my/flat-config-present-p d) '("--no-eslintrc")))
    ;; Node tools on PATH
    (when (fboundp 'add-node-modules-path) (add-node-modules-path))
    ;; Container extras
    (if (and docker-ok (string= exe wrapper))
        (let ((host-root (directory-file-name d)))
          (setq-local process-environment
                      (append (list "ESLINT_SERVICE=web"
                                    "ESLINT_WORKDIR=/usr/src/app"
                                    (concat "HOST_ROOT=" host-root))
                              process-environment))
          (setq-local flycheck-substitute-paths (list (cons "/usr/src/app" host-root))))
      (setq-local flycheck-substitute-paths nil))))

(dolist (h '(js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook json-ts-mode-hook))
  (add-hook h #'my/js-pick-eslint 'append))

(defun my/eslint-toggle-container ()
  (interactive)
  (setq-local my/eslint-prefer-container
              (if (eq my/eslint-prefer-container 'never) 'always 'never))
  (message "my/eslint-prefer-container → %s" my/eslint-prefer-container)
  (my/js-pick-eslint)
  (flycheck-buffer))
;; ==============================

(use-package direnv
  :config
  (direnv-mode))

(use-package apheleia
  :init
  (setq apheleia-mode-alist
        '((js-ts-mode . prettier) (tsx-ts-mode . prettier)
          (typescript-ts-mode . prettier) ; or biome
          (ruby-ts-mode . rubocop)        ; or standardrb
          (elixir-ts-mode . mix-format)   ; mix format
          (sql-mode . pgformatter)))
  :config (apheleia-global-mode +1))

(use-package sqlformat
  :hook (sql-mode . sqlformat-on-save-mode)
  :init (setq sqlformat-command 'pgformatter
              sqlformat-args '("-s2" "-g")))

;; lsp mode
;; lsp-mode for language server integration
(use-package lsp-mode
  :hook
  ((ruby-mode . lsp)
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

;; treesitter stuff 
(use-package treesit
  :straight nil
  :init
  (setq treesit-language-source-alist
        '((bash        "https://github.com/tree-sitter/tree-sitter-bash")
          (css         "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile  "https://github.com/camdencheek/tree-sitter-dockerfile")
          (elixir      "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex        "https://github.com/phoenixframework/tree-sitter-heex")
          (html        "https://github.com/tree-sitter/tree-sitter-html")
          (javascript  "https://github.com/tree-sitter/tree-sitter-javascript")
          (json        "https://github.com/tree-sitter/tree-sitter-json")
          (markdown    "https://github.com/ikatyang/tree-sitter-markdown")
          (python      "https://github.com/tree-sitter/tree-sitter-python")
          (ruby        "https://github.com/tree-sitter/tree-sitter-ruby")
          (sql         "https://github.com/DerekStride/tree-sitter-sql")
          (toml        "https://github.com/tree-sitter/tree-sitter-toml")
          (typescript  "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx         "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (yaml        "https://github.com/ikatyang/tree-sitter-yaml"))))

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  ;; Methods/functions
  (define-key evil-outer-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.inner"))
  ;; Classes/modules
  (define-key evil-outer-text-objects-map "c"
              (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c"
              (evil-textobj-tree-sitter-get-textobj "class.inner"))
  ;; Blocks (do…end / {…})
  (define-key evil-outer-text-objects-map "b"
              (evil-textobj-tree-sitter-get-textobj "block.outer"))
  (define-key evil-inner-text-objects-map "b"
              (evil-textobj-tree-sitter-get-textobj "block.inner"))
  (define-key evil-outer-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer" "block.outer" "function.outer")))
  (define-key evil-inner-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner" "block.inner" "function.inner")))
  ;; Goto start of next function
  (define-key evil-normal-state-map
              (kbd "]f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;; Goto start of previous function
  (define-key evil-normal-state-map
              (kbd "[f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;; Goto end of next function
  (define-key evil-normal-state-map
              (kbd "]F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;; Goto end of previous function
  (define-key evil-normal-state-map
              (kbd "[F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

;; Combobulate on supported modes (JS/TS/TSX/HTML/JSON/YAML/TOML/Go, etc.)
(use-package combobulate
  :straight (combobulate
             :type git :host github :repo "mickeynp/combobulate"
             :files ("*.el"))           ;; ignore tests/docs during build
  :hook ((prog-mode . combobulate-mode))
  :custom (combobulate-key-prefix "C-c o"))

;; --- Evil-style bindings for common structural edits ------------------------
;; Use concrete command symbols where public; fall back to sending the default keys
;; Combobulate defaults (as of now):
;;   C-M-n / C-M-p  : next/prev sibling
;;   M-N / M-P      : drag node down/up
;;   M-<right>      : splice outer (“vanish parent”)
;;   M-h            : expand node selection (à la expand-region)
(defun my/combobulate-sibling-next () (interactive) (execute-kbd-macro (kbd "C-M-n")))
(defun my/combobulate-sibling-prev () (interactive) (execute-kbd-macro (kbd "C-M-p")))
(defun my/combobulate-drag-down   () (interactive) (execute-kbd-macro (kbd "M-N")))
(defun my/combobulate-drag-up     () (interactive) (execute-kbd-macro (kbd "M-P")))
(defun my/combobulate-splice-outer() (interactive) (execute-kbd-macro (kbd "M-<right>")))
(defun my/combobulate-expand      () (interactive) (execute-kbd-macro (kbd "M-h")))

;; If available in your checkout, prefer the public commands directly:
(when (fboundp 'combobulate-drag-up)   (defalias 'my/combobulate-drag-up   #'combobulate-drag-up))
(when (fboundp 'combobulate-drag-down) (defalias 'my/combobulate-drag-down #'combobulate-drag-down))
;; Expand region is typically `combobulate-mark-node-dwim`:
(when (fboundp 'combobulate-mark-node-dwim) (defalias 'my/combobulate-expand #'combobulate-mark-node-dwim))

;; Evil bindings (normal/visual)
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "]e") #'my/combobulate-sibling-next)
  (define-key evil-normal-state-map (kbd "[e") #'my/combobulate-sibling-prev)
  (define-key evil-visual-state-map (kbd "]e") #'my/combobulate-sibling-next)
  (define-key evil-visual-state-map (kbd "[e") #'my/combobulate-sibling-prev)

  (define-key evil-normal-state-map (kbd "]E") #'my/combobulate-drag-down)
  (define-key evil-normal-state-map (kbd "[E") #'my/combobulate-drag-up)
  (define-key evil-visual-state-map (kbd "]E") #'my/combobulate-drag-down)
  (define-key evil-visual-state-map (kbd "[E") #'my/combobulate-drag-up)

  (define-key evil-normal-state-map (kbd "gS")  #'my/combobulate-splice-outer) ; “splice (vanish parent)”
  (define-key evil-visual-state-map (kbd "gS")  #'my/combobulate-splice-outer)
  (define-key evil-normal-state-map (kbd "g=")  #'my/combobulate-expand) ; expand selection
  (define-key evil-visual-state-map (kbd "g=")  #'my/combobulate-expand))

;; ==============================
;; Software Development / LLMs and Gen AI
;; ==============================

(use-package gptel
  :commands (gptel gptel-request gptel-send gptel-menu)
  :init
  (setq gptel-api-key
        (let ((entry (car (auth-source-search :host "openai.com" :max 1 :require '(:key)))))
          (when entry
            (let ((api-key (plist-get entry :key)))
              (when (stringp api-key) api-key)))))
  (setq gptel-default-model "gpt-4o"
        gptel-temperature 0.7)
  :general
  (:keymaps 'gptel-mode-map
            :states '(normal insert visual motion)
            "C-<return>" 'gptel-send)  ;; Send input with Ctrl+Enter
  (tyrant-def
    "jg" '(gptel :which-key "Open GPT chat")
    "js" '(gptel-send :which-key "Send to GPT")
    "jm" '(gptel-menu :which-key "GPT Menu")))

;; Org AI integration with Org-mode
(use-package org-ai
  :straight (:host github :repo "rksm/org-ai"
             :files ("*.el" "README.md" "snippets"))
  :after org
  :commands (org-ai-mode org-ai-global-mode)
  :hook (org-mode . org-ai-mode)  ;; Enable org-ai in Org buffers
  :init
  (setq org-ai-auto-fill t)
  (setq org-ai-default-chat-model "gpt-4o"
        org-ai-openai-api-token (getenv "OPENAI_API_KEY")  ;; Ensure you set this in your environment
        org-ai-directory (expand-file-name "org-ai" org-directory))
  :config
  ;; Enable org-ai globally (optional, remove if you prefer manual activation)
  (org-ai-global-mode)
  (org-ai-install-yasnippets)
  ;; Keybindings under "oi" (Org AI)
  (with-eval-after-load 'general
    (tyrant-def
      "oi" '(:ignore t :which-key "org-ai")   ;; Org AI group
      ;; "oim" '(org-ai-mode :which-key "toggle org-ai")
      "oir" '(org-ai-on-region :which-key "AI on region")
      "ois" '(org-ai-summarize :which-key "summarize region")
      "oif" '(org-ai-refactor-code :which-key "AI refactor code")
      "oit" '(org-ai-prompt :which-key "AI prompt")
      "oip" '(org-ai-on-project :which-key "AI on project")
      "oit" '(org-ai-switch-chat-model :which-key "AI switch chat model")
      "oix" '(org-ai-explain-code :which-key "AI explain code")
      )))

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :commands (aidermacs-start aidermacs-send)
  :init
  ;; Use the provided code to retrieve the OpenAI API key
  (setq aidermacs-openai-api-token
        (let ((entry (car (auth-source-search :host "openai.com" :max 1 :require '(:key)))))
          (when entry
            (let ((api-key (plist-get entry :key)))
              (when (stringp api-key) api-key))))

        ;; Set the default model to o4-mini-high
        aidermacs-default-model "o4-mini-high"

        ;; Optional: Adjust temperature if desired
        aidermacs-temperature 0.7)
  :config
  ;; You can define additional settings here if required by the aidermacs package
  (setq aidermacs-history-max-length 50)  ;; Example: Keep a history of the last 50 interactions
  :general
  ;; Place both aidermacs and gptel keybindings under the SPACE-j prefix
  (:keymaps 'aidermacs-mode-map
            :states '(normal insert visual motion)
            "C-<return>" 'aidermacs-send)  ;; Send input with Ctrl+Enter

  ;; Define SPACE-j bindings for both aidermacs and gptel
  (tyrant-def
    "ja" '(aidermacs-transient-menu :which-key "Start Aider Menu")))

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

(use-package web-mode
  :mode
  (("\\.erb$" . web-mode)
   ("\\.html?" . web-mode))
  :init
  (setq web-mode-code-indent-offset 2
	      web-mode-css-indent-offset 2
	      web-mode-enable-css-colorization t
	      web-mode-markup-indent-offset 2
	      web-mode-script-padding 2
	      web-mode-style-padding 2))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  :bind
  (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; ==============================
;; org-mode
;; ==============================

(use-package org
  :straight t
  :defer t
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-directory "~/Documents/org")

  (setq org-agenda-files '("~/Documents/org/agenda.org"
                           "~/Documents/org/notes.org"
                           "~/Documents/org/ai-notes.org"
                           "~/Documents/org/tasks.org"
                           "~/Documents/org/ai-tasks.org"
                           "~/Documents/org/ai-learning-plan.org"
                           "~/Documents/org/journal.org"
                           "~/Documents/org/roam"))

  (setq org-log-done 'time)  ;; Log timestamp when a task is marked DONE
  (setq org-startup-indented t)  ;; Pretty indentation
  (setq org-insert-heading-respect-content t)
  (setq org-fold-catch-invisible-edits 'show-and-error)
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)

  ;; Enable syntax highlighting in code blocks
  (setq org-src-fontify-natively t)

  ;; Enable desired languages for code block evaluation
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (js         . t)
     (sql        . t)
     (ruby       . t)
     (css        . t)
     (makefile   . t)
     (mermaid    . t)
     (shell      . t)))

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

          ;; Fitness stuff
          ("w" "💪 Daily Workout Log"
           entry
           (file+datetree "~/Documents/org/roam/fitness/workout-log.org")
           "* %<%Y-%m-%d> Daily Workout\n\n%?"
           :empty-lines 1)
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

(use-package consult-org
  :after (org consult)
  :straight nil
  :commands (consult-org-heading consult-org-agenda))

(use-package org-ql
  :after org)

(defun my/org-ql-ai-notes ()
  "Display second-level AI-related notes from ai-notes.org using org-ql."
  (interactive)
  (org-ql-search
   (append (list "~/Documents/org/ai-notes.org")
           (directory-files "~/Documents/org/roam/" t "\\.org$"))
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

(use-package org-roam
  :straight t
  :after org
  :commands (org-roam-buffer-toggle
             org-roam-node-find
             org-roam-node-insert
             org-roam-capture)
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
        '(("r" "research note" plain "* ${my-title}\n\n%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${my-slug}.org"
                              "#+title: ${my-title}\n")
           :unnarrowed t)
          ("a" "AI research note" plain "* ${my-title}\n\n%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${my-slug}.org"
                              "#+title: ${my-title}\n#+filetags: ai\n")
           :unnarrowed t
           :tags "ai")
          ))
  )

(use-package consult-org-roam
   :after (consult org-roam)
   :commands (consult-org-roam-file-find
              consult-org-roam-backlinks
              consult-org-roam-backlinks-recursive
              consult-org-roam-forward-links
              consult-org-roam-search)
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-."))

(use-package ob-mermaid
  ;; :defer t
  :after (org general)
  :init
  ;; Path to Mermaid CLI (via npm)
  (setq ob-mermaid-cli-path "~/.nvm/versions/node/v20.14.0/bin/mmdc")
  (setq org-babel-mermaid-cli-args '("-t" "default" "-o" "svg"))

  :hook (org-babel-before-execute . my/org-babel-setup-mermaid-output-dir)

  :config
  (defun my/org-babel-setup-mermaid-output-dir ()
    "Set `org-babel-mermaid-output-dir` relative to Org file and create it if missing."
    (when (and buffer-file-name (derived-mode-p 'org-mode))
      (let* ((dir (expand-file-name "img" (file-name-directory buffer-file-name))))
        (setq-local org-babel-mermaid-output-dir dir)
        (unless (file-directory-p dir)
          (make-directory dir t)))))

  ;; ✅ Safely register Mermaid with existing org-babel languages
  ;; (with-eval-after-load 'org
  ;;   (let ((existing org-babel-load-languages))
  ;;     (cl-pushnew '(mermaid . t) existing :test #'equal)
  ;;     (org-babel-do-load-languages 'org-babel-load-languages existing)))

  ;; Org-style keybindings using general
  (general-define-key
   :keymaps 'org-mode-map
   "C-c C-v m" #'org-babel-execute-src-block
   "C-c C-v o" #'org-open-at-point))

;; ==============================
;; org geopolitics briefing
;; ==============================
;; (require 'org-id)

(defvar my/geopolitics-dir "~/Documents/org/roam/geopolitics/daily-briefs")
(defvar my/geopolitics-index-file (expand-file-name "global-geopolitics-briefs.org" my/geopolitics-dir))

(defun my/auto-link-keywords (text)
  "Link key entities like China, Russia, etc. using Org-roam-style [[...]] links."
  (replace-regexp-in-string
   "\\b\\(China\\|Russia\\|United States\\|EU\\|Middle East\\|Red Sea\\|energy flows\\|financial markets\\|Ukraine\\|Taiwan\\)\\b"
   "[[\\1]]"
   text t))

(defun my/auto-link-named-entities (text callback)
  "Use GPT via gptel to extract and auto-link named entities in TEXT, then pass to CALLBACK."
  (let ((prompt (concat
                 "Given the following text, identify named entities (countries, organizations, regions, key concepts), "
                 "and return the text with each named entity replaced by an Org-mode-style link like [[Entity]]. "
                 "Don't modify anything else.\n\nText:\n" text)))
    (gptel-request
     prompt
     :callback callback)))

(defun my/insert-daily-geopolitics-brief (linked-text &rest _)
  "Insert LINKED-TEXT as a new dated heading in the monthly geopolitics org-roam file."
  (let* ((date-str (format-time-string "%Y-%m-%d"))
         (month-str (format-time-string "%Y-%m"))
         (file-name (concat month-str "-global-geopolitics-brief.org"))
         (file-path (expand-file-name file-name my/geopolitics-dir))
         (brief-title (concat date-str " - Global Geopolitics Brief"))
         (monthly-link (concat "[[" file-name "][" month-str "]]")))
    ;; Insert daily brief in monthly file
    (with-current-buffer (find-file-noselect file-path)
      (goto-char (point-max))
      (insert (format "* %s\n:PROPERTIES:\n:ID: %s\n:ROAM_TAGS: geopolitics\n:END:\n\n"
                      brief-title (org-id-new)))
      (insert linked-text "\n\n")
      (save-buffer))
    ;; Ensure monthly file is linked from index
    (with-current-buffer (find-file-noselect my/geopolitics-index-file)
      (goto-char (point-min))
      (unless (re-search-forward (regexp-quote monthly-link) nil t)
        (goto-char (point-max))
        (re-search-backward "^\\* Index" nil t)
        (forward-line)
        (insert (concat "- " monthly-link "\n"))
        (save-buffer)))))

(defun my/ensure-geopolitics-index ()
  "Create the global geopolitics index file if it doesn't exist."
  (unless (file-exists-p my/geopolitics-index-file)
    (let ((buffer (find-file-noselect my/geopolitics-index-file)))
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert "#+title: Global Geopolitics\n#+filetags: :geopolitics:index:\n\n* Index\n\n* Monthly Summaries\n")
        (save-buffer)))))

(defun my/generate-geopolitics-brief ()
  "Generate the daily geopolitics brief using gptel and insert it."
  (interactive)
  (my/ensure-geopolitics-index)
  (let ((prompt "Please summarise all of the important news and current events in the last 24 hours, especially with respect to ongoing conflicts in Asia, Europe, and the Middle East, especially with respect to global trade and energy flows, movements in financial and money markets, and especially with respect to the actions and activities of China, Russia, the United States, and the EU. I'm also interested in reading any important statements or communications made by representatives of the major powers with respect to any of the issues we are discussing. Provide a detailed Org-mode outline in plain text (do not include code fences or triple backticks). Do not include a top-level heading since the summary will be inserted under an existing date heading; instead, start with level-2 headings for each section. Go to level-3 or beyond as necessary. Include links to your sources next to each summary."))
    (gptel-request
     prompt
     :callback (lambda (response &rest _)
                 (message "Raw response: %s" response)
                 (my/auto-link-named-entities
                  response
                  #'my/insert-daily-geopolitics-brief)))))

(defun my/generate-monthly-geopolitics-summary ()
  "Generate an AI summary of the current month's geopolitical briefs and update the global index."
  (interactive)
  (my/ensure-geopolitics-index)
  (let* ((month-str (format-time-string "%Y-%m"))
         (month-file-name (concat month-str "-global-geopolitics-brief.org"))
         (month-file-path (expand-file-name month-file-name my/geopolitics-dir))
         (index-buffer (find-file-noselect my/geopolitics-index-file))
         (prompt-base "Please read the following global geopolitical briefs from the past month. Summarise the major developments, emerging trends, and turning points. Present the summary in bullet points under appropriate headings. Be concise, analytical, and neutral.\n\n---\n\n")
         (month-content (with-temp-buffer
                          (insert-file-contents month-file-path)
                          (buffer-string)))
         (prompt (concat prompt-base month-content)))
    (gptel-request
     prompt
     :callback (lambda (response &rest _)
                 (with-current-buffer index-buffer
                   (goto-char (point-min))
                   (re-search-forward "^\\* Monthly Summaries")
                   (unless (re-search-forward (regexp-quote month-str) nil t)
                     (insert (format "\n** %s\n[[%s][%s]]\n\n%s\n"
                                     month-str month-file-name month-str response))
                     (save-buffer)
                     (message "Monthly summary added for %s" month-str)))))))

(defun my/show-liturgical-season ()
  (interactive)
  (org-ql-search "~/Documents/org/roam/mysticism-and-philosophy/seasonal-prayer-rotation.org"
    '(and (property "FROM")
          (property "TO")
          (let* ((from (org-entry-get nil "FROM"))
                 (to   (org-entry-get nil "TO"))
                 (today (format-time-string "[%Y-%m-%d]")))
            (and (string<= from today) (string<= today to))))
    :title "Today's Liturgical Season"))

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
  (if (not buffer-file-name)
      (message "Buffer is not visiting a file.")
    (let ((default-directory
           (if (string= (file-name-extension buffer-file-name) "tf")
               (expand-file-name "aws/terraform/environments"
                                 default-directory) default-directory)))
      (call-interactively #'compile))))

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
   '(dumb-jump lsp-mode eglot flycheck-eglot direnv yaml-mode dockerfile-mode add-node-modules-path all-the-icons-completion all-the-icons-dired cape consult corfu doom-modeline doom-themes embark embark-consult evil-collection evil-escape exec-path-from-shell flycheck general git-timemachine magit marginalia orderless php-mode rvm terraform-mode vertico which-key writeroom-mode))
 '(safe-local-variable-values
   '((flycheck-javascript-eslint-args "--no-eslintrc")
     (eval let*
           ((pr
             (project-current))
            (root
             (if pr
                 (project-root pr)
               default-directory))
            (host-root
             (directory-file-name
              (file-truename root))))
           (setq-local flycheck-javascript-eslint-executable
                       (expand-file-name "~/bin/eslint-in-docker"))
           (setq-local process-environment
                       (append
                        (list "ESLINT_SERVICE=web" "ESLINT_WORKDIR=/usr/src/app"
                              (concat "HOST_ROOT=" host-root))
                        process-environment))
           (setq-local flycheck-substitute-paths
                       (list
                        (cons "/usr/src/app" host-root)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
