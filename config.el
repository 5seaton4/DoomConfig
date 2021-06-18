;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setenv "LC_CTYPE" "UTF-8")

(setq-default indent-tabs-mode nil)  ; I prefer spaces
(setq-default tab-width 2)
(setq-default vc-follow-symlinks t)
(setq-default create-lockfiles t)
(setq-default uniquify-buffer-name-style 'forward)
(setq-default window-combination-resize t) ;take new window space from all other windows (not just current)
(setq-default column-number-mode t)        ;; column number
(electric-indent-mode -1)                  ;; indent

(set-default 'cursor-type  '(bar . 5))
(blink-cursor-mode 1)
(set-cursor-color "#fdff00")

;TODO configure backups more.
(setq make-backup-files t)               ;; disable auto-backup
(setq auto-save-default nil)               ;; disable autosave

(show-paren-mode t)                        ;; parenthesis
(setq show-paren-delay 0)
(setq-default line-spacing 0.2)            ;; line space

;TODO does this work??
(windmove-default-keybindings)             ;; Let me switch windows with shift-arrows instead of "C-x o" all the time

(global-font-lock-mode 1)                  ;; syntax highlighting
(tooltip-mode 1)                           ;; tool tip mode on

(set-face-background 'vertical-border (face-background 'mode-line))
(set-face-foreground 'vertical-border (face-background 'mode-line))
(setq-default show-trailing-whitespace 't)
(setq-default indicate-empty-lines 't)
;; mark-ring is too big and unwieldy
;; TODO: 'unpop' [https://stackoverflow.com/a/14539202] could be useful?
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)
(setq backup-directory-alist `(("." . "~/.doom.d/saves")))

;; shift+arrow for window moving
(windmove-default-keybindings)
;; Jump to the last position on file open
(save-place-mode 1)
;; Make column number start at 1
(setq column-number-indicator-zero-based nil)

(use-package rainbow-delimiters
:ensure t)
(use-package rainbow-blocks
:ensure t)

(use-package hl-todo
:ensure t
:config
(setq hl-todo-keyword-faces
        '(("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF"))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jack Seaton"
      user-mail-address "seaton_jack@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq-default org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq-default display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;Load my custom dashboard config
(load! "custom/dashboard.el")
;(load! "custom/random.el")

;Enable the menu bar.
;(menu-bar-mode 1)
;Raise undo limit to 80Mb
(setq-default undo-limit 80000000
      evil-want-fine-undo t) ;By default while in insert all changes are one big blob. Be more granular.
(display-time-mode 1) ;enable time in mode line.
(menu-bar-mode -1)                         ;; hide menu bar
(tool-bar-mode -1)                         ;; hide tool bar
(scroll-bar-mode -1)                       ;; hide scroll bar

;; No fringe but nice glyphs for truncated and wrapped lines
;(fringe-mode '(0 . 0))
(defface fallback '((t :family "Fira Code Light")) "Fallback")
;; (set-display-table-slot standard-display-table 'truncation
;;                         (make-glyph-code ?… 'fallback))
;; (set-display-table-slot standard-display-table 'wrap
;;                         (make-glyph-code ?↩ 'fallback))

;; No sound
(setq-default visible-bell t)
(setq-default ring-bell-function 'ignore)

(if (equal "Battery status not available"
           (battery))
    (display-battery-mode 1)                        ; On laptops it's nice to know how much power you have
  (setq password-cache-expiry nil))

(global-subword-mode 1)                           ; Iterate through CamelCase words

(add-to-list 'default-frame-alist '(height . 24))   ; set the default size of frames.
(add-to-list 'default-frame-alist '(width . 80))

(map!
      ;; Swapping windows
      "C-<left>"       #'split-window-right
      "C-<down>"       #'split-window-vertically
      "C-<up>"         #'split-window-below
      "C-<right>"      #'split-window-horizontally)

(setq-default history-length 1000) ;The maximum length of the minibuffer history.
(setq-default prescient-history-length 1000) ;The number of recently selected candidates that are displayed at the top of the list.

(setq which-key-idle-delay 0.5) ;; I need the help, I really do

;This will remove the evil prefix from every command in which-key
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange")) ;Set the modified buffer colour to orange instead of red.

;Set some delays on company
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;Add in tabnine to company autocomplete
(setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
(after! company
  (setq company-idle-delay 0
        company-show-numbers t))

;https://medium.com/urbint-engineering/emacs-doom-for-newbies-1f8038604e3b

;See whitespace in a file
;TODO have emacs highlight white space
;(global-whitespace-mode 1)

;Use L to go to end of line.
(map!
    :desc (documentation 'evil-end-of-line)
    :n "L"  #'evil-end-of-line)
;Use H to go to the first non-blank in a line.
(map!
    :desc (documentation 'evil-first-non-blank)
    :n "H"  #'evil-first-non-blank)

;Wrap lines in all visual modes.
(global-visual-line-mode t)

;enable undo-tree for every buffer.
(global-undo-tree-mode t)

;; Set initial frame size and position
(defun my/set-initial-frame ()
  (let* ((base-factor 0.85)
	(a-width (* (display-pixel-width) base-factor))
        (a-height (* (display-pixel-height) base-factor))
        (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
	(a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))
(setq frame-resize-pixelwise t)
(my/set-initial-frame)

;This will allow me to select a buffer when I split my screen.
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

;only display encoding in mode-line if t is not utf-8
(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)


(advice-add 'evil-ex-search-next :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
(advice-add 'evil-ex-search-previous :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))

;highlight file edits in red.
;(global-highlight-changes-mode 1)
;Highlight VC changes.
(global-diff-hl-mode 1)
;remove the highlighting on file save.
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when highlight-changes-mode
;;               (save-restriction
;;                 (widen)
;;          (highlight-changes-remove-highlight (point-min) (point-max))))))

;Open up a diff when saving files
(defun my-diff ()
  (let ((temp-file (make-temp-file "gpgdiff-"))
    (contents (buffer-string))
    (current-file (buffer-file-name)))
    (with-temp-file temp-file
      (insert contents))
    (diff current-file temp-file))

  (unless (y-or-n-p "Save buffer?")
    (keyboard-quit)))

(add-hook 'before-save-hook 'my-diff t t)

(setq treemacs-show-hidden-files t)
;; (treemacs-follow-mode t)
;; (treemacs-filewatch-mode t)
;; (treemacs-fringe-indicator-mode t)

;TODO Have treemacs close window when file is opened.

(evil-visual-mark-mode 1)

;; Show the current function name in the header line
(which-function-mode)
 (setq-default header-line-format
               '((which-func-mode ("" which-func-format " "))))
 (setq mode-line-misc-info
;;             ;; We remove Which Function Mode from the mode line, because it's mostly
;;             ;; invisible here anyway.
             (assq-delete-all 'which-func-mode mode-line-misc-info))


;TODO List
;
;; - Intellisense
;; - Syntax highlighting.
;; - Errors
;; - Warnings
;; - Theme?
;; - Word searching.
;; - Preview Tag
;; - Startify
;; - Marks?
;; - Window Resizing.
;; - Window creation.
;; - Scratch Buffer
;; - Org Mode.
;; - Buffer bar.
;; - Move Buffers / Paste Buffers.
;; - Sudo permissions
;; - Select nearest object - wildfire (space)
;; - Todo org mode
;; - Rainbow bracket highlighting
;; - Bracket completion.
;; - Show lines which have been edited.
;; - Better indenting (if needed)
;; - Snippets.
;; - highlight regex as you search
;; - SQL completion and highlighting.
;; - Buffer resizing

; settings
;; - autoindenting
;; - copyindent
;; - virtualedit=all
;; - clipboard?
;; - wrapscan
;; - ignorecase
;; - smartcase (use case if any are used)
