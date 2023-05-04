;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ethan Smith"
      user-mail-address "ethansmith.dev@gmail.com")

;; pull changes from remote directory.
(let ((dot-files-dir "~/dot_files/"))
  (require 'magit)
  (cd dot-files-dir)
  (magit-fetch-from-upstream "https://github.com/ethanxxxl/dot_files" ""))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-molokai)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 17 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; LSP configuration
(setq gc-cons-threshold 1600000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-delay 0.5)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)

;(add-hook 'lsp-mode-hook 'lsp-headerline-breadcrumb-mode)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
(setq lsp-headerline-breadcrumb-icons-enable t)

(add-hook 'rjsx-mode-hook
          (lambda ()
            "sets up auto-complete for screeps projects.

Checks if rjsx-mode is being run in a screeps project. If so,
then copy over the autocomplete files to the current directory.

there is a potential problem, where, if there are multiple
subdirectories, the autocomplete files will be copied over
multiple times. this may or may not be an issue."

            ; check if we are in a screeps project
            (if (and (string-match "Screeps/scripts"
                                   (file-name-directory (buffer-file-name)))
                     (not (file-exists-p "ScreepsAutocomplete/")))
                ;; we are in a screeps direcotry without .tern_mode
                (copy-directory
                 "~/Library/Application Support/Screeps/scripts/ScreepsAutocomplete/"
                 (file-name-directory (buffer-file-name))))))

(load! "extra/avr-asm-autodoc-mode.el")
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

(use-package lsp-grammarly
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred

(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-doc-delay 0.25)
(setq lsp-ui-doc-use-webkit nil)
(setq lsp-ui-doc-glance t)


(defun ethan-book-mode ()
  (interactive)
  (setq size 100)
  "set a variable pitch font, and fixes the window size to size,
and centers the window."

  (let ((margin-size (/ (- (window-total-width) size) 2)))
    (set-window-margins (selected-window) margin-size margin-size)
    (variable-pitch-mode)
    (set-face-attribute  'line-number nil
                         :family "Ubuntu Mono")
    (set-face-attribute 'line-number-current-line nil
                        :family "Ubuntu Mono")
    (set-face-attribute 'variable-pitch nil
                        :family "Bookerly")
    (set-face-attribute 'org-level-1 nil
                        :family "Source Code Pro" :height 1.4)
    (set-face-attribute 'org-level-2 nil
                        :family "Source Code Pro" :height 1.2)
    (set-face-attribute 'org-level-3 nil
                        :family "Source Code Pro" :height 1.1)
    (setq line-spacing 10)))

(defun ethan-disable-book-mode ()
  "resets state to the way it was before book mode was enabled."
  (interactive)
  (set-window-margins (selected-window) 0 0)
  (variable-pitch-mode -1)

  (set-face-attribute 'line-number nil
                      :family "Monospace")
  (set-face-attribute 'line-number-current-line nil
                      :family "Monospace")
  (set-face-attribute 'variable-pitch nil
                      :family nil)
  (set-face-attribute 'org-level-1 nil
                      :family nil :height nil)
  (set-face-attribute 'org-level-2 nil
                      :family nil :height nil)
  (set-face-attribute 'org-level-3 nil
                      :family nil :height nil)
  (setq line-spacing nil))

;; NOTE: you need to set variable-pitch mode first

(defun ethan-hex-highlight ()
  (font-lock-add-keywords nil '(;; Valid hex number (will highlight invalid suffix though)
                                ("\\b0x[[:xdigit:]]+[uUlL]*\\b" . font-lock-string-face)
                                ;; Invalid hex number
                                ("\\b0x\\(\\w\\|\\.\\)+\\b" . font-lock-warning-face)
                                ;; Valid binary number (will highlight invalid suffix though)
                                ("\\b0b[[:xdigit:]]+[uUlL]*\\b" . font-lock-string-face)
                                ;; Invalid binary number
                                ("\\b0b\\(\\w\\|\\.\\)+\\b" . font-lock-warning-face)
                                ;; Valid floating point number.
                                ("\\(\\b[0-9]+\\|\\)\\(\\.\\)\\([0-9]+\\(e[-]?[0-9]+\\)?\\([lL]?\\|[dD]?[fF]?\\)\\)\\b"
                                 (1 font-lock-string-face) (3 font-lock-string-face))
                                ;; Invalid floating point number.  Must be before valid decimal.
                                ("\\b[0-9].*?\\..+?\\b" . font-lock-warning-face)
                                ;; Valid decimal number.  Must be before octal regexes otherwise 0 and 0l
                                ;; will be highlighted as errors.  Will highlight invalid suffix though.
                                ("\\b\\(\\(0\\|[1-9][0-9]*\\)[uUlL]*\\)\\b" 1 font-lock-string-face)
                                ;; Valid octal number
                                ("\\b0[0-7]+[uUlL]*\\b" . font-lock-string-face)
                                ;; Floating point number with no digits after the period.  This must be
                                ;; after the invalid numbers, otherwise it will "steal" some invalid
                                ;; numbers and highlight them as valid.
                                ("\\b\\([0-9]+\\)\\." (1 font-lock-string-face))
                                ;; Invalid number.  Must be last so it only highlights anything not
                                ;; matched above.
                                ("\\b[0-9]\\(\\w\\|\\.\\)+?\\b" . font-lock-warning-face))))

(add-hook 'asm-mode-hook 'ethan-hex-highlight)
(add-hook 'rustic-mode-hook 'ethan-hex-highlight)
