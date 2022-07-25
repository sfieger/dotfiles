;; Absolute Basics
(setq inhibit-startup-message t ; Don't show the splash screen
      visible-bell nil)         ; Don't flash, ring the bell


;; Turn off unneeded UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Display line numbers in every buffer and other UI stuff
(global-display-line-numbers-mode 1)
(hl-line-mode 1)
(blink-cursor-mode 1)


;; Must-Have settings
;; Remember recently edited files
(recentf-mode 1)

;; Remeber minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)

;; Remember last cursor position in file
(save-place-mode 1)

;; File for custom variables
(setq custom-file (locate-user-emacs-file "~/.config/emacs/custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Auto-Revert buffers on external change
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)


;; Configure the Modus Themes' appearance
(setq modus-themes-mode-line '(accented borderless)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions '((t background intense accented))
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings t
      modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
        (2 . (rainbow background 1.3))
        (3 . (rainbow bold 1.2))
        (t . (semilight 1.1))))

;; Load the dark theme by default
(load-theme 'modus-vivendi t)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)