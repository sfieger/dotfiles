;; don't show the splash screen
(setq inhibit-startup-message t)

;; turn off unneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; other UI stuff
(blink-cursor-mode 1)
(global-hl-line-mode 1)

(global-display-line-numbers-mode 1)

(global-whitespace-mode -1)
(setq whitespace-style
      '(face lines-tail tabs tab-mark spaces space-mark trailing newline newline-mark))

(setq whitespace-display-mappings
      '((space-mark   ?\    [?\ ]     [?\ ])    ; space
        (space-mark   ?\xA0 [?\ ]     [?\ ])    ; hard space
        (newline-mark ?\n   [?§ ?\n] [?§ ?\n])  ; end-of-line
        (tab-mark     ?\t   [?> ?\t] [?> ?\t])  ; tab
        ))

(global-set-key (kbd "<f6>") 'whitespace-mode)

;; color theme
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)

(set-frame-parameter nil 'background-mode 'light)
(set-terminal-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))

(defun my-toggle-bg ()
  (interactive)
  (if (eq (frame-parameter nil 'background-mode) 'dark)
    (set-frame-parameter nil 'background-mode 'light)
    (set-frame-parameter nil 'background-mode 'dark))
  (if (eq (terminal-parameter nil 'background-mode) 'dark)
    (set-terminal-parameter nil 'background-mode 'light)
    (set-terminal-parameter nil 'background-mode 'dark))
  (enable-theme 'solarized))

(global-set-key (kbd "<f5>") 'my-toggle-bg)

;; useful settings
(recentf-mode  1)

(savehist-mode 1)

(save-place-mode 1)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq use-dialog-box nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
