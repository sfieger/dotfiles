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
;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Remember recently edited files
(recentf-mode 1)

;; Remeber minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)
(setq history-delete-duplicates t)

;; File for custom variables
(setq custom-file (locate-user-emacs-file "~/.config/emacs/custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Auto-Revert buffers on external change
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; German Calendar
(setq-default calendar-week-start-day 1
          calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                                       "Donnerstag" "Freitag" "Samstag"]
          calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                     "Juni" "Juli" "August" "September"
                     "Oktober" "November" "Dezember"])
(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))
;; German Holiyas
(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))
;; for NW
(setq holiday-christian-holidays
      '(;; (holiday-fixed 1 6 "Heilige Drei Könige") ;; BW, BY, ST
    (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam") ;; BW, BY, HE, NW, RP, SL, (SN), (TH)
    ;; (holiday-fixed 8 15 "Mariä Himmelfahrt") ;; (BY), SL
        ;; (holiday-fixed 11 31 "Reformationstag") ;; BB, HB, HH, MV, NI, SN, ST, SH TH
    (holiday-fixed 11 1 "Allerheiligen") ;; BW, BY, NW, RP, SL
        ;; (holiday-float 11 3 1 "Buss- und Bettag" 16) ;; SN 
    (holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
    ))
(setq holiday-local-holidays
      '(;; (holiday-fixed 3 8 "Frauentag") ;; BE, MV
        (holiday-easter-etc -48 "Rosenmontag")
    ;; (holiday-fixed 8 8 "Augsburger Hohes Friedensfest") ;; BY
    ;; (holiday-fixed 9 20 "Weltkindertag") ;; TH
    ;; (holiday-fixed 11 11 "Elfter im Elften")
    (holiday-fixed 12 24 "Heiligabend")
    (holiday-fixed 12 31 "Silvester")
    ))
(setq holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil
      holiday-solar-holidays nil)


;; Package Manager
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
             ("org" . "https://orgmode.org/elpa/")
             ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'bind-key)
(use-package diminish)


;; Helper
;; Check if system is Microsoft Windows
(defun my-system-type-is-windows ()
  "Return true if system is Windows-based (at least up to Win7)"
  (string-equal system-type "windows-nt"))

;; Check if system is GNU/Linux
(defun my-system-type-is-gnu ()
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux"))


;; Which Key
(use-package which-key
  :ensure t
  :defer 120
  :diminish
  :config
  (which-key-setup-side-window-right)
  (setq which-key-idle-delay 1)
  (which-key-mode))

;; Ivy, Counsel, Swiper
(use-package ivy
  :ensure t
  :diminish
  :config
  (defcustom ivy-use-group-face-if-no-groups t
    "If t, and the expression has no subgroups, highlight whole match as a group.
    It will then use the second face (first of the \"group\" faces)
    of `ivy-minibuffer-faces'.  Otherwise, always use the first face
    in this case."
    :type 'boolean)
  (ivy-mode 1))
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(use-package counsel
  :ensure t
  :diminish
  :config
  (counsel-mode))

(use-package swiper)
(global-set-key (kbd "C-s") 'swiper)


;; General key bindings
(global-set-key (kbd "M-l") 'downcase-word)
(global-set-key (kbd "M-u") 'upcase-word)
(global-set-key (kbd "M-c") 'capitalize-word)


;; Org-Mode
;; Key Bindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Capture-Templates
(setq org-capture-templates
      '(("b" "Bookmark" entry (file+headline "~/org/misc.org" "Bookmarks")
     "* %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n")
    ("c" "Contact" entry (file+headline "~/org/misc.org" "Contacts")
     "* %?\n:PROPERTIES:\n:TITLE: \n:BIRTHDAY: %^t\n:PHONE: \n:MOBILE: \n:EMAIL: \n:STREET: \n:POSTALCODE: \n:CITY: \n:COUNTRY: \n:COMPANY: \n:WORKPHONE: \n:WORKMAIL: \n:CREATED: %u\n:END:\n")
        ("t" "TODO [inbox]" entry (file+headline "~/org/gtd/gtd.org" "Inbox")
     "* TODO %i%?" :prepend 1)
    ("T" "Tickler" entry (file+headline "~/org/gtd/gtd.org" "Inbox")
     "* %^t %i%?" :prepend 1)))
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))

;; Agenda
(setq org-agenda-files '("~/org/gtd/gtd.org"
             "~/org/misc.org"))
(setq org-agenda-include-diary t)
(setq org-agenda-prefix-format
      '((todo . "%b %i %-12:c")))
(setq org-agenda-custom-commands
      '(("D" "Daily Review"
     ((agenda "" ((org-agenda-span 7)))
      (todo "TODO")
      (todo "WAITING")))
    ("W" "Weekly Review"
         ((agenda "" ((org-agenda-span 14)))
          (todo "WAITING")
          (todo "SOMEDAY")
      (stuck "" ((org-agenda-files '("~/org/gtd/gtd.org"))))))
    ))


;; Spelling
(if (my-system-type-is-windows)
    (setq ispell-program-name "C:/Program Files/Git/usr/bin/aspell.exe"))

(require 'ispell)
(setq ispell-silently-savep t)
(setq ispell-dictionary "german")

(let ((langs '("german" "american")))
      (setq lang-ring (make-ring (length langs)))
      (dolist (elem langs) (ring-insert lang-ring elem)))

(defun my-toggle-ispell-language ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(if (file-exists-p "~/.config/emacs/thesaurus/openthesaurus.txt")
  (use-package synonyms
    :load-path  (lambda () (expand-file-name "~/.config/emacs/contrib/synonyms/"))
    :init ;; executed before loading package
    (setq synonyms-file        "~/.config/emacs/thesaurus/openthesaurus.txt")
    (setq synonyms-cache-file  "~/.config/emacs/thesaurus/vkcachefile")
    :config
    (defun my-synonym-current-word ()
      "Lookup synonyms for current word."
      (interactive)
      (synonyms-lookup (thing-at-point 'word) nil nil))
  )
(message ("»»» I could not locate \"~/.config/emacs/thesaurus/mthesaur.txt\""))
)

(use-package define-word
  :ensure t
)

;; Hydras
;; F-Key Settings for Hydras
(setq my-f-key-settings (concat
" ⇧               |                   |
"                                                                  (propertize
" F1  F2  F3  F4  | F5    F6  F7  F8  | F9       F10  F11      F12
"                                                                  'face '(:foreground "#859900"))
"                 | Theme             | Spelling Menu maximize

"))

(use-package hydra
  :ensure t
  :defer 90
  :config

  ;; apropos training
  (defhydra hydra-apropos (:color blue
                           :hint nil)
    "
  _a_propos        _c_ommand
  _d_ocumentation  _l_ibrary
  _v_ariable       _u_ser-option
  ^ ^       valu_e_"
    ("a" apropos)
    ("d" apropos-documentation)
    ("v" apropos-variable)
    ("c" apropos-command)
    ("l" apropos-library)
    ("u" apropos-user-option)
    ("e" apropos-value))
  (global-set-key (kbd "C-c h") 'hydra-apropos/body)

  ;; spelling
  (defhydra hydra-spelling (:color red)
    (concat my-f-key-settings
  "
  ^
  ^Spelling^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^───────
  check _r_egion      _n_ext error        _m_ode
  check _b_uffer      _c_orrect           toggle _l_anguage
  _s_ynonym
  _D_efine word
  ^^                  ^^                  ^^
  ")
  ("q" nil "quit")
  ("r" flyspell-region nil)
  ("l" my-toggle-ispell-language nil)
  ("n" flyspell-goto-next-error nil)
  ("s" my-synonym-current-word nil :color blue)
  ("D" define-word-at-point nil :color blue) ;; define-word
  ("c" flyspell-correct-word-before-point nil)
  ("b" flyspell-buffer nil)
  ("m" flyspell-mode nil)
  )
  (global-set-key (kbd "<f9>") 'hydra-spelling/body))


;; Modus Themes 4.1.0 configuration
(add-to-list 'load-path "~/.config/emacs/modus-themes")
(require 'modus-themes)

(setq modus-themes-headings
      '((1 . (rainbow overline background 1.4))
        (2 . (rainbow background 1.3))
        (3 . (rainbow bold 1.2))
        (t . (semilight 1.1))))

;; Complete Solarized Overhaul for Modus-Themes
(setq modus-vivendi-palette-overrides
      '((bg-main "#002b36")
        (bg-dim "#073642")
        (fg-main "#839496")
        (fg-dim "#93a1a1")
        (fg-alt "#586e75")
        (bg-active "#002b36")
        (bg-inactive "#073642")
        (border "#839496")
        (red "#dc322f")
        (red-warmer "#ec433b")
        (red-cooler "#cc1e24")
        (red-faint "#fc5246")
        (red-intense "#bd001a")
        (green "#859900")
        (green-warmer "#93a707")
        (green-cooler "#778c00")
        (green-faint "#a1b420")
        (green-intense "#6a7f00")
        (yellow "#b58900")
        (yellow-warmer "#c39616")
        (yellow-cooler "#a67c00")
        (yellow-faint "#d2a328")
        (yellow-intense "#987000")
        (blue "#268bd2")
        (blue-warmer "#3b98e0")
        (blue-cooler "#007fc4")
        (blue-faint "#4da6ee")
        (blue-intense "#0072b6")
        (magenta "#d33682")
        (magenta-warmer "#e2468f")
        (magenta-cooler "#c42376")
        (magenta-faint "#f1559c")
        (magenta-intense "#b50769")
        (cyan "#2aa198")
        (cyan-warmer "#3cafa5")
        (cyan-cooler "#12948b")
        (cyan-faint "#4dbdb3")
        (cyan-intense "#00877e")
        (rust "#ffb174")
        (gold "#ffd158")
        (olive "#d1e354")
        (slate "#92e2ff")
        (indigo "#d2d3ff")
        (maroon "#ffa189")
        (pink "#ffa3e6")
        (bg-red-intense "#8e0000")
        (bg-green-intense "#2e4200")
        (bg-yellow-intense "#553500")
        (bg-blue-intense "#004280")
        (bg-magenta-intense "#880046")
        (bg-cyan-intense "#004842")
        (bg-red-subtle "#7f0000")
        (bg-green-subtle "#263700")
        (bg-yellow-subtle "#4a2a00")
        (bg-blue-subtle "#003773")
        (bg-magenta-subtle "#7a003a")
        (bg-cyan-subtle "#003c37")
        (bg-red-nuanced "#700000")
        (bg-green-nuanced "#202c00")
        (bg-yellow-nuanced "#401f00")
        (bg-blue-nuanced "#002c67")
        (bg-magenta-nuanced "#6b002f")
        (bg-cyan-nuanced "#00302c")
        (bg-ochre "#640000")
        (bg-lavender "#012167")
        (bg-sage "#202c00")
        (bg-graph-red-0 "#dc322f")
        (bg-graph-red-1 "#bd001a")
        (bg-graph-green-0 "#859900")
        (bg-graph-green-1 "#6a7f00")
        (bg-graph-yellow-0 "#b58900")
        (bg-graph-yellow-1 "#987000")
        (bg-graph-blue-0 "#268bd2")
        (bg-graph-blue-1 "#0072b6")
        (bg-graph-magenta-0 "#d33682")
        (bg-graph-magenta-1 "#b50769")
        (bg-graph-cyan-0 "#2aa198")
        (bg-graph-cyan-1 "#00877e")
        (bg-completion "#012167")
        (bg-hover "#273681")
        (bg-hover-secondary "#553500")
        (bg-hl-line "#073642")
        (bg-region "#fdf6e3")
        (fg-region "#657b83")
        (bg-char-0 "#003c37")
        (bg-char-1 "#7a003a")
        (bg-char-2 "#263700")
        (bg-mode-line-active "#002b36")
        (fg-mode-line-active "#839496")
        (border-mode-line-active "#839496")
        (bg-mode-line-inactive "#073642")
        (fg-mode-line-inactive "#93a1a1")
        (border-mode-line-inactive "#839496")
        (modeline-err "#dc322f")
        (modeline-warning "#6c71c4")
        (modeline-info "#268bd2")
        (bg-tab-bar "#073642")
        (bg-tab-current "#002b36")
        (bg-tab-other "#073642")
        (bg-added "#263700")
        (bg-added-faint "#202c00")
        (bg-added-refine "#2e4200")
        (bg-added-intense "#6a7f00")
        (fg-added "#859900")
        (fg-added-intense "#778c00")
        (bg-changed "#4a2a00")
        (bg-changed-faint "#401f00")
        (bg-changed-refine "#553500")
        (bg-changed-intense "#987000")
        (fg-changed "#b58900")
        (fg-changed-intense "#a67c00")
        (bg-removed "#7f0000")
        (bg-removed-faint "#700000")
        (bg-removed-refine "#8e0000")
        (bg-removed-intense "#bd001a")
        (fg-removed "#dc322f")
        (fg-removed-intense "#cc1e24")
        (bg-diff-context "#002b36")
        (bg-paren-match "#004280")
        (bg-paren-expression "#880046")))

(setq modus-operandi-palette-overrides
      '((bg-main "#fdf6e3")
        (bg-dim "#eee8d5")
        (fg-main "#657b83")
        (fg-dim "#586e75")
        (fg-alt "#93a1a1")
        (bg-active "#fdf6e3")
        (bg-inactive "#eee8d5")
        (border "#657b83")
        (red "#dc322f")
        (red-warmer "#cc1e24")
        (red-cooler "#ec433b")
        (red-faint "#bd001a")
        (red-intense "#fc5246")
        (green "#859900")
        (green-warmer "#778c00")
        (green-cooler "#93a707")
        (green-faint "#6a7f00")
        (green-intense "#a1b420")
        (yellow "#b58900")
        (yellow-warmer "#a67c00")
        (yellow-cooler "#c39616")
        (yellow-faint "#987000")
        (yellow-intense "#d2a328")
        (blue "#268bd2")
        (blue-warmer "#007fc4")
        (blue-cooler "#3b98e0")
        (blue-faint "#0072b6")
        (blue-intense "#4da6ee")
        (magenta "#d33682")
        (magenta-warmer "#c42376")
        (magenta-cooler "#e2468f")
        (magenta-faint "#b50769")
        (magenta-intense "#f1559c")
        (cyan "#2aa198")
        (cyan-warmer "#12948b")
        (cyan-cooler "#3cafa5")
        (cyan-faint "#00877e")
        (cyan-intense "#4dbdb3")
        (rust "#7f0000")
        (gold "#553500")
        (olive "#2e4200")
        (slate "#004280")
        (indigo "#273681")
        (maroon "#8e0000")
        (pink "#880046")
        (bg-red-intense "#ffa189")
        (bg-green-intense "#d1e354")
        (bg-yellow-intense "#ffd158")
        (bg-blue-intense "#92e2ff")
        (bg-magenta-intense "#ffa3e6")
        (bg-cyan-intense "#7fede2")
        (bg-red-subtle "#ffaf96")
        (bg-green-subtle "#e0f262")
        (bg-yellow-subtle "#ffdf66")
        (bg-blue-subtle "#a1f0ff")
        (bg-magenta-subtle "#ffb2f5")
        (bg-cyan-subtle "#8efbf0")
        (bg-red-nuanced "#ffbea4")
        (bg-green-nuanced "#eeff70")
        (bg-yellow-nuanced "#ffee74")
        (bg-blue-nuanced "#b0ffff")
        (bg-magenta-nuanced "#ffc0ff")
        (bg-cyan-nuanced "#9dffff")
        (bg-ochre "#ffcd8e")
        (bg-lavender "#efefff")
        (bg-sage "#eeff70")
        (bg-graph-red-0 "#dc322f")
        (bg-graph-red-1 "#fc5246")
        (bg-graph-green-0 "#859900")
        (bg-graph-green-1 "#a1b420")
        (bg-graph-yellow-0 "#b58900")
        (bg-graph-yellow-1 "#d2a328")
        (bg-graph-blue-0 "#268bd2")
        (bg-graph-blue-1 "#4da6ee")
        (bg-graph-magenta-0 "#d33682")
        (bg-graph-magenta-1 "#f1559c")
        (bg-graph-cyan-0 "#2aa198")
        (bg-graph-cyan-1 "#4dbdb3")
        (bg-completion "#efefff")
        (bg-hover "#d2d3ff")
        (bg-hover-secondary "#ffd158")
        (bg-hl-line "#eee8d5")
        (bg-region "#002b36")
        (fg-region "#839496")
        (bg-char-0 "#8efbf0")
        (bg-char-1 "#ffb2f5")
        (bg-char-2 "#e0f262")
        (bg-mode-line-active "#fdf6e3")
        (fg-mode-line-active "#657b83")
        (border-mode-line-active "#657b83")
        (bg-mode-line-inactive "#eee8d5")
        (fg-mode-line-inactive "#586e75")
        (border-mode-line-inactive "#657b83")
        (modeline-err "#dc322f")
        (modeline-warning "#6c71c4")
        (modeline-info "#268bd2")
        (bg-tab-bar "#eee8d5")
        (bg-tab-current "#fdf6e3")
        (bg-tab-other "#eee8d5")
        (bg-added "#e0f262")
        (bg-added-faint "#eeff70")
        (bg-added-refine "#d1e354")
        (bg-added-intense "#a1b420")
        (fg-added "#859900")
        (fg-added-intense "#93a707")
        (bg-changed "#ffdf66")
        (bg-changed-faint "#ffee74")
        (bg-changed-refine "#ffd158")
        (bg-changed-intense "#d2a328")
        (fg-changed "#b58900")
        (fg-changed-intense "#c39616")
        (bg-removed "#ffaf96")
        (bg-removed-faint "#ffbea4")
        (bg-removed-refine "#ffa189")
        (bg-removed-intense "#fc5246")
        (fg-removed "#dc322f")
        (fg-removed-intense "#ec433b")
        (bg-diff-context "#fdf6e3")
        (bg-paren-match "#92e2ff")
        (bg-paren-expression "#ffa3e6")))

;; Load the dark theme by default
(load-theme 'modus-operandi t)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)
