;;; sfieger-synonyms-download.el --- Downloading synonyms.el and mthesaur.txt

;;; Commentary:
;; Downloads synonyms.el and mthesaur.txt if they are not already in their place.

;;; Code:

(when (not (file-exists-p "~/.config/emacs/contrib/synonyms/"))
  (make-directory "~/.config/emacs/contrib/synonyms")
  (when (not (file-exists-p "~/.config/emacs/contrib/synonyms/synonyms.el"))
    (require 'url)
    (url-copy-file "https://www.emacswiki.org/emacs/download/synonyms.el" "~/.config/emacs/contrib/synonyms/synonyms.el"))
  (when (not (file-exists-p "~/.config/emacs/contrib/synonyms/mthesaur.txt"))
    (require 'url)
    (url-copy-file "https://archive.org/download/mobythesauruslis03202gut/mthesaur.txt" "~/.config/emacs/contrib/synonyms/mthesaur.txt")))

(provide 'sfieger-synonyms-download)
;;; sfieger-synonyms-download.el ends here

