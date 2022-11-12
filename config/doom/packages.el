;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! org-roam)
(package! org-roam-ui)

(package! org-xournalpp
  :recipe (:host gitlab
           :repo "vherrmann/org-xournalpp"
           :files ("resources" "*.el")))

(package! ement
  :recipe (:host github :repo "alphapapa/ement.el"))
