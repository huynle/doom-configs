(setq user-full-name "Huy Le")
(setq user-mail-address "dacrazyazn.com")
(setq create-lockfiles nil)
;; (setq doom-localleader-key ";")
(map! :leader
      (:prefix "TAB"
        :desc "Rename workspace"       "r"  #'+workspace/rename)
      (:prefix "f"
        :desc "Find remote file"      "R" #'counsel-tramp
        :desc "Find file in dotfiles" "t" #'+brett/find-in-dotfiles
        :desc "Browse dotfiles"       "T" #'+brett/browse-dotfiles)
      (:prefix "n"
        :desc "Browse mode notes"    "m" #'+brett/find-notes-for-major-mode
        :desc "Browse project notes" "p" #'+brett/find-notes-for-project)
      (:prefix "t"
        :desc "Rainbow mode" "r" #'rainbow-mode)
      (:prefix "/"
        :desc "Hex colors" "h"  #'counsel-hex-colors))
(map!
 (:after evil
   :m  "] SPC" #'evil-motion-insert-newline-below
   :m  "[ SPC" #'evil-motion-insert-newline-above))
(map!
 (:after evil
   :en "C-h"   #'evil-window-left
   :en "C-j"   #'evil-window-down
   :en "C-k"   #'evil-window-up
   :en "C-l"   #'evil-window-right))
;; (map! (:localleader
;;         (:after evil-org
;;           :map evil-org-mode-map
;;           "/" #'counsel-org-goto)))
;; (map!
;;  (:after dired
;;     (:map dired-mode-map
;;     "C-SPC" #'peep-dired)))
(after! ivy
  (ivy-set-actions
   'ivy-switch-buffer
   '(("s" evil-window-split "split horizontally")
     ("v" evil-window-vsplit "split vertically")))
  (ivy-set-actions
   'counsel-find-file
   '(("s" evil-window-split "split horizontally")
     ("v" evil-window-vsplit "split vertically"))))
;; (map!
;;  (:after treemacs-evil
;;    (:map evil-treemacs-state-map
;;      "C-h" #'evil-window-left
;;      "C-l" #'evil-window-right)))
;; (setq doom-localleader-key ";")
(after! which-key
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.01
        which-key-sort-order 'which-key-key-order-alpha))
(setq mouse-wheel-scroll-amount '(3)
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      scroll-step 1)
(setq doom-scratch-buffer-major-mode t)
(setq show-trailing-whitespace t)
(after! all-the-icons
  (setq all-the-icons-scale-factor 1.0))
;; (setq eldoc-idle-delay 0)
