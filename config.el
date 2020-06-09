(setq user-full-name "Huy Le")
(setq user-mail-address "dacrazyazn.com")
(setq create-lockfiles nil)
(setq doom-localleader-key ";")
(setq doom-localleader-alt-key "M-;")
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
        :desc "Rainbow mode" "r" #'rainbow-mode))
(map!
 (:after evil
   :m  "] SPC" #'evil-motion-insert-newline-below
   :m  "[ SPC" #'evil-motion-insert-newline-above))
;; (map!
;;  (:after evil
;;    :en "C-h"   #'evil-window-left
;;    :en "C-j"   #'evil-window-down
;;    :en "C-k"   #'evil-window-up
;;    :en "C-l"   #'evil-window-right))
;; Many thanks to the author of and contributors to the following posts:
;; https://gist.github.com/mislav/5189704
;; https://robots.thoughtbot.com/post/53022241323/seamlessly-navigate-vim-and-tmux-splits
;;
;; TODO: Make a script that generates tmux and emacs code without duplication
;;
;; NOTE: My keybindings are not the default emacs ones, using windmove

;; Try to move direction, which is supplied as arg
;; If cannot move that direction, send a tmux command to do appropriate move
;; (defun windmove-emacs-or-tmux(dir tmux-cmd)
;; (interactive)
;; (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
;; nil                       ;; Moving within emacs
;; (shell-command tmux-cmd)) ;; At edges, send command to tmux
;; )

;; ;Move between windows with custom keybindings
;; (global-set-key (kbd "C-k")
;;    '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
;; (global-set-key (kbd "C-j")
;;    '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
;; (global-set-key (kbd "C-l")
;;    '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
;; (global-set-key (kbd "C-h")
;;    '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))
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
(setq doom-line-numbers-style 'relative)
(use-package! org
  :config
  (setq org-agenda-files (file-expand-wildcards "~/docs/org/*.org"))
  (setq org-directory (expand-file-name "~/docs/org"))
  (setq org-cycle-separator-lines 1)
  (defvar +org-dir (expand-file-name "~/docs/org")))
(use-package! org
  :config
  (setq org-capture-templates
        '(("l" "Linked Tasks" entry (file+headline "~/docs/org/gtd.org" "Linked Tasks")
           "* TODO %?\n  Entered on: %U - %a\n")
          ("t" "Tasks" entry (file+headline "~/docs/org/gtd.org" "Tasks")
           "* TODO %?\n  Entered on: %U\n")
          ("p" "Private" entry (file+datetree "~/docs/org/logbook.org")
           "* %?\n\n")
          ("j" "Journal" entry (file+datetree "~/docs/org/journal.org")
           "* %?\n\n"))))
;; (setq org-attach-directory $HOME/testing/attachment)
(setq org-download-method 'attach)
;; (setq org-attach-directory $HOME/testing/attachment)
(setq org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s")
(setq org-agenda-custom-commands
      '(("%" "Appointments" agenda* "Today's appointments"
	 ((org-agenda-span 1)
          (org-agenda-max-entries 3)))))
(setq centaur-tabs-height 25)
(setq centaur-tabs-cycle-scope 'tabs)
;; (setq doom-modeline-def-modeline "project")

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   )
;; (with-eval-after-load "doom-modeline"
;;   (doom-modeline-def-modeline 'main
;;   '(misc-info bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
;;   '(objed-state persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker)
;;   )
;; )
  ;; Define your custom doom-modeline
;; (doom-modeline-def-modeline 'my-simple-line
;;     '(bar " " buffer-info)
;;     '(misc-info))

;; ;; Add to `doom-modeline-mode-hook` or other hooks
;; (defun setup-custom-doom-modeline ()
;;     (doom-modeline-set-modeline 'my-simple-line 'default))
;; (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
