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
;; (setq org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s")
(setq org-download-screenshot-method "pngpaste %s")
(setq org-agenda-custom-commands
      '(("%" "Appointments" agenda* "Today's appointments"
	 ((org-agenda-span 1)
          (org-agenda-max-entries 3)))))
(setq centaur-tabs-height 25)
(setq centaur-tabs-cycle-scope 'tabs)
;; (defun my-dnd-func (event)
;;   (interactive "e")
;;   (goto-char (nth 1 (event-start event)))
;;   (x-focus-frame nil)
;;   (let* ((payload (car (last event)))
;;          (type (car payload))
;;          (fname (cadr payload))
;;          (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
;;     (cond
;;      ;; insert image link
;;      ((and  (eq 'drag-n-drop (car event))
;;             (eq 'file type)
;;             (string-match img-regexp fname))
;;       (insert (format "[[%s]]" fname))
;;       (org-display-inline-images t t))
;;      ;; insert image link with caption
;;      ((and  (eq 'C-drag-n-drop (car event))
;;             (eq 'file type)
;;             (string-match img-regexp fname))
;;       (insert "#+ATTR_ORG: :width 300\n")
;;       (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
;;       (insert (format "[[%s]]" fname))
;;       (org-display-inline-images t t))
;;      ;; C-drag-n-drop to open a file
;;      ((and  (eq 'C-drag-n-drop (car event))
;;             (eq 'file type))
;;       (find-file fname))
;;      ((and (eq 'M-drag-n-drop (car event))
;;            (eq 'file type))
;;       (insert (format "[[attachfile:%s]]" fname)))
;;      ;; regular drag and drop on file
;;      ((eq 'file type)
;;       (insert (format "[[%s]]\n" fname)))
;;      (t
;;       (error "I am not equipped for dnd on %s" payload)))))


;; (define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
;; (define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
;; (define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)
(global-set-key [M-s-drag-n-drop] 'ns-drag-n-drop-as-text)
;; (defun org-insert-clipboard-image (&optional file)
;;   (interactive "F")
;;   (shell-command (concat "pngpaste " file))
;;   (insert (concat "[[" file "]]"))
;;   (org-display-inline-images))

;; (defun my/org-insert-clipboard ()
;;     (interactive)
;;     (setq myvar/folder-path (concat default-directory "img/")) ;make the img directory
;;     (if (not (file-exists-p myvar/folder-path))
;;         (mkdir myvar/folder-path)) ;create the directory if it doesn't exist
;;     (let* ((image-file (concat
;;                         myvar/folder-path
;;                         (buffer-name)
;;                         "_"
;;                         (format-time-string "%Y%m%d_%H%M%S_.png")))
;;            (exit-status
;;             (call-process "convert" nil nil nil
;;                           "clipboard:" image-file)))
;;       (org-insert-link nil (concat "file:" image-file) "")
;;       (org-display-inline-images)))


(defun org-insert-image ()
  (interactive)
  (let* ((path (concat default-directory "data/"))
         (image-file (concat
                      path
                      (buffer-name)
                      (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
        (mkdir path))
    (shell-command (concat "pngpaste " image-file))
    (org-insert-link nil (concat "file:" image-file) ""))
    ;; (org-display-inline-images) ;; show inline picture
  )
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
