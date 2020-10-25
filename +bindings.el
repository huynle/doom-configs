;;; ~/.config/doom/+binding.el -*- lexical-binding: t; -*-

;; (when (eq system-type 'gnu/linux)
;;   (setq x-super-keysym 'meta))

;; (when IS-MAC (setq mac-command-modifier 'meta
;;                    mac-option-modifier  'alt))
;;
;; (when (eq system-type 'darwin)
;;   (setq mac-command-modifier 'meta))
;; (when (eq system-type 'gnu/linux)
;;   (setq x-super-keysym 'meta))

;; (when (eq system-type 'darwin)
;;   (setq mac-command-modifier 'meta))

;;; * edit
;;; ** disable `s-x' on macOS to prevent accidental deletions
;; (map! "s-x" nil)

(map!
  ;; Ensure there are no conflicts
  :nmvo doom-leader-key nil
  :nmvo doom-localleader-key nil

  ;; (:map override ;; general-override-mode-map
  ;; "C-h"    #'evil-window-left
  ;; "C-j"    #'evil-window-down
  ;; "C-k"    #'evil-window-up
  ;; "C-l"    #'evil-window-right

  ;; This would screw up escape in the terminal
  ;; because c-t is equilvalent to escape
  ;;"C-t"   #'xref-pop-marker-stack

  ;; :n "q"      #'delete-window
  :n "q"       #'+workspace/close-window-or-workspace
  :n "Q"      #'evil-record-macro

  ;; Text-scaling
  "M-+"    (λ! (text-scale-set 0))
  "M-="    #'text-scale-increase
  "M--"    #'text-scale-decrease
  ;; )
   ;; Text Editing
  ;; :nv "gc" #'evilnc-comment-or-uncomment-lines

 ;; Leader Configs
  (:leader
    :desc "Search project"         :n  "g"   #'+default/search-project
    :desc "Search lines in buffer"         :n  "l"   #'counsel-grep-or-swiper
    :desc "comment"               :nv "v" #'evilnc-comment-or-uncomment-lines

    (:prefix "o"                      ; toggle
     :desc "Journal file"            "j" #'org-journal-open-current-journal-file)

    ;;; <leader> q --- quit/session
    (:prefix-map ("q" . "quit/session")
      :desc "Restart emacs server"         "d" #'+default/restart-server
      :desc "Delete frame"                 "f" #'delete-frame
      :desc "Clear current frame"          "F" #'doom/kill-all-buffers
      :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
      :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
      :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
      :desc "Quick save current session"   "s" #'doom/quicksave-session
      :desc "Restore last session"         "l" #'doom/quickload-session
      :desc "Save session to file"         "S" #'doom/save-session
      :desc "Restore session from file"    "L" #'doom/load-session
      :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
      :desc "Restart Emacs"                "R" #'doom/restart)


    ;;; <leader> s --- search
    (:prefix-map ("s" . "search")
      :desc "Search buffer"                "b" #'swiper
      :desc "Search current directory"     "d" #'+default/search-cwd
      :desc "Search other directory"       "D" #'+default/search-other-cwd
      :desc "Locate file"                  "f" #'locate
      :desc "Jump to symbol"               "i" #'imenu
      :desc "Jump to visible link"         "l" #'link-hint-open-link
      :desc "Jump to link"                 "L" #'ffap-menu
      :desc "Jump list"                    "j" #'evil-show-jumps
      :desc "Jump to bookmark"             "m" #'bookmark-jump
      :desc "Look up online"               "o" #'+lookup/online
      :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
      :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
      :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
      :desc "Search project"               "p" #'+default/search-project
      :desc "Search other project"         "P" #'+default/search-other-project
      :desc "Jump to mark"                 "r" #'evil-show-marks
      :desc "Search buffer"                "s" #'swiper-isearch
      :desc "Search buffer for thing at point" "S" #'swiper-isearch-thing-at-point
      :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
      :desc "Thesaurus"                    "T" #'+lookup/synonyms)



  ;; (:after evil-easymotion
  ;;   :m "gs" evilem-map
  ;;   (:map evilem-map
  ;;   "a" (evilem-create #'evil-forward-arg)
  ;;   "A" (evilem-create #'evil-backward-arg)
  ;;   "s" #'evil-avy-goto-char-2
  ;;   "SPC" (cmd!! #'evil-avy-goto-char-timer t)
  ;;   "/" #'evil-avy-goto-char-timer))
  )
)
 ;; Leader Configs
 ;; (:leader
   ;; :desc "M-x"                    :nv ":"   #'execute-extended-command
   ;; :desc "Neotree find file"         :n  "n"   #'+neotree/find-this-file

   ;; :desc "Horizonal Split"        :n  "s"   #'split-window-below
   ;; :desc "Vertical Split"         :n  "v"   #'split-window-right

   ;; :desc "Next Error"             :n  "]"   #'flycheck-next-error
   ;; :desc "Previous Error"         :n  "["   #'flycheck-previous-error
   ;; :desc "Show flycheck errors"   :n  "!"   #'flycheck-list-errors

   ;; :desc "Find project"           :n  "p"   #'projectile-switch-project

   ;; :desc "Eval"                   :n  "e"   #'+eval/buffer
   ;; :v  "e"   #'+eval/region

   ;; :desc "Switch to last buffer"  :n  "SPC" #'evil-switch-to-windows-last-buffer
   ;; :desc "Save buffer"            :n  "RET" #'save-buffer

   ;; :desc "Delete the window"      :n  "q"   #'delete-window
   ;; :desc "Ivy open buffers"       :n  "b"   #'ivy-switch-buffer

   ;; :desc "Toggle between file and tests"  :n "t" #'projectile-toggle-between-implementation-and-test

   ;; (:desc "help" :prefix "h"
   ;;   :n "h" help-map
   ;;   :desc "Apropos"               :n "a" #'apropos
   ;;   :desc "Reload theme"          :n "R" #'doom/reload-theme
   ;;   :desc "Find library"          :n "l" #'find-library
   ;;   :desc "Toggle Emacs log"      :n "m" #'doom/popup-toggle-messages
   ;;   :desc "Command log"           :n "L" #'global-command-log-mode
   ;;   :desc "Describe function"     :n "f" #'describe-function
   ;;   :desc "Describe key"          :n "k" #'describe-key
   ;;   :desc "Describe char"         :n "c" #'describe-char
   ;;   :desc "Describe mode"         :n "M" #'describe-mode
   ;;   :desc "Show messages"         :n "m" #'view-echo-area-messages
   ;;   :desc "Describe variable"     :n "v" #'describe-variable
   ;;   :desc "Describe face"         :n "F" #'describe-face
   ;;   :desc "Describe DOOM setting" :n "s" #'doom/describe-setting
   ;;   :desc "Describe DOOM module"  :n "d" #'doom/describe-module
   ;;   :desc "Find definition"       :n "." #'+jump/definition
   ;;   :desc "Find references"       :n "/" #'+jump/references
   ;;   :desc "Find documentation"    :n "h" #'+jump/documentation
   ;;   :desc "What face"             :n "'" #'doom/what-face
   ;;   :desc "What minor modes"      :n ";" #'doom/what-minor-mode
   ;;   :desc "Info"                  :n "i" #'info
   ;;   :desc "Toggle profiler"       :n "p" #'doom/toggle-profiler)


   ;; (:desc "git" :prefix "g"
   ;;   :desc "Git status"        :n  "s" #'magit-status
   ;;   :desc "Git blame"         :n  "b" #'magit-blame
   ;;   :desc "Git timemachine branch" :n  "B" #'git-timemachine-switch-branch
   ;;   :desc "Git time machine"  :n  "t" #'git-timemachine-toggle
   ;;   :desc "Git revert hunk"   :n  "r" #'git-gutter:revert-hunk
   ;;   :desc "List gists"        :n  "g" #'+gist:list
   ;;   :desc "Next hunk"         :nv "]" #'git-gutter:next-hunk
   ;;   :desc "Previous hunk"     :nv "[" #'git-gutter:previous-hunk)

   ;; (:desc "open" :prefix "o"
   ;;   :desc "Org Agenda"          :n  "a" #'org-agenda
   ;;   :desc "Default browser"     :n  "b" #'browse-url-of-file
   ;;   :desc "Debugger"            :n  "d" #'+debug/open
   ;;   :desc "REPL"                :n  "r" #'+eval/repl
   ;;   :v  "r" #'+eval:repl
   ;;   :desc "Neotree"             :n  "n" #'+neotree/toggle
   ;;   :desc "Terminal"            :n  "t" #'+term/open-popup
   ;;   :desc "Terminal in project" :n  "T" #'+term/open-popup-in-project
   ;;   :desc "Org Capture"         :n  "o" #'org-capture)

   ;; (:desc "orgmode" :prefix "O"
   ;;   :desc "Capture item"        :n  "O" #'org-capture
   ;;   :desc "Show all todos"      :n  "t" #'org-todo-list
   ;;   )

   ;; (:desc "insert" :prefix "i"
   ;;   :desc "From kill-ring" :nv "p" #'counsel-yank-pop
   ;;   :desc "From snippet"   :nv "s" #'yas-insert-snippet)

   ;; (:desc "workspace" :prefix "TAB"
   ;;   :desc "Display tab bar"          :n "TAB" #'+workspace/display
   ;;   :desc "New workspace"            :n "n"   #'+workspace/new
   ;;   :desc "Load workspace from file" :n "l"   #'+workspace/load
   ;;   :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
   ;;   :desc "Save workspace to file"   :n "s"   #'+workspace/save
   ;;   :desc "Autosave current session" :n "S"   #'+workspace/save-session
   ;;   :desc "Switch workspace"         :n "."   #'+workspace/switch-to
   ;;   :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
   ;;   :desc "Delete session"           :n "X"   #'+workspace/kill-session
   ;;   :desc "Delete this workspace"    :n "d"   #'+workspace/delete
   ;;   :desc "Load session"             :n "L"   #'+workspace/load-session
   ;;   :desc "Next workspace"           :n "]"   #'+workspace/switch-right
   ;;   :desc "Previous workspace"       :n "["   #'+workspace/switch-left
   ;;   :desc "Rename workspace"         :n "r"   #'+workspace:rename
   ;;   :desc "Switch to 1st workspace"  :n "0"   (λ! (+workspace/switch-to 0))
   ;;   :desc "Switch to 2nd workspace"  :n "1"   (λ! (+workspace/switch-to 1))
   ;;   :desc "Switch to 3rd workspace"  :n "2"   (λ! (+workspace/switch-to 2))
   ;;   :desc "Switch to 4th workspace"  :n "3"   (λ! (+workspace/switch-to 3))
   ;;   :desc "Switch to 5th workspace"  :n "4"   (λ! (+workspace/switch-to 4))
   ;;   :desc "Switch to 6th workspace"  :n "5"   (λ! (+workspace/switch-to 5))
   ;;   :desc "Switch to 7th workspace"  :n "6"   (λ! (+workspace/switch-to 6))
   ;;   :desc "Switch to 8th workspace"  :n "7"   (λ! (+workspace/switch-to 7))
   ;;   :desc "Switch to 9th workspace"  :n "8"   (λ! (+workspace/switch-to 8)))


 ;; (:after magit
 ;;   :map magit-blame-mode-map
 ;;   :n "q" #'magit-blame-quit)


 ;; ;; neotree
 ;; (:after neotree
 ;;   :map neotree-mode-map
 ;;   :n "g"         nil
 ;;   :n [tab]       #'neotree-quick-look
 ;;   :n "RET"       #'neotree-enter
 ;;   :n [backspace] #'evil-window-prev
 ;;   :n "c"         #'neotree-create-node
 ;;   :n "j"         #'neotree-next-line
 ;;   :n "k"         #'neotree-previous-line
 ;;   :n "n"         #'neotree-next-line
 ;;   :n "p"         #'neotree-previous-line
 ;;   :n "h"         #'+neotree/collapse-or-up
 ;;   :n "l"         #'+neotree/expand-or-open
 ;;   :n "J"         #'neotree-select-next-sibling-node
 ;;   :n "K"         #'neotree-select-previous-sibling-node
 ;;   :n "H"         #'neotree-select-up-node
 ;;   :n "L"         #'neotree-select-down-node
 ;;   :n "G"         #'evil-goto-line
 ;;   :n "gg"        #'evil-goto-first-line
 ;;   :n "v"         #'neotree-enter-vertical-split
 ;;   :n "s"         #'neotree-enter-horizontal-split
 ;;   :n "q"         #'neotree-hide
 ;;   :n "R"         #'neotree-refresh)

 ;; ;; company
 ;; (:after company
 ;;   :map company-active-map
 ;;   "C-j"         #'company-select-next
 ;;   "C-n"         #'company-select-next
 ;;   "C-k"         #'company-select-previous
 ;;   "C-p"         #'company-select-previous
 ;;   "C-d"         #'company-show-doc-buffer
 ;;   )

 ;; ;; counsel
 ;; (:after counsel
 ;;   (:map ivy-mode-map
 ;;     "C-o"      #'ivy-dispatching-done)
 ;;   (:map counsel-ag-map
 ;;     [backtab]  #'+ivy/wgrep-occur           ; search/replace on results
 ;;     "C-SPC"    #'counsel-git-grep-recenter)) ; preview
 ;; ;; Elixir Mode
 ;; (:after elixir-mode
 ;;   (:leader
 ;;     :desc "Toggle between file and tests"   :n "t" (λ! (alchemist-project-toggle-file-and-tests))
 ;;     :desc "Jump to definition at point"     :n "l" #'alchemist-goto-definition-at-point))

 ;; ;; Haskell Mode
 ;; (:after haskell-mode
 ;;   :map haskell-mode-map
 ;;   :localleader
 ;;   :desc "Show IMenu Nodes"             "m" #'+lsp-ui-imenu
 ;;   :desc "Apply LSP Action"             "a" #'lsp-ui-sideline-apply-code-actions)

 ;; ;; Rust Mode
 ;; (:after rustic
 ;;   (:map rustic-mode-map
 ;;      :leader
 ;;     :desc "Find documentation" :n "d" #'+lookup/documentation
 ;;     :desc "Find definition" :n "l" #'+lookup/definition
 ;;     :localleader
 ;;     :n "f" (λ! (message (or (lsp-workspace-root-debug) "No root!")))
 ;;     :desc "Peek implementations" :n "i" #'lsp-ui-peek-find-implementation
 ;;     :desc "Apply LSP Code Action" :n "a" #'lsp-ui-sideline-apply-code-actions))


 ;; ;; Racket Mode
 ;; (:after racket-mode
 ;;   (:leader
 ;;     :desc "Describe symbol at point"     :n "d" #'racket-describe))

 ;; (:after elisp-mode
 ;;   (:leader
 ;;     :desc "Describe symbol at point"     :n "d" #'+lookup/documentation
 ;;     :desc "Jump to definition at point"  :n "l" #'+lookup/definition)
 ;;   (:localleader
 ;;     :n "p" #'eval-last-sexp
 ;;     :n "b" #'eval-buffer))

 ;; (:after clojure-mode
 ;;   (:map clojure-mode-map
 ;;     :leader
 ;;     :n "\\" #'ivy-cider-apropos
 ;;     :n "DEL" #'ivy-cider-browse-ns
 ;;     (:desc "reload" :prefix "r"
 ;;       :desc "Refresh user libraries" :n "l" #'rs/user/sync-libs
 ;;       :desc "Restart Integrant" :n "r" #'rs/ig/restart
 ;;       :desc "Reload Integrant" :n "R" #'rs/ig/reset)
 ;;     :localleader
 ;;     :n "'" #'cider-jack-in
 ;;     :n "\"" #'cider-jack-in-clojurescript))
 ;; ;; cider-mode
 ;; (:after cider-mode
 ;;   (:map cider-mode-map
 ;;     :leader
 ;;     :desc "Lookup documentation at point" :n "d" #'cider-doc
 ;;     :desc "Jump to definition at point" :n "l" #'cider-find-var
 ;;     :localleader
 ;;     :n "b" #'cider-eval-buffer
 ;;     :n "B" #'cider-switch-to-repl-buffer
 ;;     :n "n" #'cider-repl-set-ns
 ;;     :n "j" #'cider-find-var
 ;;     :n "s" #'cider-browse-spec
 ;;     :n "S" #'helm-cider-spec-ns
 ;;     :n "l" #'cljr-move-to-let
 ;;     :n "L" #'cljr-introduce-let
 ;;     (:desc "docs" :prefix "d"
 ;;       :desc "Browse Namespace" :n "n" #'cider-browse-ns
 ;;       :desc "Browse Spec" :n "s" #'cider-browse-spec
 ;;       :desc "Load ClojureDoc" :n "d" #'cider-clojuredocs)
 ;;     :n "h" #'cider-doc
 ;;     :n "c" #'cider-repl-clear-buffer
 ;;     :n "i" #'cider-inspect-last-result
 ;;     :n "p" #'cider-eval-sexp-at-point
 ;;     :n "f" #'cider-eval-defun-at-point
 ;;     :n "t" #'cider-test-run-ns-tests
 ;;     :n "T" #'cider-test-run-test)
 ;;   (:after cider-browse-ns-mode
 ;;     (:map cider-browse-ns-mode-map
 ;;       :n "RET"       #'cider-browse-ns-operate-at-point)))

 ;; ;; org-mode
 ;; (:after org-mode
 ;;   (:map org-mode-map
 ;;     :localleader
 ;;     :n  "t"  #'org-todo
 ;;     :n  "d"  #'org-deadline
 ;;     :n  "s"  #'org-schedule))

 ;; (:after julia-mode
 ;;   (:map julia-mode-map
 ;;     :localleader
 ;;     :n  "b"  #'ess-eval-buffer
 ;;     :n  "p"  #'ess-eval-function-or-paragraph
 ;;     :n  "s"  #'org-schedule))
 ;; )


;; ;; This section is dedicated to "fixing" certain keys so that they behave
;; ;; properly, more like vim. Ripped directly from hlissner
;; (map! (:map input-decode-map
;;         [?\C-i] [C-i]
;;         [S-iso-lefttab] [backtab]
;;         (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

;;       ;; I want C-a and C-e to be a little smarter. C-a will jump to
;;       ;; indentation. Pressing it again will send you to the true bol. Same goes
;;       ;; for C-e, except it will ignore comments and trailing whitespace before
;;       ;; jumping to eol.
;;       :i "C-a" #'doom/backward-to-bol-or-indent
;;       :i "C-e" #'doom/forward-to-last-non-comment-or-eol
;;       :i "C-u" #'doom/backward-kill-to-bol-and-indent

;;       ;; textmate-esque newline insertion
;;       :i [M-return] #'evil-open-below
;;       :i [S-M-return] #'evil-open-above
;;       ;; textmate-esque deletion
;;       [M-backspace] #'doom/backward-kill-to-bol-and-indent
;;       :i [backspace] #'delete-backward-char
;;       :i [M-backspace] #'doom/backward-kill-to-bol-and-indent
;;       ;; Emacsien motions for insert mode
;;       :i "C-b" #'backward-word
;;       :i "C-f" #'forward-word

;;       ;; Highjacks space/backspace to:
;;       ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
;;       ;;   b) delete space-indented blocks intelligently
;;       ;;   c) do none of this when inside a string
;;       :i [remap newline] #'newline-and-indent

;;       (:after org-mode
;;         (:map org-mode-map
;;           :i [remap doom/inflate-space-maybe] #'org-self-insert-command))

;;       ;; Make ESC quit all the things
;;       (:map (minibuffer-local-map
;;              minibuffer-local-ns-map
;;              minibuffer-local-completion-map
;;              minibuffer-local-must-match-map
;;              minibuffer-local-isearch-map)
;;         [escape] #'abort-recursive-edit
;;         "C-r" #'evil-paste-from-register)

;;       (:map messages-buffer-mode-map
;;         "M-;" #'eval-expression
;;         "A-;" #'eval-expression)

;;       (:map tabulated-list-mode-map
;;         [remap evil-record-macro] #'doom/popup-close-maybe)

;;       (:map (evil-ex-completion-map evil-ex-search-keymap read-expression-map)
;;         "C-a" #'move-beginning-of-line
;;         "C-w" #'doom/minibuffer-kill-word
;;         "C-u" #'doom/minibuffer-kill-line
;;         "C-b" #'backward-word
;;         "C-f" #'forward-word
;;         "M-z" #'doom/minibuffer-undo)

;;       (:after view
;;         (:map view-mode-map "<escape>" #'View-quit-all)))

;; (defun lsp-workspace-root-debug (&optional path)
;;   "Find the workspace root for the current file or PATH."
;;   (when-let (file-name (or path (buffer-file-name)))
;;     (do (message "Yay!")
;;         (->> (lsp-session)
;;              (lsp-session-folders)
;;              ;; (--first (f-ancestor-of? it (f-canonical file-name)))
;;              )))

