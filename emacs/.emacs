;; EMACS CONFIG FROM UBUNTU


;; Personal Information
(setq user-full-name "Sakhya Ghosh"
      user-mail-address "sakhyaghosh@google.com")


;;
;; Add melpa package repository.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("GELPA" . "http://internal-elpa.appspot.com/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;
;; use-package
;;
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)


;; Directory Settings
;; Saving customizations in a different file
(setq custom-file "~/.emacs-custom.el")
(load custom-file)
;; Load path for extensions
(add-to-list 'load-path "~/.emacs.d")
;; ----Default Directory----
;; set default
(setq default-directory "~/")


;;
;; *****************Goodies*********************
;;

;;
;; asynchronous processing
(use-package async
   :ensure async)
(require 'async-bytecomp)

;; easier key binding
(use-package bind-key
  :ensure bind-key)



;; ;; Hungry Delete
;; ;;
;; (use-package hungry-delete
;;   :ensure hungry-delete)
;; (global-hungry-delete-mode)
;; (global-auto-revert-mode)
;; (global-font-lock-mode)




;; recent files.
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)


;; Language-aware editing commands. Useful for imenu-menu.
(semantic-mode 1)

;;
;; Mighty helm
;;
(use-package helm
  :ensure helm)

;; From http://tuhdo.github.io/helm-intro.html
;; must set before helm-config,  otherwise helm use default
;; prefix "C-x c", which is inconvenient because you can
;; accidentially pressed "C-x C-c"
(setq helm-command-prefix-key "C-c h")

(require 'helm-config)
(require 'helm-files)
(require 'helm-grep)
(require 'helm-eshell)
(require 'helm-buffers)

(use-package helm-ag-r
  :ensure helm-ag-r)
(require 'helm-ag-r)

(use-package helm-swoop
  :ensure helm-swoop)
(require 'helm-swoop)

;;(use-package helm-dash
;;  :ensure helm-dash)
;;(require 'helm-dash)
;;(setq helm-dash-browser-func 'eww)
;;(add-hook 'eww-mode-hook ' (lambda ()
;;                             (setq-default show-trailing-whitespace nil)))

(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "M-C-s") 'helm-multi-swoop-all)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c f") 'helm-recentf)
(global-set-key (kbd "C-h ,") 'helm-apropos)

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(setq
 helm-google-suggest-use-curl-p t
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-quick-update t ; do not display invisible candidates
 helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
 helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

 helm-split-window-default-side 'below ;; open helm buffer below.
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                     '(picture-mode artist-mode))
 helm-buffer-max-length 30
 helm-candidate-number-limit 200 ; limit the number of displayed canidates
 helm-M-x-requires-pattern 0     ; show all candidates when set to 0
 helm-boring-file-regexp-list
 '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
 ido-use-virtual-buffers t      ; Needed in helm-buffers-list
 helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that lists buffers
 )

;; Save current position to mark ring when jumping to a different place
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode 1)

(defun helm-do-grep-recursive (&optional non-recursive)
  "Like `helm-do-grep', but greps recursively by default."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively 'helm-do-grep)))

;; Recursive grep helm
;;
(bind-key "C-c s r" 'helm-do-grep-recursive)


;;
;; Projectile
;; Best way (so far) to search for files in repo.
;
(use-package projectile
  :init
  (progn
    (projectile-global-mode)
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)))
(use-package helm-projectile)
(global-set-key (kbd "C-x f") 'helm-projectile)

;; Find other files.
(add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
(add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html
(add-to-list 'projectile-other-file-alist '("cc" "h")) ;; switch from cc -> h

;; Shortcut key to load the other file.
(global-set-key (kbd "C-x C-o") 'projectile-find-other-file)


;;
;; IDO vertical mode.
;;
(use-package ido-vertical-mode
  :ensure ido-vertical-mode)
(ido-vertical-mode)
;; using helm so IDO is disabled.
;; (setq ido-enable-flex-matching t)
;;   (setq ido-everywhere t)
;;   (ido-mode 1)


;;
;; ----Uniquify----
;; uniquely name the buffers so that it has filenames
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'forward
  uniquify-separator "/")

(setq frame-title-format
      '("%S" (buffer-file-name "%f"
                   (dired-directory dired-directory "%b"))))
;; showing the file name
(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))


;;
;; Autocomplete
;;

(use-package auto-complete
  :ensure auto-complete)
;; Load the default configuration
(require 'auto-complete-config)
;; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories "~/emacs/auto-complete/dict")
;; Load by default
(ac-config-default)
(ac-flyspell-workaround)
;; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
;; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
;;show menu immediately
;; (setq ac-auto-show-menu 0.3)
;; case sensitivity is important when finding matches
(setq ac-ignore-case nil)


;; ----yasnippet----
;;
(use-package yasnippet
  :ensure yasnippet)
;; Develop and keep personal snippets under ~/snippets
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/snippets")))
(yas-global-mode 1)
(yas--initialize)



;;
;; Flyspell (commented out as it makes my emacs quite slow)
;;
;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))
;; ;; Look for spelling mistakes in code comments and strings.
;; (add-hook 'c-mode-hook
;;           '(lambda () (flyspell-prog-mode)))
;; (add-hook 'c++-mode-hook
;;           '(lambda () (flyspell-prog-mode)))
;; (add-hook 'css-mode-hook
;;           '(lambda () (flyspell-prog-mode)))
;; (add-hook 'java-mode-hook
;;           '(lambda () (flyspell-prog-mode)))
;; (add-hook 'js2-mode-hook
;;           '(lambda () (flyspell-prog-mode)))
;; (add-hook 'python-mode-hook
;;           '(lambda () (flyspell-prog-mode)))
;; (add-hook 'emacs-mode-hook
;;           '(lambda () (flyspell-prog-mode)))


;;




;;
;; ********Look and Feel******
;;

;; Full screen emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; show line numbers
;; Display line numbers.
(use-package linum
  :ensure linum)
(global-linum-mode t)
;; Right-justify linum
;; From https://github.com/echosa/emacs.d#line-numbers
(setq linum-format (lambda
                     (line)
                     (propertize
                      (format (concat "%"
                                      (number-to-string
                                       (length
                                        (number-to-string
                                         (line-number-at-pos
                                          (point-max)))))
                                      "d ")
                              line)
                      'face
                      'linum)))

;; Display column numbers.
(setq-default column-number-mode t)

;; turn on highlighting
(global-hl-line-mode t)
(show-paren-mode t)

(blink-cursor-mode -1)
;; Highlight current line
(require 'hl-line)
(global-hl-line-mode 1)
;; Set any color as the background face of the current line:
(set-face-background 'hl-line "#111")
;;To keep syntax highlighting in the current line:
(set-face-foreground 'highlight nil)

;; just y for yes
(fset 'yes-or-no-p 'y-or-n-p)
;; will inhibit startup messages.
(setq inhibit-startup-message t)
;; Get rid of splash screens.
;; From http://www.emacswiki.org/emacs/EmacsNiftyTricks
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; syntax highlighting by default
(global-font-lock-mode 1)

(setq ring-bell-function 'ignore)


;; generic customizations

;; Disable backup.
;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
(setq backup-inhibited t)
;; Disable auto save.
;; From: http://anirudhsasikumar.net/blog/2005.01.21.html
(setq auto-save-default nil)
;; Auto revert files in emacs
(global-auto-revert-mode t)


;; Splitting
;; HorizontalSplitting
(setq split-height-threshold nil)
(setq split-width-threshold 9999)
;; Window splitting more useful
;; From http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldury
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)


;; Automatically closes brackets.
(electric-pair-mode)
(electric-indent-mode)

;; Put "carriage-return" for you automatically after left curly braces,
;; right curly braces, and semi-colons in "C mode".


;; Customizing mode line.
;; Based on http://emacs-fu.blogspot.co.uk/2011/08/customizing-mode-line.html
(setq-default mode-line-format
      (list
       ;;"★ "
       "✪ "
       ;; the buffer name; the file name as a tool tip
       '(:eval (propertize "%b"
                           'face 'font-lock-keyword-face
                           'help-echo (buffer-file-name)))
       " | "
       ;; line and column, '%02' to set to 2 chars at least
       ;; prevents flickering
       (propertize "%02l" 'face 'font-lock-type-face)
       ","
       (propertize "%02c" 'face 'font-lock-type-face)
       " | "

       ;; relative position, size of file
       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       "/"
       (propertize "%I" 'face 'font-lock-constant-face) ;; size
       " | "

       ;; the current major mode for the buffer.
       '(:eval (propertize "%m"
                           'face
                           'font-lock-string-face
                           'help-echo buffer-file-coding-system))
       " | "


       ;; insert vs overwrite mode, input-method in a tooltip
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-preprocessor-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode "overwrite" "insert") " mode")))

       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (concat ","  (propertize "Mod"
                                          'face 'font-lock-warning-face
                                          'help-echo "Buffer has been modified"))))

       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (concat ","  (propertize "RO"
                                          'face 'font-lock-type-face
                                          'help-echo "Buffer is read-only"))))
       " | "

       ;; add the time, with the date and the emacs uptime in the tooltip
       '(:eval (propertize (format-time-string "%H:%M")
                           'help-echo
                           (concat (format-time-string "%c; ")
                                   (emacs-uptime "Uptime:%hh"))))
       ))


;; Open with
;; Helps to open files externally.
(require 'openwith)
;; Set up to open json files in sublime.
(setq openwith-associations '(("\\.json\\'" "sublime" (file))))
(openwith-mode t)


;; ----TODO----
(require 'fic-mode)
  (add-hook 'prog-mode-hook 'turn-on-fic-mode)


;; load zenburn by defailt
(defun zenburn-init ()
  (load-theme 'zenburn)
)
(add-hook 'after-init-hook 'zenburn-init)


;; ----80 chars----
;; Turn on red highlighting for characters outside of the 80/100 char limit
;; Fill columns
;; (setq-default fill-column 80)
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (set-fill-column 80)))


;; Helps to copy paste in emacs
;;
;; http://stackoverflow.com/questions/5288213/how-can-i-paste-the-selected-region-outside-of-emacs
;; sudo apt-get install xclip
;; (use-package xclip
;;   :ensure xclip)
;; (xclip-mode)
(load-file "~/.emacs.d/xclip.el")



(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))




;;
;; ********Emacs Navigation*******
;;

;; Delete Whitespace Around Point
(global-set-key (kbd "C-x j") 'just-one-space)

(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)
(global-set-key "\M-\\" 'goto-last-change)


;; Moving between buffers
(bind-key "C-x x" 'next-buffer)
(bind-key "C-x z" 'previous-buffer)


(defun swap-windows ()
  (interactive)
  (let ((current-buf (current-buffer))
        (other-buf (progn
                     (other-window 1)
                     (current-buffer))))
    (switch-to-buffer current-buf)
    (other-window -1)
    (switch-to-buffer other-buf)))

(global-set-key (kbd "M-s s w") 'swap-windows)


;; Expand region (better selection)
(use-package expand-region
  :ensure expand-region)
(global-set-key (kbd "C-x w") 'er/expand-region)


;; CamelCase Navigation.
;; Enabling subword mode (ie. navigate cameCase)
;; From http://www.emacswiki.org/emacs/CamelCase
(global-subword-mode t)
;; (add-hook 'c-mode-common-hook
;;           (lambda () (subword-mode 1)))

;;  ----Window movement----
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Pop to mark
;; Handy way of getting back to previous places.
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)


;; Ace jump navigation
(use-package ace-jump-mode
  :ensure ace-jump-mode)
(require 'ace-jump-mode)

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
(global-set-key (kbd "C-^") 'top-join-line)


;; http://emacsredux.com/blog/page/5/
;; C-a Initially took you to the first non-whitespace char on a line,
;; and if pressed again took you to the actual beginning of the line.
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)



;;
;; *******************Emacs Shortcut keys***************
;;

;; disabling shortcut to kill emacs.
(defun dont-kill-emacs ()
      (interactive)
      (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

    (global-set-key "\C-x\C-c" 'dont-kill-emacs)


;; Magic key combinations key chord
(use-package key-chord
  :ensure key-chord)
(require 'key-chord)
(key-chord-define-global "jj" 'ace-jump-char-mode)
(key-chord-define-global "kk" 'ace-jump-buffer)
(key-chord-define-global "gg" 'helm-swoop)
(key-chord-define-global "hh" 'helm-do-grep-recursive)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-mode +1)


;; Sort lines (ie. package imports or headers).
(global-set-key (kbd "M-s l") 'sort-lines)


;; From http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-7-3
;; Transpose stuff with M-t
(bind-key "M-t" nil) ;; which used to be transpose-words
(bind-key "M-t l" 'transpose-lines)
(bind-key "M-t w" 'transpose-words)
(bind-key "M-t t" 'transpose-words)
(bind-key "M-t M-t" 'transpose-words)
(bind-key "M-t s" 'transpose-sexps)

;; Setting up the backspace key
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;;"Ctrl-k" kills an entire line if the cursor is at the beginning of line
(setq kill-whole-line t)


;; New browser tab.
;;make chrome the default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(cond
 ((string-equal system-type "darwin") ; Mac OS X
    (defun new-browser-tab ()
      "Open a new browser tab in the default browser."
      (interactive)
      (shell-command "open http://google.com")
      ))
 ((string-equal system-type "gnu/linux") ; Linux
    (defun new-browser-tab ()
      "Open a new browser tab in the default browser."
      (interactive)
      (shell-command "google-chrome http://google.com")
      ))
 )
(global-set-key (kbd "C-x t") 'new-browser-tab)



;; Emacs Indentation
;;

;; setting up python settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; css and js indentation
(setq css-indent-offset 2)
(setq js-indent-level 2)

;; all indentation can be made from spaces only
(setq-default indent-tabs-mode nil)


;; Indentation tab
;; Deletes trailing whitespaces after saving
(add-hook 'write-file-hooks 'delete-trailing-whitespace)



;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])



;;
;; *******Handy Functions*******
;;


;; Thank you Xah Lee.
;; from http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app. The app is chosen from your OS's preference."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )
(global-set-key (kbd "C-M-o") 'open-in-external-app)

;; Pretty print XML
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))


;; Kill all other buffers
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; Pretty printing JSON
(defun pretty-print-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))


;; Copying file name in clipboard
(defun copy-file-name-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


;; Copying the current line.
;; Called when there is no active selection, so M-w with no selection copies the
;; whole line, so cool.
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
                                                                       "Copied line") (list (line-beginning-position) (line-beginning-position
                                                                                                                       2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
                   (line-beginning-position 2)))))



;; Rename file and buffer.
;; From: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn  (rename-file name new-name 1)  (rename-buffer new-name)  (set-visited-file-name new-name)  (set-buffer-modified-p nil)))))) ;;

;; Move buffer file.
;; From: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1) (delete-file filename) (set-visited-file-name newname) (set-buffer-modified-p nil) t))))




;;
;; *************C++ mode******************
;;

;; Google specific stufff
(require 'google)
;; cpp lint integration (if you see errors do prodaccess)
(global-set-key "\C-cl" 'google-lint)


;; open header file as c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Google c++ mode
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))


;; clang-format
;; Needs clang-format installed.
;; See http://blog.hardcodes.de/articles/63/building-clang-format-and-friends-on-osx-mountain-lion
;; See http://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format
  :ensure clang-format)
(global-set-key (kbd "C-c t") 'clang-format-region)


;; Expand Member Functions (does not seem to like the include files.)
(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" #'expand-member-functions)))


;; **********JS Javascript********

(use-package js2-mode
  :commands js2-mode
  :init
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; (setq js2-basic-offset 2
;;       js2-bounce-indent-p t)


;; js2-mode

(defun google-coding-style/indent-anchor-point ()
  "Returns the position of the first nonblank character on the closest previous
nonblank line.
If no such character can be found, returns nil."
  (save-excursion
    (let (return-value)
      (while (and (not return-value)
                  (>= (forward-line -1) 0))
        (when (not (looking-at "^\\s *$"))
            (back-to-indentation)
            (setq return-value (point))))
      return-value)))

(defun google-coding-style/js-indent-lambda-point (parse-status)
  "Returns the position of the beginning of the expression that we should indent
respective to.
If the point is not inside such a lambda construct, returns nil.
PARSE-STATUS is the value of `parse-partial-sexp' at point, passed down from the
caller for efficiency."
  (save-excursion
    (back-to-indentation)
    (let (after-last-paren-open beginning-of-lambda-line)
      (if (and parse-status (nth 1 parse-status)
               (setq after-last-paren-open (1+ (nth 1 parse-status)))
               (goto-char after-last-paren-open)
               (setq beginning-of-lambda-line (point-at-bol))
               (re-search-backward "function\\s *([^);{}]*)\\s *{"
                                   beginning-of-lambda-line t)
               (= (match-end 0) after-last-paren-open)
               (goto-char (match-beginning 0))
               ;; Move back to beginning of expression...
               (progn (js2-mode-forward-sexp -1) t)
               ;; ... and double-check that we are still on the same line:
               (>= (point) beginning-of-lambda-line))
          (point)))))

(defun google-js-proper-indentation (parse-status)
  "Returns the proper indentation for the current line or nil to let the default
function `js-proper-indentation' decide.  Should NOT move point.
PARSE-STATUS is the value of `parse-partial-sexp' at point, passed down from the
caller for efficiency."
  (save-excursion
    (back-to-indentation)
    (let ((indent-anchor (google-coding-style/indent-anchor-point))
          (lambda-point
           (google-coding-style/js-indent-lambda-point parse-status)))
      (cond
       ;; Under anonymous lambdas, indent relative to the containing expression
       ((and indent-anchor lambda-point (>= lambda-point indent-anchor))
        (progn (goto-char lambda-point) (+ js2-basic-offset (current-column))))
       ;; Closing construct of same
       ((and (looking-at "}") lambda-point)
        (progn (goto-char lambda-point) (current-column)))
       ))))

(defadvice js-proper-indentation (around google-coding-style-js-indent)
  "Ask `google-js-proper-indentation' first."
  (unless (setq ad-return-value (google-js-proper-indentation (ad-get-arg 0)))
    ad-do-it))

(defun google-set-js2-style ()
  "Configures js2-mode for compliance with the JavaScript Style Guide"
  (interactive)
  (setq indent-tabs-mode nil)
  (setq js2-basic-offset 2)
  (google-coding-style/set-doc-style)
  (ad-activate 'js-proper-indentation))

(add-hook 'js2-mode-hook 'google-set-js2-style)


;; js-mode

(defun google-set-js-style ()
  "Configures Emacs' built-in js-mode for compliance with the
JavaScript Style Guide."
  (interactive)
  (setq js-indent-level 2)
  (google-coding-style/set-doc-style))

(add-hook 'js-mode-hook 'google-set-js-style)


(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;;
;; python-mode
;;

;; python-mode

;; https://github.com/msparks/dotfiles/blob/master/.emacs-lisp/google-coding-style.el

(defadvice python-calculate-indentation (around calculate-with-google-style)
    "Modify Python indentation to match the Google style guide.
Currently, there are two cases in particular that this
handles. Both have to do with multi-line parenthesized
expressions.  In the first case, the default indentation for
such expressions is `python-indent', but Google mandates
`python-continuation-offset' instead. For example:
  function(
      arg_indented_four_spaces)
In the second case, when doubly-nested multi-line parenthesized
expressions exist, Google mandates that the second nested
expression be indented relative to the first, as so:
  function('string' % (
               'format indented relative to first arg'))"
    (unless
        (block nil
          (setq python-indent-list nil
                python-indent-list-length 1)
          ;; These conditions are all taken directly from python.el.
          ;; Not great for code reuse, but probably better than copying
          ;; the entire function.
          (save-excursion
            (beginning-of-line)
            (let* ((syntax (syntax-ppss))
                   (point (point))
                   (open-start (cadr syntax)))
              (when (eq 'string (syntax-ppss-context syntax)) (return))
              (unless (python-continuation-line-p) (return))
              (unless open-start (return))
              (goto-char (1+ open-start))
              (when (with-syntax-table python-space-backslash-table
                      (let ((parse-sexp-ignore-comments t))
                        (condition-case ()
                            (progn (forward-sexp)
                                   (backward-sexp)
                                   (< (point) point))
                          (error nil))))
                (return))
              (goto-char (1+ open-start))
              (setq ad-return-value
                    (if (eolp)
                        (+ (current-indentation) python-continuation-offset)
                      (current-indentation))))))
      ;; If we don't meet the conditions for the Google workarounds,
      ;; use the normal definition.
      ad-do-it))

;; Okay we'll use 2 everywhere for Python
(defun google-set-python-style ()
  (setq py-indent-offset 2)  ; For the third_party python-mode.el

  ;; For GNU Emacs' python.el
  (setq python-indent 2)
  (when (fboundp 'python-calculate-indentation)
    (ad-activate 'python-calculate-indentation)))

(add-hook 'python-mode-hook 'google-set-python-style)




;; CSS mode


(defgroup css nil
  "Customizations for editing Cascading Style Sheets"
  :group 'languages)

(defcustom css-mode-hook nil
  "*Hook to be run when `css-mode' is entered."
  :group 'css
  :type  'hook)

(defcustom css-electric-semi-behavior nil
  "If non-nil semicolons are electric in css mode"
  :group 'css
  :type  'boolean)

(defcustom css-electric-brace-behavior nil
  "If non-nil braces are electric in css mode"
  :group 'css
  :type  'boolean)

(defcustom css-indent-offset 4
  "Number of spaces to indent lines in CSS mode"
  :group 'css
  :type  'integer)

(defcustom css-tab-mode 'auto
  "Behavior of tab in CSS mode"
  :group 'css
  :type  '(choice (const :tag "Always insert" insert)
                  (const :tag "Always indent" indent)
                  (const :tag "Always complete" complete)
                  (const :tag "Auto" auto) ))

(defvar css-mode-abbrev-table nil
  "Abbreviation table used in `css-mode' buffers.")
(define-abbrev-table 'css-mode-abbrev-table ())


(defvar css-at-rule-keywords nil
  "Keywords for CSS at rules" )
(if css-at-rule-keywords nil
  (setq css-at-rule-keywords
        '("import" "media" "page" "font-face" "charset") ))

(defvar css-at-rule-table nil
  "Table for CSS at rules" )
(if css-at-rule-table nil
  (setq css-at-rule-table (make-vector 5 0))
  (mapcar (lambda (x) (intern x css-at-rule-table))
          css-at-rule-keywords ))

(defvar css-element-keywords nil
  "Common CSS elements" )
(if css-element-keywords nil
  (setq css-element-keywords
        '("A" "ADDRESS" "B" "BLOCKQUOTE" "BODY" "BR" "CITE"
          "CODE" "DIR" "DIV" "DD" "DL" "DT" "EM" "FORM" "H1"
          "H2" "H3" "H4" "H5" "H6" "HR" "I" "IMG" "KBD" "LI"
          "MENU" "OL" "P" "PRE" "SAMP" "SPAN" "STRONG" "TABLE"
          "TR" "TH" "TD" "TT" "UL" "VAR" )))

(defvar css-element-table nil
  "Table for CSS elements" )
(if css-element-table nil
  (setq css-element-table (make-vector 5 0))
  (mapcar (lambda (x) (intern x css-element-table))
          css-element-keywords ))


(defvar css-property-keywords nil "CSS properties" )
(if css-property-keywords nil
  (setq css-property-keywords
'("azimuth" "background" "background-attachment" "background-color"
  "background-image" "background-position" "background-repeat" "border"
  "border-collapse" "border-color" "border-spacing" "border-style"
  "border-top" "border-right" "border-bottom" "border-left"
  "border-top-color" "border-right-color" "border-bottom-color"
  "border-left-color" "border-top-style" "border-right-style"
  "border-bottom-style" "border-left-style" "border-top-width"
  "border-right-width" "border-bottom-width" "border-left-width"
  "border-width" "bottom" "caption-side" "clear" "clip" "color"
  "content" "counter-increment" "counter-reset" "cue" "cue-after"
  "cue-before" "cursor" "direction" "display" "elevation" "empty-cells"
  "float" "font" "font-family" "font-size" "font-size-adjust"
  "font-stretch" "font-style" "font-variant" "font-weight" "height"
  "left" "letter-spacing" "line-height" "list-style" "list-style-image"
  "list-style-position" "list-style-type" "margin" "margin-top"
  "margin-right" "margin-bottom" "margin-left" "marker-offset" "marks"
  "max-height" "max-width" "min-height" "min-width" "orphans" "outline"
  "outline-color" "outline-style" "outline-width" "overflow" "padding"
  "padding-top" "padding-right" "padding-bottom" "padding-left" "page"
  "page-break-after" "page-break-before" "page-break-inside" "pause"
  "pause-after" "pause-before" "pitch" "pitch-range" "play-during"
  "position" "quotes" "richness" "right" "size" "speak" "speak-header"
  "speak-numeral" "speak-punctuation" "speech-rate" "stress"
  "table-layout" "text-align" "text-decoration" "text-indent"
  "text-shadow" "text-transform" "top" "unicode-bidi" "vertical-align"
  "visibility" "voice-family" "volume" "white-space" "widows" "width"
  "word-spacing" "z-index" )))

(defvar css-property-table nil
  "Table for CSS properties" )
(if css-property-table nil
  (setq css-property-table (make-vector 5 0))
  (mapcar (lambda (x) (intern x css-property-table))
          css-property-keywords ))


;; Three levels of highlighting

(defconst css-font-lock-keywords-1 nil
  "Subdued level highlighting for C modes.")

(defconst css-font-lock-keywords-2 nil
  "Medium level highlighting for C modes.")

(defconst css-font-lock-keywords-3 nil
  "Gaudy level highlighting for C modes.")

(defvar css-font-keywords nil
  "Font lock keywords for `css-mode'." )

(let* ((css-keywords  "\\(url\\|![ \t]*important\\)")
       (css-nmstart   "[a-zA-Z]")
       (css-nmchar    "[a-zA-Z0-9-]")
       (css-ident     (concat css-nmstart css-nmchar "*"))
       (css-at-rule   (concat "\\(@" css-ident "\\)"))
       (css-element-s (concat "^\\(" css-ident "\\)"))
       (css-element (concat "\\(?:[,+>][ \t]*\\)\\(" css-ident "\\)"))
       (css-class  (concat css-element "?\\.\\(" css-ident "\\)"))
       (css-pseudo (concat ":\\(" css-ident "\\)"))
       (css-attr (concat "\\[\\(" css-ident "\\)\\]"))
       (css-id (concat "#\\(" css-ident "\\)"))
       (css-declaration (concat "[ \t][ \t]*\\(\\<" css-ident "\\>\\):")) )
  (setq css-font-lock-keywords-1
   (list
    (list css-keywords    1 'font-lock-keyword-face)
    (list css-at-rule     1 'font-lock-keyword-face)
    (list css-element-s   1 'font-lock-function-name-face)
    (list css-element     1 'font-lock-function-name-face)
    (list css-class       2 'font-lock-type-face)
    (list css-pseudo      1 'font-lock-constant-face)
    (list css-attr        1 'font-lock-variable-name-face)
    (list css-id          1 'font-lock-string-face)
    (list css-declaration 1 'font-lock-variable-name-face) ))
  (setq css-font-lock-keywords-2 css-font-lock-keywords-1)
  (setq css-font-lock-keywords-3 css-font-lock-keywords-2) )

(defvar css-mode-syntax-table nil
  "Syntax table used in `css-mode' buffers.")

(if css-mode-syntax-table nil
  (setq css-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?+  "."    css-mode-syntax-table)
  (modify-syntax-entry ?=  "."    css-mode-syntax-table)
  (modify-syntax-entry ?<  "."    css-mode-syntax-table)
  (modify-syntax-entry ?>  "."    css-mode-syntax-table)
  (modify-syntax-entry ?-  "w"    css-mode-syntax-table)
  (modify-syntax-entry ?/  "w"    css-mode-syntax-table)
  (modify-syntax-entry ?.  "w"    css-mode-syntax-table)
  (modify-syntax-entry ?\' "\""   css-mode-syntax-table)
  (cond
   ;; XEmacs 19 & 20
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" css-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   css-mode-syntax-table))
   ;; Emacs 19 & 20
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" css-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   css-mode-syntax-table))
   ;; incompatible
   (t (error "CSS Mode is incompatible with this version of Emacs")) )
  (modify-syntax-entry ?\n "> b"  css-mode-syntax-table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?\^m "> b" css-mode-syntax-table) )


(defvar css-mode-map nil
  "Keymap used in `css-mode' buffers.")

(if css-mode-map nil
  (setq css-mode-map (make-sparse-keymap))
  (define-key css-mode-map ";"        'css-electric-semicolon)
  (define-key css-mode-map "{"        'css-electric-brace)
  (define-key css-mode-map "}"        'css-electric-brace)
  (define-key css-mode-map "\t"       'css-tab-function)
  (define-key css-mode-map "\C-c\C-c" 'css-comment-region)
  (define-key css-mode-map "\C-c\C-a" 'css-complete-at-keyword)
  (define-key css-mode-map "\C-c\C-e" 'css-complete-element)
  (define-key css-mode-map "\C-c\C-p" 'css-complete-property) )


;;; Utility functions

(defun css-in-comment-p ()
  "Check whether we are currently in a comment"
  (let ((here (point)))
    (and (search-backward "/*" nil t)
         (prog1
             (not (search-forward "*/" here t))
           (goto-char here) ))))


(defun css-complete-symbol (&optional table predicate prettify)
  (let* ((end (point))
   (beg (save-excursion
    (skip-syntax-backward "w")
    (point)))
   (pattern (buffer-substring beg end))
   (table (or table obarray))
   (completion (try-completion pattern table predicate)))
    (cond ((eq completion t))
    ((null completion)
     (error "Can't find completion for \"%s\"" pattern))
    ((not (string-equal pattern completion))
     (delete-region beg end)
     (insert completion))
    (t
     (message "Making completion list...")
     (let ((list (all-completions pattern table predicate)))
       (if prettify
     (setq list (funcall prettify list)))
       (with-output-to-temp-buffer "*Help*"
         (display-completion-list list)))
     (message "Making completion list...%s" "done")))))


(defun css-indent-line ()
  "Indent the current line"
  (if (or (css-in-comment-p)
          (looking-at "[ \t]*/\\*") )
      nil
    (save-excursion
      (let ((here (point))
            (depth 0))
        (while (and (forward-line -1)
                    (or (looking-at "^[ \t]*$")
                        (css-in-comment-p) ))
          ; Jump to a non comment/white-space line
          )
        (cond ((looking-at "\\([ \t]*\\)\\([^ \t].*\\)?{[ \t]*$")
               (setq depth (+ (- (match-end 1) (match-beginning 1))
                              css-indent-offset )))
              ((looking-at "\\([ \t]*\\)[^ \t]")
               (setq depth (- (match-end 1) (match-beginning 1))) )
              (t (setq depth 0)) )
        (goto-char here)
        (beginning-of-line)
        (if (looking-at "[ \t]*}")
            (setq depth (max (- depth css-indent-offset) 0)) )
        (if (looking-at "\\([ \t]*\\)")
            (if (= depth (- (match-end 1) (match-beginning 1)))
                nil
              (delete-region (match-beginning 1) (match-end 1))
              (indent-to depth))
          (if (> depth 0)
              (indent-to depth)))))
    (if (looking-at "[ \t]*")
        (end-of-line) )))


(defun css-indent-region (start end)
  "Indent the current line"
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (and (not (eobp)) (forward-line 1))
        (css-indent-line) ))))


;;; Commands

(defun css-electric-semicolon (arg)
  "Insert a semi-colon, and possibly indent line.
If numeric argument is not given, or is 1, auto-indent according to
`css-electric-semi-behavior'.  If arg is 0, do not auto-indent, if
arg is 2 always auto-indent, and if arg is anything else invert the
usual behavior."
  (interactive "P")
  ;; insert a semicolon
  (self-insert-command 1)
  ;; maybe do electric behavior
  (or (css-in-comment-p)
      (and (eq arg 1)
           css-electric-semi-behavior
           (css-indent-line) )
      (and (eq arg 2)
           (css-indent-line) )
      (eq arg 0)
      (or (not css-electric-semi-behavior)
          (css-indent-line) )))


(defun css-electric-brace (arg)
  "Insert a brace, and possibly indent line.
If numeric argument is not given, or is 1, auto-indent according to
`css-electric-brace-behavior'.  If arg is 0, do not auto-indent, if
arg is 2 always auto-indent, and if arg is anything else invert the
usual behavior."
  (interactive "P")
  ;; insert a brace
  (self-insert-command 1)
  ;; maybe do electric behavior
  (or (css-in-comment-p)
      (and (eq arg 1)
           css-electric-brace-behavior
           (css-indent-line) )
      (and (eq arg 2)
           (css-indent-line) )
      (eq arg 0)
      (or (not css-electric-brace-behavior)
          (css-indent-line) )))

(defun css-complete-at-keyword ()
  "Complete the standard element at point"
  (interactive)
  (let ((completion-ignore-case t))
    (css-complete-symbol css-at-rule-table) ))

(defun css-complete-element ()
  "Complete the standard element at point"
  (interactive)
  (let ((completion-ignore-case t))
    (css-complete-symbol css-element-table) ))

(defun css-complete-property ()
  "Complete the standard element at point"
  (interactive)
  (let ((completion-ignore-case t))
    (css-complete-symbol css-property-table) ))


(defun css-tab-function (&optional arg)
  "Function to call when tab is pressed in CSS mode.
With a prefix arg, insert a literal tab.  Otherwise behavior depends
on the value of `css-tab-mode'.  If it's 'insert, insert a literal
tab.  If it's 'indent, indent the current line, and if it's 'complete,
try to complete the expression before point.  A value of 'auto means
to inspect the current line, and indent if point is at the beginning
or end of the line, but complete if it's at a word.
There are three possible completions to perform:
`css-complete-at-keyword' if the point is after an '@',
`css-complete-property' if point is inside a block, and
`css-complete-element' otherwise."
  (interactive "P")
  (let* ((end (point))
         (start (prog2
                    (beginning-of-line)
                    (point)
                  (goto-char end) ))
         (prefix (buffer-substring start end)) )
    (cond ((or arg (eq css-tab-mode 'insert))
           (insert "\t"))
          ((eq css-tab-mode 'indent)
           (css-indent-line))
          ((and (not (eq css-tab-mode 'complete))
                (or (string-match "^[ \t]*[{}]?[ \t]*$" prefix)
                    (string-match "^.*;[ \t]*" prefix) ))
           ;; indent at the beginning or end of a line
           (css-indent-line))
          ((string-match "^.*@[a-zA-Z0-9-]*$" prefix)
           (css-complete-at-keyword))
          ((string-match "^\\([ \t]+.*\\|.*\{[ \t]*[a-zA-Z]+\\)$" prefix)
           ;; complete properties on non-starting lines
           (css-complete-property))
          ;; otherwise try an element
          (t (css-complete-element)) )))


;;;###autoload
(defun css-mode ()
  "Major mode for editing CSS files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table css-mode-syntax-table)
  (setq major-mode 'css-mode
  mode-name "CSS"
  local-abbrev-table css-mode-abbrev-table)
  (use-local-map css-mode-map)
  ;; local variables
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'block-comment-start)
  (make-local-variable 'block-comment-end)
  (make-local-variable 'block-comment-left)
  (make-local-variable 'block-comment-right)
  (make-local-variable 'block-comment-top-right)
  (make-local-variable 'block-comment-bot-left)
  (make-local-variable 'block-comment-char)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((css-font-lock-keywords-1
                              css-font-lock-keywords-2
                              css-font-lock-keywords-3)))
  ;; now set their values
  (setq parse-sexp-ignore-comments t
  comment-start-skip "/\\*+ *\\|// *"
  comment-start "/\\*"
  comment-end   "\\*/")
  (setq block-comment-start     "/*"
        block-comment-end       "*/"
        block-comment-left      " * "
        block-comment-right     " *"
        block-comment-top-right ""
        block-comment-bot-left  " "
        block-comment-char      ?* )
  (setq indent-line-function   'css-indent-line
        indent-region-function 'css-indent-region
  paragraph-ignore-fill-prefix t
  paragraph-start (concat "\\|$" page-delimiter)
  paragraph-separate paragraph-start)
  (run-hooks 'css-mode-hook))


;; *************markdown******************
(use-package markdown-mode+
  :ensure markdown-mode+)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))



;;
;; *************GIT and source control************
;;
;; magit keybinding
(use-package magit
  :ensure magit)
(put 'upcase-region 'disabled nil)
(eval-after-load "vc-hooks"
  '(define-key vc-prefix-map "=" 'vc-ediff))
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-status-buffer-switch-function 'switch-to-buffer)


;; Highlight git hunks.
(use-package git-gutter
  :ensure git-gutter)
(global-git-gutter-mode +1)
;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Handy pop-up messages with git info.
(use-package git-messenger
  :ensure git-messenger)

(use-package git-timemachine
  :ensure git-timemachine)

;;
;; ediff settings
;;
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; ediff-revision cleanup.
;; From http://www.emacswiki.org/emacs/DavidBoon#toc8
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (window-configuration-to-register my-ediff-bwin-reg))

(defun my-ediff-aswh ()
  "setup hook used to remove the `ediff-cleanup-mess' function.  It causes errors."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (ediff-cleanup-mess)
  (jump-to-register my-ediff-bwin-reg))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-aswh);
(add-hook 'ediff-quit-hook 'my-ediff-qh)



;; Shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
