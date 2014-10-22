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
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;
;; use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)


;; Saving customizations in a different file
(setq custom-file "~/.emacs-custom.el")
(load custom-file)
;; Load path for extensions
(add-to-list 'load-path "~/.emacs.d")


;;
;; asynchronous processing
(use-package async
   :ensure async)
(require 'async-bytecomp)

;; easier key binding
(use-package bind-key
  :ensure bind-key)



;; Hungry Delete
;;
;; (use-package hungry-delete
;;   :ensure hungry-delete)
;; (global-hungry-delete-mode)
;; (global-auto-revert-mode)
;; (global-font-lock-mode)




;; Recent files.
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)


;; Language-aware editing commands. Useful for imenu-menu.
(semantic-mode 1)


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

(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "M-C-s") 'helm-swoop)
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


;; Projectile
;; Best way (so far) to search for files in repo.
(projectile-global-mode)
(use-package helm-projectile
   :ensure helm-projectile)
(require 'helm-projectile)
(global-set-key (kbd "C-x f") 'helm-projectile)


;; IDO vertical mode.
;;
(use-package ido-vertical-mode
  :ensure ido-vertical-mode)
(ido-vertical-mode)
;; using helm so IDO is disabled.
;; (setq ido-enable-flex-matching t)
;;   (setq ido-everywhere t)
;;   (ido-mode 1)


;; Automatically closes brackets.
(electric-pair-mode)
(electric-indent-mode)

;; Put "carriage-return" for you automatically after left curly braces,
;; right curly braces, and semi-colons in "C mode".
;;(setq c-auto-newline 1)


;; Sort lines (ie. package imports or headers).
(global-set-key (kbd "M-s l") 'sort-lines)




;;
;; Look and Feel
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

;; Highlight current line
(require 'hl-line)
(global-hl-line-mode 1)
;; Set any color as the background face of the current line:
(set-face-background 'hl-line "black")
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


;; HorizontalSplitting
(setq split-height-threshold nil)
(setq split-width-threshold 9999)



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
(setq-default fill-column 80)
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)




;;
;; Emacs Navigation
;;

;; Expand region (better selection)
(use-package expand-region
  :ensure expand-region)
(global-set-key (kbd "C-c w") 'er/expand-region)


;; CamelCase Navigation.
;; Enabling subword mode (ie. navigate cameCase)
;; From http://www.emacswiki.org/emacs/CamelCase
(global-subword-mode t)
;; (add-hook 'c-mode-common-hook
;;           (lambda () (subword-mode 1)))





;;
;; Emacs Shortcut keys
;;


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
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;; css and js indentation
(setq css-indent-offset 2)
(setq js-indent-level 2)






;; ----keyboard settings----

;; Indentation tab
;; Deletes trailing whitespaces after saving
(add-hook 'write-file-hooks 'delete-trailing-whitespace)


;; ----Default Directory----
;; set default
(setq default-directory "~/")


;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])


;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))


;; .json files
(setq auto-mode-alist (cons '("\\.json\\'" . js-mode) auto-mode-alist))
;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;; all indentation can be made from spaces only
(setq-default indent-tabs-mode nil)



;; sorting out html indentation
(add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)))








;; ----Uniquify----
;; uniquely name the buffers so that it has filenames
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'forward
  uniquify-separator "/")

(setq frame-title-format
      '("%S" (buffer-file-name "%f"
                   (dired-directory dired-directory "%b"))))


;;  ----Window movement----
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)



;; Autocomplete
;; Load the default configuration
(require 'auto-complete-config)
;; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories "~/emacs/auto-complete/dict")
;; Load by default
(ac-config-default)
;; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
;; (global-auto-complete-mode t)
;; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
;; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; auto complete clang
(require 'auto-complete-clang)
(global-set-key (kbd "C-`") 'ac-complete-clang)


;;
;; ----yasnippet----

(use-package yasnippet
  :ensure yasnippet)

;; Develop and keep personal snippets under ~/snippets
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/snippets")))
(yas-global-mode 1)
(yas--initialize)







;; showing the file name
(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))




;; Helps to copy paste in emacs
;;
;; http://stackoverflow.com/questions/5288213/how-can-i-paste-the-selected-region-outside-of-emacs
;; sudo apt-get install xclip
(use-package xclip
  :ensure xclip)
(xclip-mode 1)


;; Google specific stufff
(require 'google)
;; automatic boilerplate
(add-hook 'find-file-not-found-hooks 'autogen)
;; cpp lint integration
(global-set-key "\C-cl" 'google-lint)






;;
;; *******Handy Functions*******
;;


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
(defun my-put-file-name-on-clipboard ()
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
;; Shortcut key to load the header file C++
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

;; open header file as c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Google c++ mode
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;; clang-format
;; Needs clang-format installed.
;; See http://blog.hardcodes.de/articles/63/building-clang-format-and-friends-on-osx-mountain-lion
;; See http://clang.llvm.org/docs/ClangFormat.html
(use-package clang-format
  :ensure clang-format)
(global-set-key (kbd "C-c t") 'clang-format-region)





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
