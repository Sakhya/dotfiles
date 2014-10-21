;; EMACS CONFIG FROM UBUNTU


;;https://github.com/xenodium/dotfiles/blob/master/emacs/init.el
;;
;; Add melpa package repository.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

;; (use-package async
;;   :ensure async)
;; (require 'async-bytecomp)

(use-package bind-key
  :ensure bind-key)


(use-package hackernews
  :ensure hackernews)

;; (use-package rainbow-delimiters
;;   :ensure rainbow-delimiters)
;; (global-rainbow-delimiters-mode)


(use-package expand-region
  :ensure expand-region)
(global-set-key (kbd "C-c w") 'er/expand-region)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;; Language-aware editing commands. Useful for imenu-menu.
(semantic-mode 1)



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
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
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


(use-package ido-vertical-mode
  :ensure ido-vertical-mode)
(ido-vertical-mode)

;; ggtags code indexing.
;; https://github.com/leoliu/ggtags
;; https://github.com/leoliu/ggtags/wiki/Install-Global-with-support-for-exuberant-ctags
;; Linux
;; Install exuberant ctags from trunk.
;; Install GNU Global using ./configure --with-exuberant-ctags=PATH_TO_CTAGS_EXECUTABLE
;; Mac OS
;; brew install --HEAD ctags
;; brew install global --with-exuberant-ctags





;; Automatically closes brackets.
(use-package smartparens
  :ensure smartparens)
(smartparens-global-mode)
(show-smartparens-global-mode +1)
(electric-indent-mode)


;; Sort lines (ie. package imports or headers).
(global-set-key (kbd "M-s l") 'sort-lines)



;; Best way (so far) to search for files in repo.
(projectile-global-mode)


;; Best way (so far) to search for files in repo.
(use-package helm-projectile
   :ensure helm-projectile)
(require 'helm-projectile)
(global-set-key (kbd "C-x f") 'helm-projectile)






;; full screen emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; show line numbers
(global-linum-mode t)
;; turn on highlighting
(global-hl-line-mode t)
(show-paren-mode t)
;; Show column-number in the mode line
(column-number-mode 1)

;; setting up python settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;; Saving customizations in a different file
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; Load path for extensions
(add-to-list 'load-path "~/.emacs.d")

;; ----keyboard settings----
;; setting up the backspace key
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;;"Ctrl-k" kills an entire line if the cursor is at the beginning of line
(setq kill-whole-line t)

;; Indentation tab
;; Deletes trailing whitespaces after saving
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; just y for yes
(fset 'yes-or-no-p 'y-or-n-p)

;; will inhibit startup messages.
(setq inhibit-startup-message t)

;; syntax highlighting by default
(global-font-lock-mode 1)


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

;; ----Whitespace mode----
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(tabs tab-mark trailing))



;; all indentation can be made from spaces only
(setq-default indent-tabs-mode nil)

;; css and js indentation
(setq css-indent-offset 2)
(setq js-indent-level 2)


;; ;; splitting vertically default
;; (setq split-height-threshold 0)
;; (setq split-width-threshold nil)
;; pretty printing JSON
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))


;; sorting out html indentation
(add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)))





;; C++ mode
;; Shortcut key to load the header file C++
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

;; open header file as c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Google c++ mode
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; (add-to-list 'load-path "~/extensions/google-c-style.el")
;; (require 'google-c-style)
;; (load "google-c-style.el")
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)
;; (add-hook 'c-mode-common-hook '(lambda () (c-set-style "google")))

;; TODO(sakhyaghosh):
;; Python Hook 2 spaces
;; (add-hook 'python-mode-hook
;;       (lambda ()
;;         (setq indent-tabs-mode t)
;;         (setq tab-width 2)
;;         (setq python-indent 2)))

;; ;; tells you which function you are in
;; (which-function-mode 1)


;;will let emacs put in a "carriage-return" for you automatically after left curly braces, righ curly braces, and semi-colons in "C mode"
;;(setq c-auto-newline 1)


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

;; ;; Develop and keep personal snippets under /snippets
;; (setq yas/root-directory '("~/snippets"))
;; (mapc 'yas/load-directory yas/root-directory)

(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/snippets")))

(yas-global-mode 1)
(yas--initialize)

;; ;; automatically reload snippets after saving:
;; (defun reload-snippets ()
;;   (interactive)
;;   (yas-reload-all)
;;   (yas-recompile-all)
;;   (yas-reload-all)
;;   (yas-recompile-all)
;;   )
;; (defun snippet-mode-before-save ()
;;   (interactive)
;;   (when (eq major-mode 'snippet-mode) (reload-snippets)))
;; (add-hook 'after-save-hook 'snippet-mode-before-save)')))


;; (require 'yasnippet)
;; (setq yas-snippet-dirs
;;       '("~/extensions/snippets"            ;; personal snippets
;;         "~/.emacs.d/elpa/yasnippet-0.8.0/snippets"    ;; the default collection
;;         ))
;; (yas-global-mode 1)

;; (setq yas/root-directory "/usr/local/google/home/sakhyaghosh/extensions/snippets")
;; (yas/load-directory yas/root-directory)
;;(setq yas-snippet-dirs "/usr/local/google/home/sakhyaghosh/extensions/snippets")

;; (add-hook 'the-major-mode-hook 'yas/minor-mode-on)

;; (add-to-list 'load-path "/Users/sakhyaghosh/extensions/yasnippet.el")
;; (require 'yasnippet)
;; (setq yas/root-directory "/Users/sakhyaghosh/extensions/snippets")
;; (yas/load-directory yas/root-directory)

;; ;; defining space for yasnippet expansion
;; (define-key yas-minor-mode-map (kbd "C-c SPC") 'yas/expand)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)







;; ----IDO mode----
;; (setq ido-enable-flex-matching t)
;;   (setq ido-everywhere t)
;;   (ido-mode 1)


;; Auto revert files in emacs
(global-auto-revert-mode t)


;; Automatically save and restore sessions
;; (desktop-save-mode 1)

;; showing the file name
(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))

;; copying file name in clipboard
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


;; copying the current line
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

;; pretty printing JSON
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;; kill all other buffers
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))


;; ----80 chars----
;; Turn on red highlighting for characters outside of the 80/100 char limit
;; Fill columns
(setq-default fill-column 80)

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)


;; ;; dictionary
;; (set-variable 'debian-ispell-dictionary "american")

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
;; (add-hook 'matlab-mode-hook
;;           '(lambda () (flyspell-prog-mode)))


;; load zenburn by defailt
(defun zenburn-init ()
  (load-theme 'zenburn)
)

(add-hook 'after-init-hook 'zenburn-init)


;; helps to copy paste in emacs
;;
(load-file "~/.emacs.d/xclip.el")

;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; (global-set-key "\C-w" 'clipboard-kill-region)
;; (global-set-key "\M-w" 'clipboard-kill-ring-save)
;; (global-set-key "\C-y" 'clipboard-yank)


;; Google specific stufff
(require 'google)
;; automatic boilerplate
(add-hook 'find-file-not-found-hooks 'autogen)
;; cpp lint integration
(global-set-key "\C-cl" 'google-lint)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; create the autosave dir if necessary, since emacs won't.
;;(make-directory "~/.emacs.d/autosaves/" t)
;;(make-directory "~/.emacs.d/backups/" t)

;; Make sure all backup files only live in one place

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;;make chrome the default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
;;allow navigation of dashcase/camelCase etc word at a time( awesome for deleting parts of  a variable name Hit Alt+D and it will delete only a subword.
(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))


;; pretty print XML
(defun bf-pretty-print-xml-region (begin end)
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

;; Highlight current line
(require 'hl-line)
(global-hl-line-mode 1)
;; Set any color as the background face of the current line:
(set-face-background 'hl-line "black")

;;To keep syntax highlighting in the current line:
(set-face-foreground 'highlight nil)


(setq ring-bell-function 'ignore)

;; ----TODO----
(require 'fic-mode)
  (add-hook 'prog-mode-hook 'turn-on-fic-mode)

;; ----Package emacs 24---

;; Install some repos for emacs packages.
(eval-after-load 'package
  '(add-to-list 'package-archives
                '("GELPA" . "http://internal-elpa.appspot.com/packages/")))
;;install marlmalade repo
(eval-after-load 'package
  '(add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/")))

;; install melpa repo
(eval-after-load 'package
  '(add-to-list 'package-archives
                '("melpa" . "http://melpa.milkbox.net/packages/") t))
(package-initialize)
; (require 'package)
; (add-to-list 'package-archives
;   '("melpa" .
;     "http://melpa.milkbox.net/packages/"))
; (add-to-list 'package-archives
;   '("marmalade" .
;     "http://marmalade-repo.org/packages/"))
; (package-initialize)


;; HorizontalSplitting
(setq split-height-threshold nil)
(setq split-width-threshold 9999)

;; ediff split vertical
(setq ediff-split-window-function 'split-window-horizontally)

;; magit keybinding
(global-set-key (kbd "C-x g") 'magit-status)
(put 'upcase-region 'disabled nil)


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
