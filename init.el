;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(python
     rust
     html
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     org
     (shell :variables
             shell-default-height 30
             shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     version-control
     ;;Nyan Cat 彩虹猫，用于显示文件进度
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     evernote
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      cnfonts
                                      calfw
                                      calfw-org
                                      calfw-cal
                                      calfw-ical
                                      helm-bibtex
                                      org-ref
                                      citeproc-org
                                      dired-k
                                      git-gutter
                                      conda
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         monokai
		                     spacemacs-dark
		                     ;; spacemacs-light
		                     ;; solarized-light
		                     solarized-dark
		                     ;; leuven
		                     ;; monokai
		                     ;; zenburn
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (cnfonts-enable)
(setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  ;; define the refile targets
  (defvar org-agenda-dir "" "gtd org files location")
  (setq-default org-agenda-dir "~/org/agenda")
  (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
  (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-files (list org-agenda-dir))

  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
      "." 'spacemacs/org-agenda-transient-state/body)
    )
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "UNCLEAR(u)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" ))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("UNCLEAR" :foreground "light blue" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  ;; the %i would copy the selected text into the template
  ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
  ;;add multi-file journal
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Inbox")
           "* TODO [#B] %?\n %i\n"
           :empty-lines 1)
          ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
           "* %?\n %i\n %U"
           :empty-lines 1)
          ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
           "* TODO [#B] %?\n %i\n %U"
           :empty-lines 1)
          ("s" "Code Snippet" entry
           (file org-agenda-file-code-snippet)
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ("w" "work" entry (file+headline org-agenda-file-gtd "Inbox")
           "* TODO [#A] %?\n %i\n %U"
           :empty-lines 1)
          ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
           "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
           :empty-lines 1)
          ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
           "* TODO [#C] %?\n %i\n %a \n %U"
           :empty-lines 1)
          ("j" "Journal Entry"
           entry (file+datetree org-agenda-file-journal)
           "* %?"
           :empty-lines 1)))

  ;;An entry without a cookie is treated just like priority ' B '.
  ;;So when create new task, they are default 重要且紧急
  (setq org-agenda-custom-commands
        '(
          ("w" . "任务安排")
          ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
          ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
          ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
          ("b" "Blog" tags-todo "BLOG")
          ("p" . "项目安排")
          ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
          ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
          ("W" "Weekly Review"
           ((stuck "") ;; review stuck projects as designated by org-stuck-projects (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
            ))))
  (defun my:org-agenda-time-grid-spacing ()
    "Set different line spacing w.r.t. time duration."
    (save-excursion
      (let* ((background (alist-get 'background-mode (frame-parameters)))
             (background-dark-p (string= background "dark"))
             (colors (if background-dark-p
                         (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue")
                       (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7")))
             pos
             duration)
        (nconc colors colors)
        (goto-char (point-min))
        (while (setq pos (next-single-property-change (point) 'duration))
          (goto-char pos)
          (when (and (not (equal pos (point-at-eol)))
                     (setq duration (org-get-at-bol 'duration)))
            (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                  (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
              (overlay-put ov 'face `(:background ,(car colors)
                                                  :foreground
                                                  ,(if background-dark-p "black" "white")))
              (setq colors (cdr colors))
              (overlay-put ov 'line-height line-height)
              (overlay-put ov 'line-spacing (1- line-height))))))))
  (add-hook 'org-agenda-finalize-hook #'my:org-agenda-time-grid-spacing)
  ;; org refile
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path t)

; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
; (setq org-completion-use-ido t)
; (setq ido-everywhere t)
; (setq ido-max-directory-size 100000)
; (ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
; (setq ido-default-file-method 'selected-window)
; (setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
  ;;(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'bh/verify-refile-target)
  (add-hook 'org-mode-hook
            (lambda()
              (setq truncate-lines nil)))

  ;; brand new calendar
  (require 'calfw-cal)
  (require 'calfw-ical)
  ;; (require 'calfw-howm)
  (require 'calfw-org)

  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")  ; orgmode source
      ;; (cfw:howm-create-source "Blue")  ; howm source
      ;; (cfw:cal-create-source "Orange") ; diary source
      ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
      ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
      )))
  ;;
  (global-set-key(kbd "C-c o") 'my-open-calendar)

  (display-time-mode 1);; 显示时间
  (setq display-time-24hr-format t) ;;格式
  (setq display-time-day-and-date t) ;;显示时间、星期、日期

  ;;org-mode export to latex
  (require 'ox-latex)
  (setq org-export-latex-listings t)

  ;;org-mode source code setup in exporting to latex
  (add-to-list 'org-latex-listings '("" "listings"))
  (add-to-list 'org-latex-listings '("" "color"))

  (add-to-list 'org-latex-packages-alist
               '("" "xcolor" t))
  (add-to-list 'org-latex-packages-alist
               '("" "listings" t))
  (add-to-list 'org-latex-packages-alist
               '("" "fontspec" t))
  (add-to-list 'org-latex-packages-alist
               '("" "indentfirst" t))
  (add-to-list 'org-latex-packages-alist
               '("" "xunicode" t))
  (add-to-list 'org-latex-packages-alist
               '("" "geometry"))
  (add-to-list 'org-latex-packages-alist
               '("" "float"))
  (add-to-list 'org-latex-packages-alist
               '("" "longtable"))
  (add-to-list 'org-latex-packages-alist
               '("" "tikz"))
  (add-to-list 'org-latex-packages-alist
               '("" "fancyhdr"))
  (add-to-list 'org-latex-packages-alist
               '("" "textcomp"))
  (add-to-list 'org-latex-packages-alist
               '("" "amsmath"))
  (add-to-list 'org-latex-packages-alist
               '("" "tabularx" t))
  (add-to-list 'org-latex-packages-alist
               '("" "booktabs" t))
  (add-to-list 'org-latex-packages-alist
               '("" "grffile" t))
  (add-to-list 'org-latex-packages-alist
               '("" "wrapfig" t))
  (add-to-list 'org-latex-packages-alist
               '("normalem" "ulem" t))
  (add-to-list 'org-latex-packages-alist
               '("" "amssymb" t))
  (add-to-list 'org-latex-packages-alist
               '("" "capt-of" t))
  (add-to-list 'org-latex-packages-alist
               '("figuresright" "rotating" t))
  (add-to-list 'org-latex-packages-alist
               '("Lenny" "fncychap" t))

  (add-to-list 'org-latex-classes
               '("lengyue-org-book"
                 "\\documentclass{book}
\\usepackage[UTF8]{ctex}
\\usepackage[utf8]{inputenc}
% chapter set
\\usepackage{titlesec}
\\usepackage{hyperref}

[NO-DEFAULT-PACKAGES]
[PACKAGES]

\\setmainfont{DejaVu Sans} % 英文衬线字体
\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{DejaVu Sans Mono}

%如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
\\defaultfontfeatures{Mapping=tex-text}

% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

% 代码设置
\\lstset{numbers=left,
numberstyle= \\tiny,
keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
frame=shadowbox,
breaklines=true,
rulesepcolor= \\color{ red!20!green!20!blue!20}
}

[EXTRA]
"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("lengyue-org-article"
                 "\\documentclass{article}
\\usepackage[UTF8]{ctex}
\\usepackage[utf8]{inputenc}
\\usepackage{titlesec}
\\usepackage{hyperref}

[NO-DEFAULT-PACKAGES]
[PACKAGES]

\\parindent 2em

\\setmainfont{DejaVu Sans} % 英文衬线字体
\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{DejaVu Sans Mono}

%如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
\\defaultfontfeatures{Mapping=tex-text}

% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

% 代码设置
\\lstset{numbers=left,
numberstyle= \\tiny,
keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
frame=shadowbox,
breaklines=true,
rulesepcolor= \\color{ red!20!green!20!blue!20}
}

[EXTRA]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("lengyue-org-beamer"
                 "\\documentclass{beamer}
\\usepackage[UTF8]{ctex}
\\usepackage[utf8]{inputenc}
% beamer set
\\usepackage[none]{hyphenat}
\\usepackage[abs]{overpic}

[NO-DEFAULT-PACKAGES]
[PACKAGES]

\\setmainfont{DejaVu Sans} % 英文衬线字体
\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{DejaVu Sans Mono}

%如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
\\defaultfontfeatures{Mapping=tex-text}

% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt

% 代码设置
\\lstset{numbers=left,
numberstyle= \\tiny,
keywordstyle= \\color{ blue!70},commentstyle=\\color{red!50!green!50!blue!50},
frame=shadowbox,
breaklines=true,
rulesepcolor= \\color{ red!20!green!20!blue!20}
}

[EXTRA]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          ;;"biber %b" "xelatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"))
  ;; init helm-bibtex
(use-package helm-bibtex :ensure t
  :bind ("<f11>" . helm-bibtex)
  :commands (helm-bibtex)
  :init
  (add-hook 'bibtex-completion-edit-notes 'org-ref-open-bibtex-notes)
  (setq bibtex-completion-open-any 'org-ref-open-bibtex-pdf)
  :config
  (setq bibtex-completion-bibliography "~/literature/bibliography.bib"
      bibtex-completion-library-path "~/literature/pdfs"
      bibtex-completion-notes-path "~/literature/refnotes.org")
  ;(setq bibtex-completion-display-formats
  ;  '((t . "${=type=:7} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${author:30} ${title:72} ")))
  (setq bibtex-completion-additional-search-fields '(keywords))
  (setq bibtex-completion-notes-template-one-file
	(format "\n** TODO ${=key=} - ${title}\n  :PROPERTIES:\n    :Author: ${author-or-editor}\n    :Journal: ${journal}\n  :END:\n\n"))
  (setq bibtex-completion-display-formats
	'((t . "${author:20} ${year:4} ${=has-pdf=:3} ${=has-note=:1} ${=type=:7} ${title:90}")))
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-pdf-symbol "PDF")
  (setq bibtex-completion-notes-symbol "N")
 )

(use-package org-ref :ensure t
  ;;:defer 1
  :after (org)
  :config
  ;;(setq reftex-default-bibliography '("~/OneDrive/2020.03.28_PunchingShearReferences/Literature.bib"))
  ;; see org-ref for use of these variables
  (setq bibtex-completion-pdf-field "file")
  (setq org-ref-bibliography-notes "~/literature/refnotes.org"
        org-ref-default-bibliography '("~/literature/bibliography.bib")
	      org-ref-pdf-directory "~/literature/pdfs")
  ;;(setq bibtex-completion-bibliography "~/OneDrive/2020.03.28_PunchingShearReferences/Literature.bib"
  ;;    bibtex-completion-library-path "~/OneDrive/2020.03.28_PunchingShearReferences/PDFs"
  ;;    bibtex-completion-notes-path "~/OneDrive/2020.03.28_PunchingShearReferences/Literature-manuscript.org")
  (setq org-ref-show-broken-links nil)
  (setq bibtex-completion-pdf-open-function 'org-open-file)
  (setq org-ref-note-title-format
   "** TODO %k - %t
 :PROPERTIES:
  :CUSTOM_ID: %k
  :AUTHOR: %9a
  :JOURNAL: %j
  :DOI: %D
  :URL: %U
 :END:
")

  (setq bibtex-completion-display-formats
	'((t . "${author:20} ${year:4} ${=has-pdf=:3} ${=has-note=:1} ${=type=:7} ${title:90}")))
  (defun my/org-ref-notes-function (candidates)
    (let ((key (helm-marked-candidates)))
      (funcall org-ref-notes-function (car key))))

  (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
;; Note that 7 is a magic number of the index where you want to insert the command. You may need to change yours.
  (helm-add-action-to-source "Edit notes" 'my/org-ref-notes-function helm-source-bibtex 7)
)
;; citeproc-org
;; 使得 org mode 可以使用引用
(citeproc-org-setup)
;; git-gutter
;; 使得 emacs 可以在侧边栏显示 git 的状态

;; conda
;; 使得 emacs 可以利用 conda 的环境
;;Anaconda support
(require 'conda)

(setq conda-env-home-directory conda-anaconda-home)
;;get current environment--from environment variable CONDA_DEFAULT_ENV
(conda-env-activate "base")
;;(conda-env-autoactivate-mode t)
;; add conda-environment into mode-line
(setq-default mode-line-format (cons mode-line-format '(:exec conda-env-current-name)))
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(wgrep smex ivy-hydra counsel-projectile counsel swiper ivy cnfonts org-projectile org-pomodoro alert log4e xterm-color unfill smeargle shell-pop orgit org-category-capture org-present gntp org-mime org-download mwim multi-term mmm-mode markdown-toc markdown-mode magit-gitflow magit-popup htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger g  it-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md fuzzy flycheck-pos-tip pos-tip flycheck evil-magit magit git-commit with-editor transient eshell-z eshell-prompt-extras esh-help diff-hl company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete spaceline paradox hydra highlight-numbers helm-projectile projectile flx-ido evil-unimpaired f evil-search-highlight-persist evil-lisp-state ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree toc-org powerline restart-emacs request rainbow-delimiters pkg-info popwin persp-mode pcre2el spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide lv hungry-delete hl-todo highlight-parentheses parent-mode highlight-indentation helm-themes helm-swoop epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg eval-sexp-fu elisp-slime-nav dumb-jump dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   '(conda yapfify stickyfunc-enhance sphinx-doc pytest pyenv-mode py-isort poetry pippel pipenv pip-requirements lsp-python-ms lsp-pyright live-py-mode importmagic epc ctable concurrent deferred helm-pydoc helm-gtags helm-cscope xcscope ggtags dap-mode lsp-treemacs bui lsp-mode cython-mode counsel-gtags company-anaconda blacken anaconda-mode pythonic wgrep smex ivy-hydra counsel-projectile counsel swiper ivy cnfonts org-projectile org-pomodoro alert log4e xterm-color unfill smeargle shell-pop orgit org-category-capture org-present gntp org-mime org-download mwim multi-term mmm-mode markdown-toc markdown-mode magit-gitflow magit-popup htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger g it-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md fuzzy flycheck-pos-tip pos-tip flycheck evil-magit magit git-commit with-editor transient eshell-z eshell-prompt-extras esh-help diff-hl company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete spaceline paradox hydra highlight-numbers helm-projectile projectile flx-ido evil-unimpaired f evil-search-highlight-persist evil-lisp-state ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree toc-org powerline restart-emacs request rainbow-delimiters pkg-info popwin persp-mode pcre2el spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide lv hungry-delete hl-todo highlight-parentheses parent-mode highlight-indentation helm-themes helm-swoop epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg eval-sexp-fu elisp-slime-nav dumb-jump dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
