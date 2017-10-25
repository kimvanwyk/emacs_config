;; Ensure base lisp dir and addons dir are on load path
(add-to-list 'load-path "~/emacs/lisp")
(add-to-list 'load-path "~/emacs/lisp/addons")

;; Bring in require-if-exist, used extensively below
(require 'de_require-if-exists)

;;Ensure dot_emacs is open
(find-file-noselect "~/emacs/lisp/addons/dot_emacs.el")

;; Customise and private data
(require 'de_cust_priv)

;; Windows only - detect with window-system variable
(require 'de_windows)
;; X only
(require 'de_x)

;; Autosave and backup
(require 'de_backup)
;; Ack, grep, isearch etc
(require 'de_searching)
;; Programming specific
(require 'de_programming)
;; Dired changes
(require 'de_dired)
;; General Emacs IDE changes, applicable for all modes and usages
(require 'de_emacs_env)
;; LaTeX specific
(require 'de_latex)
;; iBuffer and buffer switching
(require 'de_buffer)
;; Network - jabber, twitter etc
(require 'de_network)
;; VCS related changes
;; (require 'de_vcs)
;; Org mode
(require 'de_org)
;; Disable music stuff as it takes a while to load and is currently unused.
;; (require 'de_music)
;; yasnippet, put in last to try and solve some timing issues
;; (require 'de_yasnippet)
;; (set-font)
