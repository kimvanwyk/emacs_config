;;
;;(@* "Minibuffer Bindings")
;;  ** Minibuffer Bindings **
;;
;;  There are many key bindings available during completion.  Most of
;;  these key sequences are bound in the minibuffer completion
;;  keymaps, but some are bound in the *Completions* buffer keymap.
;;  In addition, clicking `C-mouse-3' on a completion candidate in
;;  buffer *Completions* pops up a menu of available commands.
;;
;;  Some of these menu commands are applicable to the completion you
;;  click; others apply to the current state of completion or to the
;;  complete set of completion candidates.  The associated key
;;  bindings are indicated in the menu items, so this can be a good
;;  way to learn minibuffer and *Completions* bindings.
;;
;;  The following key bindings are made for the minibuffer completion
;;  keymaps.  They are in effect whenever you are using the minibuffer
;;  for input with completion (e.g. `completing-read',
;;  `read-file-name', `M-x').
;;
;;    `C-?' is bound to `icicle-completion-help': Pop up a *Help*
;;    buffer with information on using Icicles completion.  This
;;    includes information on key bindings during completion similar
;;    to what you are reading now.
;;
;;    Keys bound globally to `next-line' and `previous-line' are bound
;;    to `icicle-next-prefix-candidate' and
;;    `icicle-previous-prefix-candidate'.  Those are the commands
;;    that cycle candidate prefix completions.  By default, this means
;;    keys `down', `up', `C-n', and `C-p'.
;;
;;    Keys bound globally to `scroll-up' and `scroll-down' are bound
;;    to `icicle-next-apropos-candidate' and
;;    `icicle-previous-apropos-candidate'.  Those are the commands
;;    that cycle candidate apropos completions.  By default, this
;;    means keys `next', `prior', `C-v', and `M-v'.
;;
;;    Keys bound globally to commands that perform simple text
;;    insertion, deletion, and transposition operations - commands
;;    such as `self-insert-command' - are bound to Icicles versions of
;;    those commands that do the same thing but also provide apropos
;;    icompletion.  This includes keys such as `C-d', `M-d', `C-y',
;;    `C-k', and `C-w' (and lots more).  See (@> "Icompletion").
;;
;;    `pause'  - `icicle-switch-to/from-minibuffer': Move cursor to
;;               the buffer from which the minibuffer was activated.
;;
;;    `C-insert' - `icicle-switch-to-Completions-buf': Move cursor to
;;               the current candidate in buffer *Completions*.
;;
;;    `M-*'    - `icicle-narrow-candidates': Narrow the set of
;;               completion candidates using another input regexp.
;;
;;    `SPC'    - `icicle-self-insert' (see above): Insert a space.
;;
;;    `M-SPC'  - `icicle-prefix-word-complete': Complete current input
;;               in minibuffer, as a prefix, a single word at a time.
;;               This replaces `minibuffer-complete-word'.  In fact,
;;               it is the value of `icicle-word-completion-key' that
;;               is bound to this command; `M-SPC' is the default
;;               value of this user option.
;;
;;    `S-SPC'  - `icicle-apropos-complete-and-narrow': Same as
;;               `S-TAB' followed by `M-*'.  However, this binds
;;               `icicle-top-level-when-sole-completion-flag' to t,
;;               so if there is only one matching candidate, then that
;;               candidate is used.
;;
;;    `TAB'    - `icicle-prefix-complete': Complete current input in
;;               minibuffer, as a prefix.  If there is more than one
;;               prefix-completion candidate, display them in buffer
;;               *Completions*, highlighting the common prefix.  This
;;               replaces `minibuffer-complete'.
;;
;;    `S-TAB'  - `icicle-apropos-complete': Like `TAB', but use
;;               apropos completion.  During standard incremental
;;               search (`isearch'), `S-TAB' is bound in the
;;               minibuffer (actually, the echo area) to
;;               `icicle-isearch-complete'.
;;
;;    `C-M-TAB' - `icicle-prefix-complete-no-display': Like `TAB', but
;;               does not display candidates in *Completions*.
;;
;;    `C-M-S-TAB' - `icicle-apropos-complete-no-display': Like
;;               `S-TAB', but does not display candidates.
;;
;;    `C-M-&'  - `icicle-save-predicate-to-variable': Save the current
;;               predicate used for completion to a variable.
;;
;;    `delete' - `icicle-remove-candidate': Remove the current
;;               candidate from consideration.
;;
;;    `S-mouse-2' - `icicle-mouse-remove-candidate': Same as `delete'.
;;
;;    `M-q'    - `icicle-insert-key-description': Insert the textual
;;               representation of a key sequence, during key
;;               completion.
;;
;;  If you prefer, you can use the keys that are the values of options
;;  `icicle-modal-cycle-up-key' and `icicle-modal-cycle-down-key'
;;  (`up' and `down', by default) for both prefix and apropos
;;  completion, as well as for input-history traversal - the behavior
;;  is determined by whether you have previously used `TAB' or
;;  `S-TAB'.  To obtain this modal behavior, set user option
;;  `icicle-cycling-respects-completion-mode-flag' to non-nil.  The
;;  documentation here assumes the default value of nil.  See
;;  (@> "Customization and General Tips").
;;
;;  The following minibuffer bindings are made to clear minibuffer
;;  input, making them handy for editing and removing completions
;;  (e.g. default or initial values) in the minibuffer.
;;
;;    `M-k' - `icicle-erase-minibuffer-or-history-element'
;;    `M-S-backspace', `M-S-delete' - `icicle-erase-minibuffer'
;;
;;  `M-k' has an alternative behavior when you are cycling minibuffer
;;  history items: it deletes the current item from the history.
;;
;;  The following minibuffer binding kills (deletes) the part of your
;;  input that does not complete, that is, the part that does not
;;  match any completion candidate.  The input remaining after this
;;  operation matches at least one candidate.
;;
;;    `C-M-l'         - `icicle-kill-failed-input'
;;
;;  The following minibuffer bindings can be used to get rid of a
;;  completion inserted during cycling, and retrieve what you last
;;  typed during completion or any previous completion inputs:
;;
;;    `C-l'           - `icicle-retrieve-previous-input'
;;    `C-S-l' (`C-L') - `icicle-retrieve-next-input'
;;
;;  You can use these to cycle among and reuse inputs that you typed
;;  during completion but did not enter.  This completion input is not
;;  recorded in the standard input histories - they record only input
;;  that you have entered with `RET'.
;;  See (@> "History Enhancements").
;;
;;  For example, suppose that you used `C-h v hook' to examine various
;;  hook variables, and you did this using`C-next' to display their
;;  documentation.  If you finished the command by just typing `C-g',
;;  then your input (`hook') was never really entered, so it is not
;;  available via the minibuffer history (`M-p').  You can retrieve it
;;  with `C-l', to use it again, in your next command.  User option
;;  `icicle-C-l-uses-completion-flag' controls the behavior of `C-l'
;;  and `C-L'; if non-nil, then, instead of cycling inputs, these
;;  commands let you access previous inputs using completion.
;;
;;  You of course have the standard access to the minibuffer history,
;;  via `M-p', `M-n', `M-r', and `M-s'.  In addition to these, the
;;  following minibuffer bindings let you use apropos completion on
;;  the current minibuffer history list.  For explanation, see
;;  (@> "History Enhancements").
;;
;;    `M-h'     - `icicle-history'
;;    `M-pause' - `icicle-keep-only-past-inputs'
;;
;;  The following minibuffer bindings let you act on candidate
;;  completions.  For explanation, see (@> "Multi-Commands"),
;;  (@> "Choose All Completion Candidates"), and
;;  (@> "OO: Object-Action Interaction").
;;
;;    `C-RET'     - `icicle-candidate-action': current candidate
;;    `C-mouse-2' - `icicle-mouse-candidate-action': clicked candidate
;;    `C-up'      - `icicle-previous-prefix-candidate-action'
;;    `C-down'    - `icicle-next-prefix-candidate-action'
;;    `C-prior'   - `icicle-previous-apropos-candidate-action'
;;    `C-next'    - `icicle-next-apropos-candidate-action'
;;    `C-!'       - `icicle-all-candidates-action': all candidates
;;    `M-RET'     - `icicle-candidate-read-fn-invoke': apply function
;;    `M-mouse-2' - `icicle-mouse-candidate-read-fn-invoke': apply fn
;;
;;  (The bindings for `icicle-mouse-*' are actually in the
;;  *Completions* buffer.)
;;
;;  The following minibuffer bindings provide help on candidate
;;  completions.  For explanation, see (@> "Get Help on Candidates")
;;  and (@> "Multi-Commands").
;;
;;    `C-M-RET'   - `icicle-help-on-candidate': current candidate
;;    `C-M-mouse-2' - `icicle-mouse-help-on-candidate': clicked
;;    `C-M-up'    - `icicle-help-on-previous-prefix-candidate'
;;    `C-M-down'  - `icicle-help-on-next-prefix-candidate'
;;    `C-M-prior' - `icicle-help-on-previous-apropos-candidate'
;;    `C-M-next'  - `icicle-help-on-next-apropos-candidate'
;;
;;  The following minibuffer bindings provide an alternative action
;;  for individual candidates.  An alternative action is specific to
;;  the given command.  Most commands define no alternative action.
;;
;;    `C-S-RET'     - `icicle-candidate-alt-action': current candidate
;;    `C-S-mouse-2' - `icicle-mouse-candidate-alt-action': clicked
;;    `C-S-up'      - `icicle-previous-prefix-candidate-alt-action'
;;    `C-S-down'    - `icicle-next-prefix-candidate-alt-action'
;;    `C-S-prior'   - `icicle-previous-apropos-candidate-alt-action'
;;    `C-S-next'    - `icicle-next-apropos-candidate-alt-action'
;;
;;  The following minibuffer bindings let you perform set operations
;;  on sets of completion candidates.  For explanation, see
;;  (@> "Sets of Completion Candidates").
;;
;;    `C-~'     - `icicle-candidate-set-complement'
;;    `C--'     - `icicle-candidate-set-difference'
;;    `C-+'     - `icicle-candidate-set-union'
;;    `C-*'     - `icicle-candidate-set-intersection'
;;    `C-M->'   - `icicle-candidate-set-save': save current set
;;    `C-M-<'   - `icicle-candidate-set-retrieve': retrieve saved set
;;    `C->'     - `icicle-candidate-set-save-more': add to current set
;;    `insert'  - `icicle-save-candidate': save current candidate
;;    `M-S-mouse-2' - `icicle-mouse-save-candidate': click to save
;;    `C-%'     - `icicle-candidate-set-swap': swap saved and current
;;    `C-:'     - `icicle-candidate-set-define': define current (Lisp)
;;
;;  The following minibuffer bindings insert text in the minibuffer.
;;
;;    `M-.'     - `icicle-insert-string-at-point'
;;    `C-='     - `icicle-insert-string-from-variable'
;;
;;  The following minibuffer bindings let you toggle Icicles options.
;;
;;    `C-S-a' (that is, `C-A') - `icicle-toggle-case-sensitivity'
;;    `C-.'     - `icicle-toggle-ignored-extensions' (file completion)
;;    `C-.'     - `icicle-toggle-search-cleanup' (search)
;;    `C-,'     - `icicle-change-sort-order'
;;    `C-,'     - `icicle-toggle-search-replace-whole' (search)
;;    `M-,'     - `icicle-change-alternative-sort-order'
;;    `C-M-,'   - `icicle-toggle-alternative-sorting'
;;    `C-^'     - `icicle-toggle-ignored-space-prefix'
;;    `C-^'     - `icicle-toggle-highlight-all-current' (search)
;;    `C-#'     - `icicle-toggle-incremental-completion'
;;    `C-`'     - `icicle-toggle-regexp-quote'
;;    `C-M-`'   - `icicle-toggle-literal-replacement' (search)
;;    `C-<'     - `icicle-toggle-angle-brackets'
;;    `C-$'     - `icicle-toggle-transforming' (removal of duplicates)
;;    `C-pause' - `icicle-toggle-highlight-historical-candidates'
;;    `M-q'     - `icicle-toggle-search-whole-word' (search)
;;    `M-~'     - `icicle-toggle-~-for-home-dir'
;;
;;  In addition, the following minibuffer bindings are in effect
;;  during Icicles search:
;;
;;    `C-.'     - `icicle-toggle-search-cleanup'
;;    `C-,'     - `icicle-toggle-search-replace-whole'
;;    `M-,'     - `icicle-search-define-replacement'
;;    `M-q'     - `icicle-toggle-search-whole-word'
;;    `C-^'     - `icicle-toggle-highlight-all-current'
;;    `C-M-`'   - `icicle-toggle-literal-replacement'
;;
;;  The following minibuffer binding lets you remove the *Completions*
;;  window.
;;
;;    `C-x 0'   - `icicle-remove-Completions-window'
;;
;;  The following minibuffer binding lets you evaluate an Emacs-Lisp
;;  expression at any time, using a recursive minibuffer.  It calls
;;  `pp-eval-expression' to display the result in the echo area or in
;;  a popup buffer, *Pp Eval Output*.
;;
;;    `M-:'     - `icicle-pp-eval-expression'
;;
;;  The following bindings are made for `completion-list-mode', that
;;  is, for buffer *Completions*, which shows the list of candidate
;;  completions:
;;
;;    `left', `right' (`TAB')
;;                    - `icicle-move-to-previous-completion',
;;                      `icicle-move-to-next-completion': Navigate
;;                      backward & forward among candidates
;;    `up', `down'    - `icicle-previous-line', `icicle-next-line':
;;                      Navigate up & down among candidates
;;    `C-insert'      - `icicle-insert-completion': Move cursor to the
;;                      minibuffer, with the current *Completions*
;;                      candidate as input
;;    `C-g', `q'      - `icicle-abort-minibuffer-input'
;;    `mouse-2'       - `icicle-mouse-choose-completion'
;;    `C-mouse-2'     - `icicle-mouse-candidate-action'
;;    `C-M-mouse-2'   - `icicle-mouse-help-on-candidate'
;;    `M-S-mouse-2'   - `icicle-mouse-save-candidate'
;;    `C-mouse-3'     - `icicle-Completions-mouse-3-menu'
;;
;;  If you are used to using `down', `up', `C-n', and `C-p' for the
;;  minibuffer history, you can restore those bindings and bind the
;;  corresponding Icicles commands to different keys.  See
;;  (@> "Customizing Key Bindings").
 
;;(@* "Customizing Key Bindings")
;;
;;  Customizing Key Bindings
;;  ------------------------
;;
;;  See (@> "Key Bindings") for a description of the key bindings
;;  defined by Icicles.
;;
;;  Key bindings are very personal choices, and reflect preferences
;;  and habits, as well as keyboard and other configurations.  You
;;  might want to change some of the bindings that Icicles creates.
;;  This section tells you how to do that.
;;
;;  However, before doing so, unless the default bindings present a
;;  hardware or OS configuration problem for you, please try using the
;;  default bindings for a while, before deciding that you want to
;;  change them.  Habit is a powerful persuader, but its advice is not
;;  always the best ;-).
;;
;;  There are two kinds of Icicles bindings:
;;
;;  * Additions to menu-bar menus
;;  * Minibuffer bindings
;;
;;(@* "Customizing Menu-Bar Menus")
;;  ** Customizing Menu-Bar Menus **
;;
;;  Icicles normally adds items to appropriate existing menu-bar
;;  menus, such as File and Options, as well as to menu-bar menus
;;  Minibuf and Icicles, but you can prevent this, if you like.  If
;;  you do not want to add items to menus besides Minibuf and Icicles,
;;  then just set option `icicle-touche-pas-aux-menus-flag' to non-nil
;;  before loading Icicles.  The items in question are then added to
;;  the Icicles menu instead.
;;
;;(@* "Customizing Minibuffer Bindings")
;;  ** Customizing Minibuffer Bindings **
;;
;;  Note: If you are thinking about customizing key bindings just so
;;  that you can use `up' and `down' for the minibuffer history,
;;  consider setting `icicle-cycling-respects-completion-mode-flag' to
;;  t instead; it should give you the behavior you want.  If you still
;;  want to customize keys to do this, then see the example below.
;;
;;  To understand how you can modify Icicles minibuffer bindings, it
;;  helps to know how Icicles creates the default bindings.  For that,
;;  the best advice is to consult the Emacs-Lisp code in library
;;  `icicle-fn.el'.
;;
;;  Even if you are not very familiar with Emacs-Lisp, however, you
;;  should be able to do what you want by adapting the example in this
;;  section.
;;
;;  Suppose that you use `up', `down', `C-p', and `C-n' outside of
;;  Emacs to traverse history lists.  This is a common feature of
;;  various shells, for instance.  And suppose you want to use these
;;  keys similarly within Emacs.  Suppose that you decide to replace
;;  the Icicles minibuffer bindings for these keys with bindings to
;;  the history-traversal commands, and replace the history-traversal
;;  bindings of Icicles, `M-p' and `M-n', with bindings to the Icicles
;;  commands that are bound by default to `up', `down', `C-p', and
;;  `C-n'.  That is, suppose that you want to remap:
;;
;;    `previous-history-element'         to `up' and `C-p'
;;    `next-history-element'             to `down' and `C-n'
;;    `icicle-previous-prefix-candidate' to `M-p'
;;    `icicle-next-prefix-candidate'     to `M-n'
;;
;;  You can do that by inserting code such as the following into your
;;  init file (~/.emacs), before the code that requires (loads)
;;  library `icicles.el':
;;
;;  (add-hook 'icicle-mode-hook 'bind-my-icicles-keys)
;;  (defun bind-my-icicles-keys ()
;;    "Replace some default Icicles bindings with others I prefer."
;;    (dolist
;;        (map
;;          (append
;;           (list minibuffer-local-completion-map
;;                 minibuffer-local-must-match-map)
;;           (and (fboundp
;;                 'minibuffer-local-filename-completion-map)
;;                (list minibuffer-local-filename-completion-map))))
;;      (when icicle-mode
;;        (icicle-remap 'previous-line 'previous-history-element map)
;;        (icicle-remap 'next-line 'next-history-element map)
;;        (define-key map [?\M-p] 'icicle-previous-prefix-candidate)
;;        (define-key map [?\M-n] 'icicle-next-prefix-candidate))))
;;
;;  In the `global-map', command `next-line' is bound to `down' and
;;  `C-n'.  Icicles uses function `icicle-remap' to remap `next-line'
;;  to `icicle-next-prefix-candidate'.  Your `bind-my-icicles-keys'
;;  code then remaps it to `next-history-element'.  Likewise for
;;  `previous-line'.  Icicles does not change the standard Emacs
;;  minibuffer bindings for `M-p' and `M-n', so they are still
;;  `previous-history-element' and `next-history-element'.  You can
;;  just use `define-key' to change the bindings of `M-p' and `M-n'.
;;
;;  See also (@> "Customization and General Tips") for information
;;  about other customizations, besides key bindings.
 
;;(@* "Icicles Redefines Some Standard Commands")
;;
;;  Icicles Redefines Some Standard Commands
;;  ----------------------------------------
;;
;;  If user option `icicle-redefine-standard-commands-flag' is
;;  non-nil, then Icicles automatically redefines a few standard Emacs
;;  commands when you are in Icicle mode, enhancing them for Icicles
;;  completion:
;;
;;    `customize-apropos', `customize-apropos-faces',
;;    `customize-apropos-groups', `customize-apropos-options',
;;    `dabbrev-completion', `digit-argument' (in minibuffer),
;;    `exit-minibuffer', `lisp-complete-symbol',
;;    `minibuffer-complete-and-exit', `mouse-choose-completion',
;;    `negative-argument' (in minibuffer), `repeat-complex-command',
;;    `switch-to-completions', `universal-argument' (in minibuffer),
;;    `yank' (in minibuffer), `yank-pop' (in minibuffer).
;;
;;  When you exit Icicle mode, the pre-Icicles definitions are
;;  restored.
 
;;(@* "Programming Multi-Completions")
;;
;;  Programming Multi-Completions
;;  -----------------------------
;;
;;  This section is for Emacs-Lisp programmers.
;;
;;  Multi-completions are completion candidates that are composed of
;;  parts separated by `icicle-list-join-string' and terminated by
;;  `icicle-list-end-string'.  See (@> "Multi-Completions") for
;;  information about how users interact with multi-completions.
;;
;;  You can define your own Icicles commands that use
;;  multi-completions.  You can bind `icicle-list-join-string' or
;;  `icicle-list-end-string' to any strings you like, depending on
;;  your needs.  See (@> "Key Completion") for an example where
;;  `icicle-list-join-string' is bound to "  =  " and
;;  `icicle-list-end-string' is "".  This section describes two
;;  additional variables that you can bind to affect the appearance
;;  and behavior of multi-completions.
;;
;;(@* "Variable icicle-list-use-nth-parts")
;;  ** Variable icicle-list-use-nth-parts **
;;
;;  Variable `icicle-list-use-nth-parts' affects the minibuffer
;;  behavior of multi-completions.  If you bind this to a list of
;;  whole numbers, then multi-completion candidates are transformed
;;  using those numbers as indexes.  During completion and cycling,
;;  whenever a sole candidate matches the user input, if that
;;  candidate is a multi-completion, then it is transformed by
;;  extracting and possibly reordering its parts according to
;;  `icicle-list-use-nth-parts'.
;;
;;  The actual candidate to match is still the original candidate; the
;;  transformation takes place after matching, for final insertion in
;;  the minibuffer.  This means that you must use this feature only
;;  with lax (permissive) completion, since strict completion requires
;;  an exact match against the original completion candidate, and the
;;  transformed candidate will normally not match the original.
;;
;;  Variable `icicle-list-use-nth-parts' works as follows.  The
;;  matching candidate is split at each `icicle-list-join-string' into
;;  its component parts.  The indexes in `icicle-list-use-nth-parts'
;;  are then used to extract parts, in the same order as the indexes
;;  appear.  The extracted parts are joined back together in an order
;;  that you specify, separated by the value of user option
;;  `icicle-list-nth-parts-join-string'.  An index greater than the
;;  number of parts means to use the last part.
;;
;;  For example: If the value of `icicle-list-use-nth-parts' is (1),
;;  then only the first part of the multi-completion is used as the
;;  completion candidate.  If the value is (2 1), then the resulting
;;  candidate is the second part followed by the first part, the two
;;  parts being joined by `icicle-list-nth-parts-join-string'.  If the
;;  value is (1 99) and the multi-completion has fewer than 99 parts,
;;  then the first and last parts are used.  If the value is (2 1 2),
;;  then the resulting candidate is composed of the second part
;;  followed by the first part followed by the second part again.
;;
;;  Thus, you can use a given part any number of times.  You can also
;;  mix multi-completions and single-string completions, and you can
;;  mix multi-completions composed of different numbers of strings.
;;  For example, a set of completions might be:
;;
;;  ((("cmd1" "description of cmd1"))
;;   (("cmd2" "description of cmd" "more"))
;;   (("cmd3")))
;;
;;  If you use multi-completions with `icicle-list-use-nth-parts' in
;;  your own commands, please make sure that their doc strings let
;;  users know what to expect, and remind them of the behavior of
;;  option `icicle-list-nth-parts-join-string'.  Let them know, in
;;  particular, that:
;;
;;  * They can match any part of a candidate as it is displayed in
;;    `*Completions*'.
;;
;;  * The candidate choice they make will in fact have the form that
;;    you define in your command.
;;
;;  * They can control how the parts are joined, using option
;;    `icicle-list-nth-parts-join-string'.
;;
;;(@* "Variable icicle-candidate-properties-alist")
;;  ** Variable icicle-candidate-properties-alist **
;;
;;  Whereas variable `icicle-list-nth-parts-join-string' affects the
;;  appearance of multi-completions in the minibuffer, variable
;;  `icicle-candidate-properties-alist' affects their appearance in
;;  buffer *Completions*.  You use it to apply text properties to
;;  individual parts of a multi-completion, where the parts are
;;  defined in the same way as for `icicle-list-use-nth-parts'.
;;
;;  This feature affects all candidates the same way.  See also
;;  (@> "Text Properties in *Completions") for how to apply text
;;  properties to individual candidates (which need not be
;;  multi-completions).
;;
;;  The value of `icicle-candidate-properties-alist' is an alist whose
;;  entries have either of these forms:
;;
;;  (NTH PROPERTIES) or (NTH PROPERTIES JOIN-TOO)
;;
;;  NTH is the number of the target multi-completion part.
;;
;;  PROPERTIES is a list of text properties to apply to the NTH part.
;;
;;  JOIN-TOO is optional.  If it is present and non-nil, then the text
;;  properties are also applied to the join string that follows the
;;  target part.
;;
;;  You can use any text properties, including `invisible', `keymap',
;;  `display', and properties that you define yourself and that have
;;  meaning to only your code.
;;
;;  As an example of its use, commands `icicle-fundoc',
;;  `icicle-vardoc', `icicle-doc', and `icicle-plist' bind
;;  `icicle-candidate-properties-alist' to
;;  ((1 (face 'icicle-candidate-part))), so that the first part of
;;  each multi-completion candidate is highlighted using face
;;  `icicle-candidate-part'.
;;
;;  Here is another example value of
;;  `icicle-candidate-properties-alist':
;;
;;  ((3 (face 'underline))
;;   (2 (invisible t) t))
;;
;;  The first entry underlines the third multi-completion part.  The
;;  second entry makes both the second part and the join string that
;;  follows it invisible.
;;
;;  One use of making a completion part invisible is so that you can
;;  sort candidates using it, and let users match input against it,
;;  but not have it appear explicitly.
;;
;;  Recall that `completing-read' displays only the car of each
;;  element present in its TABLE (alist) argument.  For example, if
;;  you pass `completing-read' an alist such as (("foo" . 2) ("bar"
;;  . 3)), then only `foo' and `bar' are displayed as candidates.
;;  However, the PREDICATE argument to `completing-read' applies to
;;  the entire alist element, and your command that calls
;;  `completing-read' might well use the chosen candidate (e.g. `foo')
;;  to look up the entire element (e.g. ("foo" . 2)) for further
;;  processing.  Several Icicles commands, including `icicle-search'
;;  and `icicle-select-region', do that.
;;
;;  However, sometimes you might want the user to be able to match
;;  against the additional information (e.g. 2 and 3), and you might
;;  want to use it to sort candidates.  In that case, you can use the
;;  alist (("foo 2") ("bar 3")).  In cases where the additional
;;  information can be distracting, you can use multi-completion with
;;  `icicle-candidate-properties-alist' to hide it: Pass the alist
;;  ((("foo "2")) (("bar" 3"))) and use ((2 (invisible t))) for
;;  `icicle-candidate-properties-alist'.
;;
;;  Keep in mind that hiding completion parts can be confusing to
;;  users.  Do so with care, and let your users know what to expect.
;;  Inform them that there are invisible parts that are nevertheless
;;  taken into account for input matching and candidate sorting.  When
;;  you hide parts, you will often want to omit them from the
;;  minibuffer as well, using `icicle-list-use-nth-parts', to avoid
;;  confusion.
;;
;;  Consider also the position of a hidden part: In some cases you
;;  might want to place it first among the multi-completion parts, but
;;  in many cases you will want to place it last, to minimize
;;  interference with prefix-completion matching.
;;
;;  Similar considerations apply to other text properties, such as
;;  `display' and `keymap', that change the appearance or behavior of
;;  a completion candidate.
;;
;;(@* "What You See Is Not What You Get")
;;  ** What You See Is Not What You Get **
;;
;;  While on the subject of confusing users, let me point out a
;;  general drawback that is common to both
;;  `icicle-list-use-nth-parts' and
;;  `icicle-candidate-properties-alist': *not* WYSIWYG.  Keep this in
;;  mind if you decide to take advantage of these variables.  Users
;;  see one thing, choose it, and they get something different as a
;;  result.  That promotes confusion that you will need to weigh
;;  against the possible benefits.
;;
;;  Users are confused, because what they choose is not exactly what
;;  they get.  What's more, a user's completion choice is not
;;  reflected in the input history, leading to further confusion.  For
;;  example, Icicles highlighting of previously used inputs in buffer
;;  *Completions* does not apply to such a candidate, even though it
;;  was previously entered using `RET'.  It is the transformed
;;  candidate that was entered, not the candidate as it was proposed
;;  for choosing, so when that candidate is proposed again, it is not
;;  recognized as having been previously chosen.
;;
;;  The bottom line here is this: variables
;;  `icicle-list-use-nth-parts' and
;;  `icicle-candidate-properties-alist' are useful in certain
;;  contexts, but be aware of the downside: confusing your users.
;;
;;  See Also:
;;
;;  * (@> "Multi-Completions")
;;  * (@> "Text Properties in *Completions")
 
;;(@* "Text Properties in *Completions*")
;;
;;  Text Properties in *Completions*
;;  --------------------------------
;;
;;  This section is for Emacs-Lisp programmers.
;;
;;  Section (@> "Programming Multi-Completions") explains how to apply
;;  text properties to all multi-completion candidates in buffer
;;  *Completions* at the same time.  This section tells you how to
;;  apply text properties to individual candidates in *Completions*.
;;  The candidates need not be multi-completions.
;;
;;  There are two ways to do this:
;;
;;  1. Use the text properties as the value of the candidate's
;;     `icicle-special-candidate property.
;;
;;  2. Apply the text properties to the candidate.
;;
;;(@* "Using Property icicle-special-candidate")
;;  ** Using Property icicle-special-candidate **
;;
;;  In this approach, you attach the text properties as the value of
;;  property `icicle-special-candidate' to the symbol that represents
;;  the candidate.  If the candidate is a string, not a symbol, then
;;  `intern' it and use the resulting symbol.
;;
;;  If you want the effect to be temporary, then don't forget to set
;;  property `icicle-special-candidate' for the candidate to
;;  nil when you are done.
;;
;;  As a shortcut, if you use the value t instead of a property list
;;  for property `icicle-special-candidate', then face
;;  `icicle-special-candidate' will be used as the `face' property of
;;  the candidate.  In other words, using t is equivalent to using
;;  (face icicle-special-candidate).  This approach is used, for
;;  instance, in the definition of command `icicle-complete-keys'
;;  (`S-TAB').
;;
;;  As another shortcut, if you want several candidates to have face
;;  `icicle-special-candidate, instead of giving each of their symbols
;;  this property you can simply define (e.g. bind)
;;  `icicle-special-candidate-regexp' to match those candidates.
;;  (This works, whether the candidates are strings or symbols.)
;;
;;(@* "Applying Text Properties to a Candidate String")
;;  ** Applying Text Properties to a Candidate String **
;;
;;  In this approach, you apply the text properties to the string(s)
;;  that represent the candidate, which you then pass to
;;  `completing-read' in its TABLE (alist) parameter.  You can use
;;  multi-completions, if you want to, propertizing each the way you
;;  want.
;;
;;  You can use any text properties you like, including these:
;;
;;  * `face' - to make some completion candidates stand out in
;;    particular ways
;;
;;  * `help-echo' - for individualized candidate help
;;
;;  * `keymap' and `pointer' - for individualized mouse treatment of
;;    candidates
;;
;;  * `display' - to include images in candidates
;;
;;  * `invisible' - to hide part or all of particular candidates
;;    (which are nevertheless available for completion)
;;
;;  How does this work?  Icicles simply redefines the standard Emacs
;;  function `display-completion-list' so that it retains text
;;  properties.  Emacs should do the same, but it doesn't (yet).
;;
;;  Icicles command `icicle-read-color' presents a simple
;;  illustration.  In this case, a multi-completion is used, composed
;;  of an unpropertized string that names a color and a propertized
;;  string that names its RGB (red, green, blue) value.  The RGB
;;  string, by default, has a background of the same color - each
;;  completion candidate is thus accompanied by its own color swatch.
;;
;;  The code that does this is in function
;;  `icicle-make-color-candidate', which is used by
;;  `icicle-read-color':
;;
;;   (defun icicle-make-color-candidate (color-name)
;;     "Return candidate of COLOR-NAME and its hex RGB string.
;;   If `icicle-WYSIWYG-Completions-flag' is non-nil, then the hex RGB
;;   string has the color as its background text property."
;;     (let ((rgb-string (hexrgb-color-name-to-hex color-name)))
;;       (when icicle-WYSIWYG-Completions-flag
;;         (put-text-property
;;           0 (length rgb-string) 'face
;;           (cons 'background-color rgb-string) rgb-string))
;;       (list (list color-name rgb-string))))
;;
;;  You'll notice that adding the face property is only added when
;;  option `icicle-WYSIWYG-Completions-flag' is non-nil.  Users can
;;  toggle this option at any time during completion to change the
;;  behavior.  (The new value takes effect for the next act of
;;  completion.)
;;
;;  Users can match any part of the multi-completion: color name or
;;  RGB value.  Command `icicle-read-color' defines a set of sort
;;  orders that are pertinent to the color candidates.  Users can use
;;  `C-,' to sort by color name, RGB value, hue, saturation, value, or
;;  amount of red, blue, or green.
;;
;;  Icicles treats reading face names similarly, by redefining
;;  standard function `read-face-name' when you are in Icicle mode.
;;  The pertinent function is this, which provides a WYSIWYG face
;;  sample whenever `icicle-WYSIWYG-Completions-flag' is non-nil.
;;
;;  A whole-number value for `icicle-WYSIWYG-Completions-flag'
;;  presents the face name accompanied by a separate sample swatch.  A
;;  value of t presents the face name itself in the face it names.
;;
;;   (defun icicle-make-face-candidate (face)
;;     "Return a completion candidate for FACE."
;;     (if (wholenump icicle-WYSIWYG-Completions-flag)
;;         (let ((swatch (make-string icicle-WYSIWYG-Completions-flag
;;                                    ?M)))
;;           (put-text-property 0 icicle-WYSIWYG-Completions-flag
;;                              'face face swatch)
;;           (list (list (symbol-name face) swatch)))
;;       (let ((face-name (copy-sequence (symbol-name face))))
;;         (when icicle-WYSIWYG-Completions-flag
;;           (put-text-property 0 (length face-name)
;;                              'face face face-name))
;;         (list face-name))))
;;
;;  See Also: (@> "Sorting Candidates and Removing Duplicates") for
;;  information about changing sort orders.
 
;;(@* "Defining Icicles Commands (Including Multi-Commands)")
;;
;;  Defining Icicles Commands (Including Multi-Commands)
;;  ----------------------------------------------------
;;
;;  This section is for Emacs-Lisp programmers.
;;
;;(@* "Nothing To It!")
;;  ** Nothing To It! **
;;
;;  Defining a command that uses Icicles completion and cycling is
;;  simple: just call `completing-read' or `read-file-name' to read
;;  input, then act on that input.
;;
;;  Nothing could be simpler - just use `completing-read'or
;;  `read-file-name'!  Icicles does the rest.  This is the most
;;  important thing to learn about defining Icicles commands: you
;;  don't need to do anything except call `completing-read' or
;;  `read-file-name' as you would normally anyway.
;;
;;  Or at least as I HOPE you would normally.  I fear that many
;;  Emacs-Lisp programmers don't take sufficient advantage of
;;  `completing-read' when they could, using instead a function such
;;  as (quel horreur !)  `read-string' to read user input.
;;
;;(@* "Multi-Commands Are Easy To Define Too")
;;  ** Multi-Commands Are Easy To Define Too **
;;
;;  If defining an Icicles command is trivial, so is defining an
;;  Icicles multi-command.  For the same effort it 
b6e2
takes to define a
;;  command that acts on a single input choice, you can have a command
;;  that acts on any number of input choices.  A multi-command takes
;;  advantage of an action function when cycling candidates, as
;;  described in sections (@> "Multi-Commands") and
;;  (@> "Choose All Completion Candidates").
;;
;;  In fact, there is no reason NOT to define your commands as
;;  multi-commands - you lose nothing, and you gain a lot.  Whenever
;;  it is appropriate for a user to possibly want to act on multiple
;;  objects, define a multi-command that does that.
;;
;;  Macros `icicle-define-command' and `icicle-define-file-command'
;;  make it easy to define a multi-command.  Without them, it is not
;;  so easy - see (@> "Defining Multi-Commands the Hard Way") for a
;;  taste of what is involved.  If you read that section first, make
;;  sure you come back here to see how easy things can be.
;;
;;  Here is how you might define a multi-command to delete one or more
;;  files or directories:
;;
;;  1. Define the multi-command, `my-delete-file':
;;
;;  (icicle-define-file-command
;;   my-delete-file                  ; Command name
;;   "Delete a file or directory."   ; Doc string
;;   my-delete-file-or-directory     ; Function to perform the action
;;   "Delete file or directory: "    ; `read-file-name' arguments...
;;   default-directory nil t)
;;
;;  2. Define the action function that deletes a single file:
;;
;;  (defun my-delete-file-or-directory (file)
;;    "Delete file (or directory) FILE."
;;    (condition-case i-delete-file
;;        (if (eq t (car (file-attributes file)))
;;            (delete-directory file)
;;          (delete-file file))
;;      (error (message (error-message-string i-delete-file))
;;             (error (error-message-string i-delete-file)))))
;;
;;  There are two parts to the definition of `my-delete-file':
;;
;;  1. The definition of the command itself, using
;;     `icicle-define-file-command'.
;;
;;  2. The definition of a helper, action function,
;;     `my-delete-file-or-directory', which deletes a single file (or
;;     directory), given its name.
;;
;;  It is #1 that is of interest here, because that is essentially
;;  what you do to define any multi-command.
;;
;;  The details of #2 are less interesting, even if more complex in
;;  this case: `my-delete-file-or-directory' checks whether its
;;  argument is a file or directory, and then tries to delete it.  If
;;  an error occurs, it prints the error message and then returns the
;;  message, so that the calling command can report on all deletion
;;  errors.
;;
;;  In #1, the arguments to `icicle-define-file-command' are
;;  straightforward:
;;
;;  * The name of the command being defined `my-delete-file'.
;;
;;  * Its doc string.
;;
;;  * The function that actually performs the action on the input file
;;    name - `my-delete-file-or-directory'.
;;
;;  * The arguments that you would supply anyway to `read-file-name'
;;    to read a single file name.
;;
;;  These are the SAME things you would need if you were defining a
;;  simple command to delete a SINGLE file or directory.  The only
;;  differences here are that you:
;;
;;  * Use `icicle-define-file-command' instead of `defun' with an
;;    `interactive' spec.
;;
;;  * Separate the action code into a separate function (here,
;;    `my-delete-file-or-directory') that acts on a single object
;;    (here, a file).
;;
;;  When you use `icicle-define-file-command', the action function is
;;  called on the result of `read-file-name', and it is also bound to
;;  `icicle-candidate-action-fn', so that it will be applied to the
;;  current candidate via `C-RET' (or `C-next' and so on).
;;
;;  Command `icicle-all-candidates-action' (`C-!' -- see
;   (@> "Choose All Completion Candidates")) can report in buffer
;;  *Help* on the objects that it did not act upon successfully.  For
;;  this reporting, the function bound to `icicle-candidate-action-fn'
;;  (e.g. `my-delete-file-or-directory', above) should return `nil'
;;  for "success" and non-`nil' (for example, an error message) for
;;  "failure", whatever "success" and "failure" might mean in the
;;  particular context of use.  This is not a requirement, except if
;;  you want to take advantage of such reporting.  For a command that
;;  deletes files, it is important to let the user know which
;;  deletions failed when s?he tries to delete all matching candidates
;;  at once.
;;
;;  If the command you want to define acts on objects other than
;;  files, then use `icicle-define-command' instead of
;;  `icicle-define-file-command' - the only difference is that you
;;  then supply the arguments for `completing-read' instead of those
;;  for `read-file-name'.
;;
;;  To let users know that a command is a multi-command, and how to
;;  use it as such, `icicle-define-command' and
;;  `icicle-define-file-command' add this explanation to the doc
;;  string you provide for the multi-command:
;;
;;  ---
;;  Read input, then call `<your action function name>' to act on it.
;;
;;  Input-candidate completion and cycling are available.  While
;;  cycling, these keys act on the current candidate:
;;
;;  `C-RET'  - Act on current completion candidate only
;;  `C-next' - Act, then move to next prefix-completion candidate
;;  `C-prior'- Act, then move to previous prefix-completion candidate
;;  `next'   - Act, then move to next apropos-completion candidate
;;  `prior'  - Act, then move to previous apropos-completion candidate
;;  `C-!'    - Act on *all* candidates, successively (careful!)
;;
;;  Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to
;;  quit.  This is an Icicles command - see `icicle-mode'.
;;  ---
;;
;;  Notice that the doc string of your new multi-command references
;;  your action function (e.g. `my-delete-file-or-directory').  The
;;  doc string you provide for the multi-command can thus be a little
;;  more abstract, leaving any detailed explanation of the action to
;;  the doc string of your action function.
;;
;;  To provide more flexibility, `icicle-define-command' and
;;  `icicle-define-file-command' provide some predefined bindings and
;;  allow for additional arguments.
;;
;;  Here is a definition of a multi-command, `change-font', that reads
;;  a font name and changes the selected frame to use that font.
;;
;;  1  (icicle-define-command
;;  2   change-font "Change font of current frame."
;;  3   (lambda (font)
;;  4     (modify-frame-parameters orig-frame
;;  5                              (list (cons 'font font))))
;;  6   "Font: " (mapcar #'list (x-list-fonts "*"))
;;  7   nil t nil nil nil nil
;;  8   ((orig-frame (selected-frame))
;;  9    (orig-font (frame-parameter nil 'font)))
;;  10  nil
;;  11  (modify-frame-parameters orig-frame
;;  12                           (list (cons 'font orig-font)))
;;  13  nil)
;;
;;  The arguments to `icicle-define-command' here are as follows:
;;
;;  Command name    (line 2)
;;  Doc string      (line 2)
;;  Action function (lines 3-5)
;;  Args passed to `completing-read' (lines 6-7)
;;  Additional bindings (lines 8-9)
;;  Additional initialization code (line 10)
;;  "Undo" code to run in case of error or user quit (lines 11-12)
;;  Additional code to run at the end (line 13)
;;
;;  The following bindings are predefined - you can refer to them in
;;  the command body:
;;
;;   `orig-buff'   is bound to (current-buffer)
;;   `orig-window' is bound to (selected-window)
;;
;;  Before running any "undo" code that you supply, the original
;;  buffer is restored, in case of error or user quit (`C-g').
;;
;;  Most of the arguments to `icicle-define-command' are optional.  In
;;  this case, optional arguments were provided to save (lines 8-9)
;;  and then restore (lines 11-12) the original font and frame.
;;
;;  Several top-level Icicles commands have been defined using
;;  `icicle-define-command' and `icicle-define-file-command'.  You can
;;  use their definitions as models for your own multi-commands.
;;
;;  `clear-option' (alias) - Set value of binary option to nil
;;  `icicle-add-buffer-candidate' - Add buffer to those always shown
;;  `icicle-add-buffer-config' - Add to `icicle-buffer-configs'
;;  `icicle-bookmark'     - Jump to a bookmark
;;  `icicle-buffer'       - Switch to another buffer
;;  `icicle-buffer-config' - Choose a config for `icicle-buffer'
;;  `icicle-buffer-list'  - Choose a list of buffer names
;;  `icicle-color-theme'  - Change color theme
;;  `icicle-comint-command' - Reuse a previous command in comint mode
;;  `icicle-delete-file'  - Delete a file or directory
;;  `icicle-delete-windows' - Delete windows showing a buffer anywhere
;;  `icicle-doc'          - Display doc of function, variable, or face
;;  `icicle-execute-extended-command' -
;;                          A multi-command version of `M-x'
;;  `icicle-execute-named-keyboard-macro' - Execute named kbd macro
;;  `icicle-face-list'    - Choose a list of face names
;;  `icicle-file-list'    - Choose a list of file names
;;  `icicle-find-file'    - Open a file or directory
;;  `icicle-find-tag'     - Visit definition with tag
;;  `icicle-font'         - Change the frame font
;;  `icicle-frame-bg'     - Change the frame background color
;;  `icicle-frame-fg'     - Change the frame foreground color
;;  `icicle-fundoc'       - Display the doc of a function
;;  `icicle-goto-global-marker' - Go to a global marker
;;  `icicle-goto-marker'  - Go to a marker in this buffer
;;  `icicle-insert-kill'  - Insert entries from `kill-ring'
;;  `icicle-insert-thesaurus-entry' -
;;                          Insert thesaurus entry
;;  `icicle-keyword-list' - Choose a list of keywords (regexps)
;;  `icicle-kill-buffer'  - Kill a buffer
;;  `icicle-kmacro'       - Execute a keyboard macro
;;  `icicle-locate-file'  - Open a file located anywhere
;;  `icicle-recent-file'  - Open a recently used file
;;  `icicle-remove-buffer-candidate' - 
;;                          Remove buffer from those always shown
;;  `icicle-remove-buffer-config' - 
;;                          Remove from `icicle-buffer-configs'
;;  `icicle-remove-saved-completion-set' - Remove a set from
;;                          `icicle-saved-completion-sets'
;;  `icicle-reset-option-to-nil' -
;;                          Set value of binary option to nil
;;  `icicle-select-frame' - Select frame by name and raise it
;;  `icicle-select-window' - Select window by its buffer name
;;  `icicle-other-window-or-frame' - Other window/frame or select it
;;  `icicle-set-option-to-t' -
;;                          Set value of binary option to t
;;  `icicle-toggle-option' - Toggle the value of a binary option
;;  `icicle-vardoc'       - Display the doc of a variable
;;  `toggle' (alias)      - Toggle the value of a binary option
;;
;;  For simplicity, the descriptions of these commands are singular
;;  actions (e.g. "kill a buffer"), but each of them can be used to
;;  act on any number of items any number of times (e.g. kill one or
;;  more buffers).  I recommend that you follow a similar naming
;;  convention - remember that the doc string will let users know that
;;  the command can be used on multiple objects.
;;
;;(@* "Are Users Dependent on Icicles To Use Multi-Commands?")
;;  ** Are Users Dependent on Icicles To Use Multi-Commands? **
;;
;;  For users to be able to take advantage of the Icicles features
;;  that your multi-command provides, they must load Icicles.  You can
;;  do this for them, by adding (require 'icicles nil t) to your code.
;;  The last two arguments mean that no error will be raised if for
;;  some reason Icicles cannot be found or successfully loaded.
;;
;;  But that brings up another question: What happens to your
;;  multi-command if Icicles is not available for a user, or s?he
;;  doesn't want to load it? No problem - your multi-command then
;;  automatically turns into a normal, single-choice command -
;;  graceful degradation.
;;
;;  Similarly, users can always turn off `icicle-mode' at any time, to
;;  return to the standard Emacs behavior.
;;
;;  Users will, in any case, need to load Icicles at compile time, in
;;  order to byte-compile your library that calls macro
;;  `icicle-define-command' or `icicle-define-file-command' - either
;;  that, or you can duplicate the definition of the macro in your
;;  library.  To let users load Icicles at (only) compile time, add
;;  this to your library that defines multi-commands:
;;
;;  (eval-when-compile '(require icicles))
;;
;;  See Also:
;;
;;  * (@> "Defining Multiple-Choice Menus").
;;
;;  * (@> "Note to Programmers") for further programming guidelines.
;;
;;  * Library `synonyms.el', which uses `icicle-define-command' to
;;    define command `synonyms'.  This command lets you use Icicles
;;    completion on input regexps when you search a thesaurus.
;;
;;  * Library `palette.el', which uses `icicle-define-command' to
;;    define command `palette-pick-color-by-name-multi'.  This command
;;    lets you use Icicles completion on input regexps when you choose
;;    a palette color by name.
;;
;;  * Library `highlight.el', which uses `icicle-define-command' to
;;    defined commands `hlt-choose-faces', `hlt-choose-visible-faces',
;;    and `hlt-choose-invisible-faces'.  These commands let you choose
;;    a set of faces.
 
;;(@* "Defining Multiple-Choice Menus")
;;
;;  Defining Multiple-Choice Menus
;;  ------------------------------
;;
;;  Icicles multi-commands (see (@> "Multi-Commands")) can be used
;;  provide users with multiple-choice menus.  While the possible
;;  choices can be accessed by minibuffer completion or cycling, a
;;  user can also display them in buffer *Completions* using `TAB' or
;;  `S-TAB', and click them there to choose them.
;;
;;  That is, buffer *Completions* can act as a multiple-choice menu.
;;
;;  Simple use case: Suppose that you use special characters (Greek
;;  letters, math symbols, accented letters in another language...),
;;  but only occasionally - you don't want to take the trouble to
;;  learn a special input method for them or flip to a different soft
;;  keyboard.  One simple way to handle this is to create a menu of
;;  such special characters - Greek letters, for instance.  You only
;;  need to create the menu once, providing the necessary completions
;;  as, say, Unicode characters.  When you need to input such a
;;  character, just use your command that pops up buffer *Completions*
;;  with the available special characters.  Even if you don't know how
;;  to type them on your keyboard, you can cycle through them or use
;;  `mouse-2' to choose them.
;;
;;  Here's a simple example of defining a command that uses a
;;  multiple-choice menu.  (Other examples given above, such as
;;  `my-delete-file-or-directory' are also examples, but this one uses
;;  menu items that look more like menu items.)
;;
;;  (icicle-define-command my-menu-command
;;      "Display menu and act on choice(s)."
;;      my-menu-action
;;      "`TAB' for menu.  `C-mouse-2' to choose. "
;;      my-menu-items nil t)
;;
;;  (defvar my-menu-items 
;;    '(("Foobar" . foobar-fn) ("Toto" . toto-fn) ("Titi" . titi-fn))
;;    "Alist of menu items and their associated commands.")  
;;
;;  (defun my-menu-action (item)
;;    "Call function associated with menu-item ITEM."
;;    (funcall (cdr (assoc item my-menu-items))))
;;
;;  (defun foobar-fn () (message "Foobar chosen"))                    
;;  (defun toto-fn () (message "Toto chosen"))                     
;;  (defun titi-fn () (message "Titi chosen"))
;;
;;  A user does `M-x my-menu-command' and hits `TAB' to display this
;;  menu in the *Completions* buffer:
;;
;;  Click mouse-2 on a completion to select it.  (C-h: help)
;;
;;  Possible completions are:
;;  Foobar          Titi
;;  Toto
;;
;;  The user presses and holds the Control key.  S?he clicks `Foobar'
;;  - message "Foobar chosen" appears.  S?he clicks `Toto - message
;;  "Toto chosen" appears.
;;
;;  And so on - all while holding Control pressed.  Any number of menu
;;  items can be chosen, any number of times.  The command is finally
;;  exited with `RET' or `C-g'.
;;
;;  The `TABLE' argument passed to `completing-read' here is
;;  `my-menu-items', an alist of key-value pairs, where the key is a
;;  menu-item name and the value is the function that implements the
;;  menu item.  For example, menu item `Foobar' is implemented by
;;  function `foobar-fn', and the alist element is therefore ("Foobar"
;;  . foobar-fn).
;;
;;  Function `my-menu-action' is executed when a user clicks
;;  `C-mouse-2' on a menu item.  It just looks up the menu item's
;;  function in alist `my-menu-items', and then calls that function.
;;
;;  What?  You think it's odd that the user must hit `TAB' to display
;;  the menu?  Then just use this code instead:
;;
;;  (icicle-define-command
;;   my-menu-command
;;   "Display menu and act on choice(s)."
;;   my-menu-action
;;   "`C-mouse-2' or `C-RET' to choose menu items"
;;   my-menu-items nil t nil nil nil nil
;;   ((icicle-show-*Completions*-initially-flag t)))
;;
;;  This just adds a binding for
;;  `icicle-show-*Completions*-initially-flag', so that *Completions*
;;  is displayed initially.
;;
;;  Granted, the *Completions* display doesn't exactly look like your
;;  average menu.  And the header line doesn't mention the
;;  multiple-choice possibility (holding Control while clicking).  But
;;  the header does say to use `C-h' for help, and that help does
;;  mention `C-mouse-2' (as does the prompt).  And the menu does act
;;  like a menu.  And the doc string of `my-menu-command' can provide
;;  more help, as needed.
;;
;;  There are also some freebie advantages of using such menus,
;;  besides the feature of multiple-choice.  These include choosing
;;  menu items from the keyboard, with completion, and cycling among
;;  menu items.  The additional features are all explained when the
;;  user hits `C-?'.
;;
;;  One common use of a multiple-choice menu is letting the user
;;  select a list of items from a larger list of candidates.  The list
;;  is returned, with the items in the order selected.  Examples of
;;  this include these multi-commands:
;;
;;  * `icicle-buffer-list' - buffer names, selected from `buffer-list'
;;    (possibly after filtering)
;;
;;  * `icicle-face-list' - face names, selected from `face-list'
;;
;;  * `icicle-file-list' - file names, selected from the files in the
;;    current directory
;;
;;  * `icicle-keyword-list' - keywords (regexps), selected from those
;;    you have previously entered
;;
;;  * `hlt-choose-faces', `hlt-choose-visible-faces',
;;    `hlt-choose-invisible-faces' - face names, selected from the
;;    (visible/invisible) highlighting faces in the buffer
;;
;;  Such commands can be used on their own, or they can be used in the
;;  `interactive' specs of other commands that act on an entire list
;;  of selected items.
;;
;;  Here, for example, is the definition of `icicle-file-list':
;;
;;   (icicle-define-command icicle-file-list
;;     "Choose a list of file names.
;;   The list of names (strings) is returned."
;;     (lambda (name) (push name file-names))
;;     "Choose file (`RET' when done): "
;;     (mapcar #'list (directory-files default-directory nil
;;                                     icicle-re-no-dot))
;;     nil nil nil 'file-name-history nil nil
;;     ((file-names nil))                    ; Additional bindings
;;     nil nil
;;     (prog1 (setq file-names (delete "" file-names)) ; Return list
;;       (when (interactive-p) (message "Files: %S" file-names))))
 
;;(@* "Defining Icicles Multi `M-x'")
;;  
;;  Defining Icicles Multi `M-x'
;;  ----------------------------
;;
;;  This section is for Emacs-Lisp programmers.  It explains how the
;;  Icicles Multi `M-x' feature is implemented, providing an advanced
;;  illustration of using macro `icicle-define-command'.
;;
;;(@* "How Multi `M-x' is Defined")
;;  ** How Multi `M-x' is Defined **
;;
;;  The definition of `icicle-execute-extended-command' provides an
;;  interesting illustration of using `icicle-define-command'.  The
;;  candidate action function itself binds a candidate action
;;  function, in case the candidate is a command that reads input with
;;  completion.
;;
;;  (icicle-define-command 
;;    icicle-execute-extended-command   ; `M-x' in Icicle mode.
;;    "Read command name, then read its arguments and call it.
;;    icicle-execute-extended-command-1 ; Action function
;;    (format "Execute command%s: "     ; `completing-read' args
;;            (if current-prefix-arg
;;                (format " (prefix %d)"
;;                        (prefix-numeric-value current-prefix-arg))
;;               ""))
;;    obarray 'commandp t nil 'extended-command-history nil nil
;;    ((last-cmd last-command))        ; Save the last command.
;;    nil (setq last-command last-cmd) ; Undo: restore last command.
;;    (setq last-command last-cmd))    ; Last: restore last command.
;;
;;  (defun icicle-execute-extended-command-1 (cmd-name)
;;    "Action function for `icicle-execute-extended-command'."
;;     (set-buffer orig-buff) ; bound by `icicle-define-command'.
;;     (select-window orig-window)
;;     (let ((icicle-candidate-action-fn
;;            (lambda (x) (funcall (intern cmd-name) x))))
;;       (run-hooks 'post-command-hook)
;;       (setq this-command cmd)
;;       (run-hooks 'pre-command-hook)
;;       (let ((enable-recursive-minibuffers t))
;;         (call-interactively (intern cmd-name) 'record-it))))
;;
;;  The last seven lines of this action function rebind
;;  `icicle-candidate-action-fn' to a function that calls the
;;  candidate `cmd-name' on a single argument that it reads.  This is
;;  useful if `cmd-name' is a command that, itself, reads an input
;;  argument with completion.  When that is the case, you can use
;;  completion on that input, and if you do that, you can use `C-RET'
;;  to use command `cmd-name' as a multi-command.  In other words,
;;  this binding allows for two levels of multi-commands.
;;
;;  There are a few things wrong with this definition, however.  In
;;  the action function, the candidate command is applied to a
;;  candidate that is a string.  What if it is a command, such as
;;  `describe-variable', that expects a symbol argument?  Or a number
;;  argument?  There is no way to know what kind of command will be
;;  used, and what kind of argument it will need.  The solution is to
;;  first try a string candidate argument, then convert the string to
;;  a symbol or number.  That is, bind this to
;;  `icicle-candidate-action-fn':
;;
;;  (lambda (x) 
;;    (condition-case nil
;;        (funcall cmd x)    ; Try to use a string candidate.  If that
;;      (wrong-type-argument ; didn't work, use a symbol or number.
;;       (funcall cmd (car (read-from-string x))))))
;;
;;  A similar problem occurs if the action function called doesn't
;;  accept a (single) argument.  The best thing to do in this case is
;;  punt - call `icicle-help-on-candidate' to display help on the
;;  candidate. To the code above, we add another error handler:
;;
;;  (wrong-number-of-arguments (funcall #'icicle-help-on-candidate))
;;
;;  And what if the command `cmd' does something that changes the
;;  focus away from the minibuffer's frame?  That's the case for
;;  `describe-variable', for instance: it selects buffer `*Help*'.  To
;;  fix this potential problem, the action function needs to reset the
;;  focus back to the minibuffer frame:
;;
;;  (lambda (x) 
;;    (condition-case nil
;;        (funcall cmd x)
;;      (wrong-type-argument (funcall cmd (car (read-from-string x))))
;;      (wrong-number-of-arguments
;;       (funcall #'icicle-help-on-candidate)))
;;    (select-frame-set-input-focus
;;      (window-frame (minibuffer-window))))
;;
;; See Also: (@> "Icicles Multi `M-x'").
 
;;(@* "Defining Multi-Commands the Hard Way")
;;  
;;  Defining Multi-Commands the Hard Way
;;  ------------------------------------
;;
;;  This section is for Emacs-Lisp programmers.  It gives you a taste
;;  of what is involved behind the scene when you effortlessly use
;;  `icicle-define-command' or `icicle-define-file-command' to define
;;  a multi-command.
;;  See (@> "Defining Icicles Commands (Including Multi-Commands)").
;;
;;  It can be good to know this, if only for the rare case where you
;;  need to define a multi-command that has special behavior not
;;  provided by `icicle-define(-file)-command' out of the box.  For
;;  example, if you want the normal, single-choice `RET' behavior to
;;  be different from the multiple-choice `C-RET' behavior, then you
;;  might want to roll your own.  Likewise, if you want to define your
;;  own help on individual candidates, to be invoked when users use
;;  `C-M-RET' and so on.
;;
;;  To write your own multi-command, you must make the command do
;;  this:
;;
;;  1. Call `completing-read' or `read-file-name', and perform some
;;     action on the completed input.
;;
;;  2. Bind `icicle-candidate-action-fn' to a function that performs
;;     an action on a completion candidate - possibly the same action
;;     as #1.
;;
;;  3. Optionally, bind `icicle-candidate-help-fn' to a function that
;;     displays specialized help for an individual candidate.
;;
;;  #1 just lets people use the command normally, to perform the #1
;;  action on a completion candidate entered with `RET'.  Because of
;;  #2, people can perform the #2 action on any completion candidates,
;;  while still continuing to cycle or complete candidates.
;;  Typically, the actions for #1 and #2 are the same, but nothing
;;  prevents you from using different actions.
;;
;;  When internal variable `icicle-candidate-action-fn' is not bound,
;;  the default action is performed: display help on the current
;;  completion candidate.  When `icicle-candidate-help-fn' is not
;;  bound, the default help display is used.
;;
;;  Here is a definition of a simple (not multi-) command that reads a
;;  font name and then changes the selected frame to use that font.
;;  By virtue of calling `completing-read', Icicles completion and
;;  cycling are available, using all available font names as the pool
;;  of candidates.
;;
;;  (defun change-font ()
;;    "Change font of selected frame."
;;    (modify-frame-parameters
;;     (selected-frame)
;;     (list (cons 'font (completing-read
;;                        "Font: " (mapcar #'list (x-list-fonts "*"))
;;                        nil t)))))
;;
;;  Here's a definition of a multi-command `change-font' that takes
;;  advantage of an action function when cycling candidates:
;;
;;  1  (defun change-font ()
;;  2    "Change font of current frame."
;;  3    (interactive)
;;  4   (let* ((orig-frame (selected-frame))
;;  5          (orig-font (frame-parameter nil 'font))
;;  6          (icicle-candidate-action-fn
;;  7           ;; Perform the action on a candidate, without leaving
;;  8           ;; `completing-read'.  You can do this over and over.
;;  9           (lambda (font)
;;  10             (modify-frame-parameters orig-frame
;;  11                                      (list (cons 'font font))))))
;;  12     (condition-case nil
;;  13         (modify-frame-parameters
;;  14          orig-frame
;;  15          (list
;;  16           (cons 'font
;;  17                 ;; Perform the action on your final choice.
;;  18                 (completing-read
;;  19                  "Font: "
;;  20                  (mapcar #'list (x-list-fonts "*")) nil t))))
;;  21       ((quit error)
;;  22        (modify-frame-parameters
;;  23         orig-frame
;;  24         (list (cons 'font orig-font)))))))
;;
;;  As you can see, there is a lot more going on here than in the
;;  simple-command version.  These are the points to keep in mind,
;;  when defining a multi-command by hand:
;;
;;  1. Save anything you need to restore, so you can, in effect, undo
;;     the action in case of `C-g' (lines 4-5).
;;
;;  2. Bind `icicle-candidate-action-fn' to the action to perform
;;     (lines 6-11).
;;
;;  3. Perform the action, using `completing-read' to provide the
;;     target candidate (lines 13-20).  Do this in the body of a
;;     `condition-case' (lines 12-24).
;;
;;  4. Restore the original context in the error-handling part of the
;;     `condition-case' (lines 22-24).  Include `quit' in the
;;     error-type list.
;;
;;  The above definition is not quite complete, in fact.  To let
;;  `icicle-all-candidates' be able to report on failures, the
;;  `icicle-candidate-action-fn' code should also trap errors and
;;  return nil as an error indicator.
;;
;;  In fact, things can get even hairier (much hairier) still, if the
;;  function at the core of your command does things like create a new
;;  frame - especially on MS Windows, with its click-to-focus window
;;  manager.  The action of `change-font' doesn't do that, but if it
;;  did, you would need to redirect the focus back to the minibuffer
;;  frame, using `select-frame-set-input-focus'.  As an illustration
;;  of what's involved, here's a definition that would deal with such
;;  problems.  It also traps `icicle-candidate-action-fn' errors,
;;  returning nil to report success and the error message to report
;;  failure.
;;
;;  (defun change-font ()
;;    "Change font of current frame."
;;    (interactive)
;;    (let* ((orig-buff (current-buffer))
;;           (orig-window (selected-window))
;;           (orig-frame (selected-frame))
;;           (orig-font (frame-parameter nil 'font))
;;           (icicle-candidate-action-fn
;;            (lambda (candidate)
;;              (condition-case action-fn-return
;;                  (progn
;;                    (modify-frame-parameters
;;                     orig-frame (list (cons 'font candidate)))
;;                    (select-frame-set-input-focus
;;                     (window-frame (minibuffer-window)))
;;                    nil) ; Return nil to report success.
;;                ;; Return error message to report error.
;;                (error (error-message-string action-fn-return))))))
;;      (condition-case act-on-choice
;;          (modify-frame-parameters
;;           orig-frame
;;           (list (cons 'font
;;                       (completing-read
;;                        "Font: " (mapcar #'list (x-list-fonts "*"))
;;                        nil t nil nil nil nil))))
;;        (quit (switch-to-buffer orig-buff)
;;              (modify-frame-parameters
;;               orig-frame (list (cons 'font orig-font))))
;;        (error (switch-to-buffer orig-buff)
;;               (modify-frame-parameters
;;                orig-frame (list (cons 'font orig-font)))
;;               (error (error-message-string act-on-choice))))))
;;
;;  That's a lot of (error-prone) work!  You obviously don't want to
;;  be doing that a lot.  Whenever you can, you should use macro
;;  `icicle-define-command' or `icicle-define-file-command' to define
;;  your multi-commands.
;;
;;  See (@> "Defining Icicles Commands (Including Multi-Commands)") for the
;;  easy way to define `change-font'.
 
;;(@* "Global Filters")
;;
;;  Global Filters
;;  --------------
;;
;;  This section is for Emacs-Lisp programmers.
;;
;;  Which completion candidates get displayed?  To review:
;;
;;  1. The domain of discourse, that is, all possible candidates, is
;;     determined by the arguments to `completing-read',
;;     `read-file-name', or `M-x'.
;;
;;  2. A user types something in the minibuffer.  This narrows the
;;     possible candidates to those that match the input.  Matching
;;     can be prefix-matching or apropos-matching.
;;
;;  Wouldn't it sometimes be useful to filter #1 in a global way,
;;  before filtering it with the user input (#2)?  Functions
;;  `completing-read' and `read-file-name' take a predicate argument,
;;  so that can be used for global filtering.  However, those
;;  functions are usually called from some command, and it would also
;;  be useful to give end users, not just programmers, some way to
;;  globally filter candidates.
;;
;;  For example, if you have a command, such as `icicle-buffer', that
;;  reads a buffer name and displays the buffer, some users might
;;  always be interested only in buffers that are associated with
;;  files.  They don't want to see possible candidates like
;;  `*scratch*' and `*Messages*'.  What they need is a way to apply a
;;  global predicate that limits candidates to file-buffer names - but
;;  they don't have access to the call to `completing-read' that is
;;  inside the command definition.
;;
;;  For this reason, some global filtering variables are provided by
;;  Icicles:
;;
;;    `icicle-must-match-regexp', `icicle-must-not-match-regexp',
;;    `icicle-must-pass-predicate', `icicle-extra-candidates'.
;;
;;  The first and second of these are regexps that candidates must
;;  match and must not match, respectively, in order for them to be
;;  displayed.  The third is a predicate that candidates must satisfy.
;;  The fourth is a list of extra candidates to display.  Any of the
;;  filters can be nil, in which case it has no effect.
;;
;;  Variable `icicle-extra-candidates' is not really a "filter".  It
;;  does not restrict the set of possible candidates - rather, it
;;  extends that set.
;;
;;  These global variables are internal variables - they are not meant
;;  to be customized.  If you are not an Emacs-Lisp programmer, you
;;  will not use these variables, but some commands that you use might
;;  provide corresponding global-filter user options.  Icicles
;;  provides customizable user options for command `icicle-buffer',
;;  for example:
;;
;;    `icicle-buffer-match-regexp'    - Regexp that buffers must match
;;    `icicle-buffer-no-match-regexp' - Regexp buffers must not match
;;    `icicle-buffer-predicate'       - Predicate buffer must satisfy
;;    `icicle-buffer-extras'          - Extra buffers to display
;;
;;  You might, for instance, customize `icicle-buffer-no-match-regexp'
;;  to not display file-buffers whose names end in `.elc', and
;;  customize `icicle-buffer-predicate' to show only buffers that are
;;  associated with files.  The former would use a value of "\\.elc$",
;;  and the latter would use a value such as this:
;;
;;     (lambda (bufname) (buffer-file-name (get-buffer bufname)))."
;;
;;  If you, as a programmer, write a command, and you want to expose
;;  global filters to users of the command, you should:
;;
;;  1. Create corresponding user options that can be customized.
;;  2. Bind the user options to the corresponding filtering variables.
;;
;;  If you use `icicle-define-command' or `icicle-define-file-command'
;;  to define a command (recommended), then you can simply pass the
;;  filter-variable bindings as part of the BINDINGS argument.
;;
;;  For example, here is the core definition of `icicle-buffer':
;;
;;   (icicle-define-command
;;    icicle-buffer                          ; Command name
;;    "Switch to a different buffer."        ; Doc string
;;    switch-to-buffer                       ; Action function
;;    "Switch to buffer: "                   ; completing-read args
;;    (mapcar (lambda (buf) (list (buffer-name buf)))
;;            (buffer-list))
;;    nil nil (buffer-name (if (fboundp 'another-buffer)
;;                             (another-buffer nil t)
;;                           (other-buffer (current-buffer))))
;;    nil nil nil
;;    ;; Filter bindings
;;    ((icicle-must-match-regexp icicle-buffer-match-regexp)
;;     (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
;;     (icicle-must-pass-predicate icicle-buffer-predicate)
;;     (icicle-extra-candidates icicle-buffer-extras)
;;     (icicle-sort-function icicle-buffer-sort)))
;;
;;  If you define a command that uses completion, but you don't use
;;  `icicle-define-command' or `icicle-define-file-command', then you
;;  can just bind such variables around a call to `completing-read' or
;;  `read-file-name'.  Command `icicle-complete-keys' presents an
;;  example of this, binding `icicle-buffer-no-match-regexp'.
;;
;;  Another way that users can apply predicates to completion
;;  candidates is to use `M-&' while completing.  These predicates
;;  apply to the full alist-entry candidates that are supplied to
;;  `completing-read' or `read-file-name', not just to the textual
;;  candidates that are displayed in buffer *Completions*.
;;  See (@> "Progressive Completion").
 
;;(@* "Note to Programmers")
;;
;;  Note to Programmers
;;  -------------------
;;
;;  Here are some simple guidelines for using Icicles in Emacs-Lisp
;;  programming:
;;
;;  1. *Use it*!  Even if you don't do anything else, include this in
;;     your library:
;;
;;     (require 'icicles nil t)
;;
;;     That has absolutely no consequences if Icicles is not present
;;     in the user's `load-path' (there is no load error).  If Icicles
;;     is present, however, then users can take advantage of each use
;;     you make of `completing-read' and `read-file-name' in your
;;     code.
;;
;;  2. Use an input-completion read function, such as
;;     `completing-read' or `read-file-name', when you read input!
;;     There is almost never a reason not to use an input-completion
;;     function when reading user input - especially considering that
;;     you need not always provide a REQUIRE-MATCH argument.
;;
;;     Try also to find an appropriate PREDICATE argument, and a good
;;     set of default values to pass to `completing-read' as its TABLE
;;     argument.  Too often, I think, we use an overly general TABLE
;;     argument, such as the `obarray', and we don't provide a (good)
;;     PREDICATE.  Using an input-completion function with an
;;     appropriate candidate completion list and predicate can help
;;     users considerably.
;;
;;     If you want to also give users a way to customize a (different)
;;     predicate that applies only to the textual candidates that are
;;     displayed in buffer *Completions*, as opposed to the full
;;     alist-entry candidates that are supplied to `completing-read'
;;     or `read-file-name', then you can define a new user option and
;;     then bind internal variable `icicle-must-pass-predicate' to the
;;     value of that option. See (@> "Global Filters").
;;
;;  3. Avoid using a literal-string `interactive' spec (e.g.
;;     (interactive "fFile: ")) that reads input with completion.
;;     Instead, call `completing-read' or `read-file-name' within the
;;     `interactive' spec.  This saves Icicles users of progressive
;;     completion the need to hit `RET' multiple times to pass their
;;     input up through multiple levels of recursive minibuffers to
;;     the top level.  See (@> "Progressive Completion").
;;
;;  4. In many cases, it makes sense to define a multi-command, rather
;;     than a simple command.  People can always use a multi-command
;;     as a simple command, but not vice versa.
;;     See (@> "Multi-Commands"),
;;     (@> "Defining Icicles Commands (Including Multi-Commands)"),
;;     and (@> "Defining Multi-Commands the Hard Way").
;;
;;  5. Consider using `icicle-completing-read-history' instead of
;;     `read-from-minibuffer' or `read-string' for most purposes.
;;     This lets users complete their input against previously entered
;;     input.  Completion is lax, so they can also enter new input.
;;
;;  6. You can bind `icicle-sort-function' temporarily to any sort
;;     function you need.
;;
;;  7. Function `icicle-next-candidate' is a general framework for
;;     letting users cycle completions of partial input strings.  I
;;     use it to define the cycling behavior for both prefix and
;;     apropos completions.  You can use it to easily define other,
;;     application-specific input matching/completion/cycling
;;     behavior.  Just supply it with a function that takes the
;;     current partial user input (a string) and returns a list of
;;     candidate completions, however those might be defined.
;;
;;  8. If the potential number of completion candidates is enormous,
;;     then icompletion display in *Completions* can be slow.  In that
;;     case, consider turning it off for the duration of the command,
;;     by binding `icicle-incremental-completion-flag' to nil.  An
;;     alternative to turning it off is the approach taken in Icicles
;;     (e.g. `icicle-vardoc' and `icicle-insert-thesaurus-entry'):
;;     Just add a reminder to the doc string to tell users that they
;;     can toggle `icicle-incremental-completion-flag' with `C-#'.
;;
;;  9. Another of my libraries that can help programmers provide
;;     default values is `thingatpt+.el'.  It provides functions for
;;     picking up symbols, sexps, numbers, words, and other sorts of
;;     thing near the text cursor (`point').
;;
;;  See Also:
;;
;;  * (@> "Multi-Commands")
;;  * (@> "Defining Icicles Commands (Including Multi-Commands)")
;;  * (@> "Defining Multi-Commands the Hard Way")
;;  * (@> "Defining Multiple-Choice Menus")
;;  * (@> "Global Filters")
;;  * (@> "Multi-Completions")
 
;;(@* "La Petite Histoire")
;;
;;  La Petite Histoire
;;  ------------------
;;
;;  1. This library started life as `elect-mbuf.el', by Hans Koomen.
;;
;;    Original posting:
;;    From koomen@cs.rochester.edu Mon Jun 19 19:27:58 1989
;;    To: info-gnu-emacs@prep.ai.mit.edu
;;    Cc: Hans <Koomen@cs.rochester.edu>
;;    Subject: elect-mbuf.el
;;    Date: Tue, 13 Jun 89 15:17:07 -0400
;;
;;  2. I hacked and enhanced the library in various relatively minor
;;  ways over the years, maintaining it as `elect-mbuf.el' - see
;;  details under (@> "Change log") in file `icicles.el'.
;;
;;  I did not change the main functionality of the library during this
;;  period: it always cycled the COMPLETE list of (prefix) completion
;;  candidates passed to `completing-read'; it did not update the
;;  candidate list based on the current minibuffer contents.
;;
;;  So, for instance, if you had `M-x for' in the minibuffer, `C-n'
;;  would cycle among ALL Emacs commands, not just those that start
;;  with "for".  I used the library this way for fifteen years without
;;  thinking much about this behavior or the code behind it.
;;
;;  3. In July 2005, Lennart Borgman gave `elect-mbuf.el' a quick try,
;;  and intuitively expected to see behavior along the lines that you
;;  see now for prefix completion:
;;
;;  a. `C-n' should cycle completions relative to the current input,
;;     not all completions supplied to `completing-read'.
;;  b. If buffer *Completions* is displayed, `C-n' should highlight
;;     the current candidate there.
;;
;;  Good idea Lennart (<lennart.borgman.073@student.lu.se>).  So I
;;  implemented that behavior, and renamed the library "Icicles" (for,
;;  I suppose, "input cycles" or some such - or because it's "cool").
;;
;;  4. The code changes I made to implement #3 (completion cycling
;;  relative to current input) made me realize that other completion
;;  matchings could be implemented in a similar way.  Prefix
;;  completion (the completion provided by Emacs) is handy, but it is
;;  also sometimes a bit limited.  The idea of apropos completion
;;  occurred to me, and I implemented that as well.
;;
;;  5. I extended the library quite a bit more, in terms of
;;  convenience (highlighting, treatment of buffer *Completions*,...,
;;  but also in terms of functionality.  In particular, it now treats
;;  file names too.  And, because Emacs 21 and later versions use
;;  `read-file-name' for `find-file' and so on, Icicles now treats
;;  `read-file-name' the same as `completing-read'.
;;
;;  6. On another suggestion from LennartBorgman, I made Icicles take
;;  advantage of Delete Selection mode.  And I finally implemented it
;;  as a minor mode.
;;
;;  7, 8, 9,...  One thing has led to another, and I've just kept
;;  adding features.  Feature creep, I guess.  But the more I play
;;  with Icicles, the more I imagine new ways it might be made more
;;  useful.
 
;;(@* "Note on Non-Nil `pop-up-frames' on MS Windows")
;;
;;  Note on Non-Nil `pop-up-frames' on MS Windows
;;  ---------------------------------------------
;;
;;  If you use `pop-up-frames' = t, like I do, you might have noticed
;;  that Emacs completion does not play well with using separate
;;  frames for each buffer.  In particular, it does not play well with
;;  having a separate frame for buffer *Completions*.  When you try to
;;  complete input using `TAB', a new frame is created for buffer
;;  *Completions*, and, at least on MS Windows, it is selected, taking
;;  the input focus away from the original frame's minibuffer!
;;
;;  This means that, once the *Completions* buffer has been displayed
;;  in a separate frame, you cannot, for instance, cycle completion
;;  candidates, without first reselecting the original frame manually.
;;  You cannot even use normal completion - you cannot add text in the
;;  minibuffer, or delete text there, because the minibuffer in the
;;  original frame no longer has the input focus.  Bummer.
;;
;;  In general, Emacs does not play too well with one-buffer-per-frame
;;  (`pop-up-frames' = t), and this is a good example of that general
;;  problem.
;;
;;  I reported this Emacs bug.  I'm hoping it will be corrected in
;;  Emacs 22.x.
;;
;;  I don't have this problem of loss of frame input focus in my own
;;  setup, even though I use `pop-up-frames' = t, because I use my
;;  library `oneonone.el'.  (Try it!)  If you need a solution while
;;  waiting for Emacs 22, you can try doing something similar to what
;;  I do in `oneonone.el':
;;
;;  1. Use dedicated frames for both *Completions* and the minibuffer.
;;
;;  2. Display buffer *Completions* using a special-display function
;;  that explicitly redirects the input focus from the *Completions*
;;  frame back to the minibuffer frame.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; You need not load this file.  It contains only documentation.

(provide 'icicles-doc2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-doc2.el ends here

0

HTTP/1.1 200 OK
Date: Thu, 06 Sep 2007 07:04:21 GMT
Server: Apache/2.2.3 (Debian) DAV/2 SVN/1.4.2 mod_jk/1.2.18 mod_ssl/2.2.3 OpenSSL/0.9.8c
Etag: 1188946112
Cache-control: max-age=10
Last-Modified: Tue, 04 Sep 2007 22:48:32 GMT
Keep-Alive: timeout=15, max=97
Connection: Keep-Alive
Transfer-Encoding: chunked
Content-Type: text/plain; charset=UTF-8

15000
;;; icicles-cmd.el --- Top-level commands for Icicles
;;
;; Filename: icicles-cmd.el
;; Description: Top-level commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Tue Sep 04 15:33:10 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 11861
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-cmd.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `avoid', `cl', `color-theme',
;;   `cus-edit', `cus-face', `cus-load', `cus-start', `custom',
;;   `dired', `dired+', `dired-aux', `dired-x', `easymenu',
;;   `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;   `ediff-mult', `ediff-util', `ediff-wind', `ffap', `ffap-',
;;   `fit-frame', `frame-cmds', `frame-fns', `help-mode', `hexrgb',
;;   `icicles-fn', `icicles-mcmd', `icicles-opt', `icicles-var',
;;   `info', `info+', `kmacro', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `pp', `pp+', `strings', `thingatpt',
;;   `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  top-level commands (and a few non-interactive functions used in
;;  those commands).  For commands to be used mainly in the minibuffer
;;  or buffer *Completions*, see `icicles-mcmd.el'.  For Icicles
;;  documentation, see `icicles.el' .
;;
;;  Commands defined here - (+) means a multi-command:
;;
;;    (+) `a', (+) `any', (+)`buffer', (+)`clear-option', (+)`file',
;;    (+)`icicle-add-buffer-candidate', (+)`icicle-add-buffer-config',
;;    `icicle-add-candidate-to-saved-completion-set',
;;    `icicle-add-region', (+)`icicle-anything', `icicle-apropos',
;;    `icicle-apropos-command', `icicle-apropos-function',
;;    `icicle-apropos-option', `icicle-apropos-variable',
;;    `icicle-apropos-zippy', (+)`icicle-bookmark',
;;    (+)`icicle-buffer', (+)`icicle-buffer-config',
;;    (+)`icicle-buffer-list', (+)`icicle-buffer-other-window',
;;    (+)`icicle-color-theme', (+)`icicle-comint-command',
;;    (+)`icicle-comint-search', (+)`icicle-compilation-search',
;;    (+)`icicle-complete-keys', `icicle-complete-thesaurus-entry',
;;    `icicle-customize-apropos', `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options', (+)`icicle-customize-face',
;;    `icicle-customize-icicles-group', `icicle-dabbrev-completion',
;;    (+)`icicle-delete-file', (+)`icicle-delete-window',
;;    (+)`icicle-delete-windows',
;;    `icicle-dired-saved-file-candidates',
;;    `icicle-dired-saved-file-candidates-other-window',
;;    (+)`icicle-doc', (+)`icicle-exchange-point-and-mark',
;;    (+)`icicle-execute-extended-command',
;;    (+)`icicle-execute-named-keyboard-macro', (+)`icicle-face-list',
;;    (+)`icicle-file-list', (+)`icicle-find-file',
;;    (+)`icicle-find-file-other-window', (+)`icicle-find-tag',
;;    (+)`icicle-find-tag-other-window', (+)`icicle-font',
;;    (+)`icicle-frame-bg', (+)`icicle-frame-fg', (+)`icicle-fundoc',
;;    (+)`icicle-generic-S-tab', (+)`icicle-goto-global-marker',
;;    (+)`icicle-goto-marker', (+)`icicle-imenu',
;;    (+)`icicle-imenu-command',
;;    (+)`icicle-imenu-non-interactive-function',
;;    (+)`icicle-Info-goto-node', (+)`icicle-Info-goto-node-cmd',
;;    (+)`icicle-Info-index', (+)`icicle-Info-index-20',
;;    (+)`icicle-Info-index-cmd', `icicle-insert-char',
;;    (+)`icicle-insert-kill', (+)`icicle-insert-thesaurus-entry',
;;    (+)`icicle-keyword-list', (+)`icicle-kill-buffer',
;;    (+)`icicle-kmacro', `icicle-lisp-complete-symbol',
;;    (+)`icicle-locate-file', (+)`icicle-locate-file-other-window',
;;    (+)`icicle-map', `icicle-non-whitespace-string-p',
;;    (+)`icicle-object-action', (+)`icicle-occur',
;;    (+)`icicle-other-window-or-frame', (+)`icicle-plist',
;;    `icicle-read-color', `icicle-read-kbd-macro',
;;    (+)`icicle-recent-file', (+)`icicle-recent-file-other-window',
;;    (+)`icicle-regexp-list', `icicle-region-open-all-files',
;;    (+)`icicle-remove-all-regions-in-buffer',
;;    (+)`icicle-remove-buffer-candidate',
;;    (+)`icicle-remove-buffer-config',
;;    `icicle-remove-candidate-from-saved-completion-set',
;;    (+)`icicle-remove-region',
;;    (+)`icicle-remove-saved-completion-set',
;;    `icicle-repeat-complex-command',
;;    (+)`icicle-reset-option-to-nil',
;;    `icicle-save-string-to-variable', (+)`icicle-search',
;;    (+)`icicle-search-all-regions', (+)`icicle-search-buffer',
;;    (+)`icicle-search-char-property', (+)`icicle-search-file',
;;    (+)`icicle-search-generic', `icicle-search-highlight-cleanup',
;;    (+)`icicle-search-keywords',
;;    (+)`icicle-search-overlay-property', (+)`icicle-search-region',
;;    (+)`icicle-search-text-property', (+)`icicle-search-word',
;;    (+)`icicle-select-frame', (+)`icicle-select-region',
;;    (+)`icicle-select-window', `icicle-send-bug-report',
;;    (+)`icicle-set-option-to-t', (+)`icicle-toggle-option',
;;    (+)`icicle-vardoc', (+)`icicle-yank-insert', (+)`toggle',
;;    (+)`what-which-how'.
;;
;;  Non-interactive functions defined here:
;;
;;    `custom-variable-p', `icicle-add-key+cmd',
;;    `icicle-anything-candidate-value', `icicle-binary-option-p',
;;    `icicle-char-properties-in-buffer',
;;    `icicle-char-properties-in-buffers',
;;    `icicle-choose-anything-candidate',
;;    `icicle-choose-candidate-of-type',
;;    `icicle-comint-get-final-choice',
;;    `icicle-comint-get-minibuffer-input', `icicle-comint-hook-fn',
;;    `icicle-comint-send-input', `icicle-compilation-hook-fn',
;;    `icicle-compilation-search-in-context-fn',
;;    `icicle-complete-keys-1', `icicle-complete-keys-action',
;;    `icicle-customize-faces', `icicle-delete-file-or-directory',
;;    `icicle-delete-region-from-alist', `icicle-edmacro-parse-keys',
;;    `icicle-execute-extended-command-1',
;;    `icicle-find-file-other-window-w-wildcards',
;;    `icicle-find-file-w-wildcards', `icicle-flat-list',
;;    `icicle-get-anything-actions-for-type',
;;    `icicle-get-anything-cached-candidates',
;;    `icicle-get-anything-candidates',
;;    `icicle-get-anything-candidates-of-type',
;;    `icicle-get-anything-default-actions-for-type',
;;    `icicle-get-anything-types', `icicle-group-regexp',
;;    `icicle-imenu-command-p', `icicle-imenu-in-buffer-p',
;;    `icicle-imenu-non-interactive-function-p',
;;    `icicle-Info-goto-node-action', `icicle-Info-index-action',
;;    `icicle-insert-for-yank',
;;    `icicle-insert-thesaurus-entry-cand-fn',
;;    `icicle-keys+cmds-w-prefix', `icicle-kill-a-buffer',
;;    `icicle-kill-a-buffer-and-update-completions',
;;    `icicle-kmacro-action', `icicle-make-color-candidate',
;;    `icicle-map-action', `icicle-marker+text', `icicle-markers',
;;    `icicle-next-single-char-property-change',
;;    `icicle-read-single-key-description',
;;    `icicle-read-var-value-satisfying', `icicle-region-add-buffers',
;;    `icicle-region-help', `icicle-region-or-buffer-limits',
;;    `icicle-region-sorted', `icicle-remove-all-regions-action',
;;    `icicle-remove-buffer-candidate-action',
;;    `icicle-remove-buffer-config-action',
;;    `icicle-remove-color-duplicates', `icicle-search-action',
;;    `icicle-search-char-property-scan',
;;    `icicle-search-choose-buffers', `icicle-search-help',
;;    `icicle-search-highlight-all-input-matches',
;;    `icicle-search-highlight-and-maybe-replace',
;;    `icicle-search-highlight-input-matches-here',
;;    `icicle-search-in-context-default-fn',
;;    `icicle-search-property-args',
;;    `icicle-search-read-context-regexp', `icicle-search-read-word',
;;    `icicle-search-regexp-scan', `icicle-search-region-action',
;;    `icicle-select-region-action',
;;    `icicle-search-replace-candidate',
;;    `icicle-search-replace-fixed-case-p',
;;    `icicle-search-replace-match',
;;    `icicle-search-replace-search-hit', `icicle-search-where-arg',
;;    `icicle-this-command-keys-prefix'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-complete-keys-alist'.
;;
;;
;;  ***** NOTE: The following functions defined in `dabbrev.el' have
;;              been REDEFINED HERE:
;;
;;  `dabbrev-completion' - Use Icicles completion when you repeat
;;                         (`C-M-/').
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' have
;;              been REDEFINED in Icicles:
;;
;;  `lisp-complete-symbol' - Selects *Completions* window even if on
;;                           another frame.
;;
;;
;;  ***** NOTE: The following function defined in `simple.el' has
;;              been REDEFINED HERE:
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;
;;  ***** NOTE: The following functions defined in `cus-edit.el' have
;;              been REDEFINED HERE:
;;
;;  `customize-apropos', `customize-apropos-faces',
;;  `customize-apropos-groups', `customize-apropos-options' -
;;     Use `completing-read' to read the regexp.
;;  `customize-face', `customize-face-other-window' - Multi-commands.
;;
;;
;;  Key bindings made by Icicles: See "Key Bindings" in `icicles.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Icicles multi-commands")
;;  (@> "Other top-level Icicles commands")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2007/09/04 dadams
;;     icicle-read-color: Added optional PROMPT arg.
;; 2007/08/31 dadams
;;     icicle-buffer-list: Prefix arg means only buffers visiting files are candidates.
;;     icicle-search-where-arg, icicle-occur:
;;       Prefix arg 99 means only buffers visiting files are candidates.
;;     Added: icicle-search-choose-buffers.
;; 2007/08/27 dadams
;;     icicle-search-action: Fixed for return value.  Display the error message.
;; 2007/08/25 dadams
;;     icicle-choose-candidate-of-type: Removed (> emacs-major-version 21) restriction.
;; 2007/08/25 dadams
;;     Added: a, any, buffer, file, icicle-get-anything-(types|(default-)actions-for-type|
;;            candidates-of-type|(cached-)candidates), icicle-anything(-candidate-value),
;;            what-which-how, icicle-choose-anything-candidate.
;;     Renamed icicle-clear-option to clear-option.
;;     icicle-object-action: Treat Anything stuff.
;;       Added optional type arg.  Use vars icicle-object-(named|predicate)-types.
;;       Move icicle-read-var-value-satisfying call here from icicle-choose-candidate-of-type.
;;     icicle-choose-candidate-of-type: Create buffer if needed.
;;     Protected alias definitions with icicle-define-alias-commands-flag. 
;;     icicle-map(-action):
;;       Use icicle-whole-candidate-as-text-prop-p and new icicle-get-alist-candidate.
;;     icicle-(select|remove|search)-region, icicle-search:
;;       Bind/use icicle-whole-candidate-as-text-prop-p.
;;     icicle-occur: Require buffer names to match existing buffers.  Thx to Hadron Quark.
;; 2007/08/15 dadams
;;     Added command toggle as alias for icicle-toggle-option.
;; 2007/08/09 dadams
;;     Soft require kmacro.  Define icicle-kmacro* if kmacro gets loaded.
;; 2007/07/27 dadams
;;     icicle-search: Renamed icicle-act-first-then-navigate-p to icicle-act-before-cycle-flag.
;; 2007/07/22 dadams
;;     icicle-customize-face: Added prefix arg to give vanilla completion.
;;     Added: icicle-customize-face-other-window.
;;     Replace standard customize-face(-other-window) by icicle-customize-face(-other-window).
;;     No longer require icicles-mode.el.
;;     Require cus-edit.el (not just at compile time).
;; 2007/07/08 dadams
;;     Added: icicle-customize-faces.
;;     icicle-customize-face: Bind icicle-all-candidates-action-fn to icicle-customize-faces.
;; 2007/06/23 dadams
;;     icicle-search-read-context-regexp, icicle-search-read-word, icicle-search-property-args,
;;      icicle-add-region, icicle-save-string-to-variable:
;;        Use icicle-completing-read-history, not read-from-minibuffer or read-string.
;;     icicle-face-list: Bound icicle-list-nth-parts-join-string etc.
;;     Moved to icicles-fn.el: icicle-read-from-minibuf-nil-default.
;; 2007/06/22 dadams
;;     Added: icicle-group-regexp, icicle-keyword-list, icicle-search-keywords.
;; 2007/06/21 dadams
;;     icicle-plist, icicle-(var|fun)doc, icicle-region-add-buffers, icicle-search-regexp-scan,
;;      icicle-search-char-property-scan:
;;        Use face icicle-candidate-part, not icicle-special-candidate.
;;     icicle-add-key+cmd: Highlight key side of the pair (key  =  binding).
;; 2007/06/18 dadams
;;     Added: icicle-customize-face.
;; 2007/06/17 dadams
;;     icicle-make-color-candidate: Respect icicle-WYSIWYG-Completions-flag.
;;     icicle-search-action: Added priority in call to icicle-place-overlay.
;; 2007/06/12 dadams
;;     icicle-execute-extended-command(-1): 
;;       i-e-e-c-1 prepares the new last command, but i-e-e-c sets it, at the end, to this-command.
;; 2007/06/10 dadams
;;     Added: icicle-comint-hook-fn, icicle-compilation-hook-fn.
;;     Removed unconditional add-hooks for comint-mode-hook and compilation(minor)-mode-hook.
;; 2007/06/09 dadams
;;     icicle-set-option-to-t, icicle-reset-option-to-nil, icicle-delete-windows,
;;      icicle-add-buffer-candidate, icicle-remove-buffer-candidate, icicle-buffer-list,
;;      icicle-remove-buffer-config, icicle-face-list, icicle-file-list,
;;      icicle-remove-all-regions-in-buffer, icicle-delete-file:
;;        Bind icicle-use-candidates-only-once-flag to t.
;;     icicle-set-option-to-t, icicle-clear-option: Candidate variables must have value nil/non-nil.
;;     icicle-search-regexp-scan: Added eobp test and empty hit-string test.
;;     icicle-select-(frame|window), icicle-delete-windows:
;;       Removed extra t arg to icicle-define-command.
;; 2007/06/08 dadams
;;     Added icicle-find-tag(-other-window).
;; 2007/06/07 dadams
;;     Renamed: icicle-font-history to icicle-font-name-history,
;;              icicle-function-history to icicle-function-name-history,
;;              icicle-variable-history to  icicle-variable-name-history.
;;     Use standard history variable if bound, else use Icicles history variable:
;;       bookmark-history, color-history, color-theme-history, face-name-history, font-name-history,
;;       frame-name-history, function-name-history, variable-name-history
;; 2007/06/05 dadams
;;     Don't require hexrgb.el if no window system.
;;     icicle-read-color: Use featurep, not require, for hexrgb.
;;     icicle-make-color-candidate: Error if hexrgb is not loaded.
;; 2007/05/31 dadams
;;     icicle-execute-extended-command-1: Set, don't bind this-command.
;;     icicle-execute-extended-command, icicle-execute-named-keyboard-macro:
;;       Simpler save and restore of last-command.
;; 2007/05/28 dadams
;;     icicle-imenu: Use same prefix-arg behavior as icicle-search (search multiple...).
;;     Added: icicle-imenu-(command|non-interactive-function(-p)).
;;     icicle-search-highlight-context-levels:
;;       Wrapped loop in condition-case.  Took max-levels determination out of loop.
;; 2007/05/25 dadams
;;     Added: icicle-face-list, icicle-next-single-char-property-change,
;;            icicle-search-(where-arg|property-args|char-property(-scan)|overlay-property),
;;            icicle-char-properties-in-buffer(s).
;;     Removed: icicle-text-properties-in-buffer(s), icicle-search-text-property-scan.
;;     icicle-search(-word): Use icicle-search-where-arg.
;; 2007/05/21 dadams
;;     Added: icicle-search-(buffer|file|all-regions).
;;     icicle-imenu: Fixed treatment of prefix arg.
;;     icicle-imenu-command-p: Fix to work also with Emacs 22 and  21.
;; 2007/05/14 dadams
;;     icicle-execute-extended-command-1: Error if empty command name.
;; 2007/05/12 dadams
;;     icicle-imenu: Added prefix arg for search for commands and non-cmds in Emacs-Lisp mode.
;;     Added: icicle-imenu-command-p.
;; 2007/05/11 dadams
;;     icicle-search-define-candidates:
;;       Move scan-fn-or-regexp to 4th arg from 2nd.  Removed &optional.
;;       Apply icicle-search-regexp-scan to args also.
;;     icicle-search-regexp-scan: Treat predicate.
;;       Move regexp to 3rd arg from 2nd, and add predicate arg.  Removed &optional.
;;     icicle-search-regexp-scan, icicle-search-text-property-scan:
;;       Add lengths of temp-list and icicle-candidates-alist.
;;     icicle-search-text-property(-scan): Added predicate argument.
;;     icicle-search-region-action: Bind icicle-candidate-action-fn to icicle-search-action.
;;     icicle-(select|search|remove)-region: Get rid of temp var bound to (icicle-region-sorted).
;;     icicle-search: Mention predicate in no-match message.
;; 2007/05/07 dadams
;;     icicle-add-buffer-config: Added history args for icicle-read-from-minibuf-nil-default.
;; 2007/05/06 dadams
;;     icicle-search: Bind icicle-must-pass-predicate to icicle-search-context-match-predicate.
;;     icicle-search-read-context-regexp: Don't accept an empty regexp.
;;     Changed S-C- to C-S- and M-C- to C-M- in doc.
;; 2007/05/04 dadams
;;     icicle-(re)set-option-to-(nil|t), icicle-toggle-option: Enable recursive minibuffers.
;; 2007/05/02 dadams
;;     icicle-search-read-word: Updated doc string.
;;     icicle-search: Respect icicle-search-whole-word-flag and icicle-regexp-quote-flag.
;;                    Single message for non-existent buffers.
;;     icicle-select-region-action: Message for non-existent file.
;;     Added empty defvars for Emacs 22 standard vars, to quiet byte compiler.
;; 2007/05/01 dadams
;;     icicle-execute-extended-command-1:
;;       Don't bind icicle-candidate-action-fn if we have already read command name.
;;     icicle-search-*-scan: Fill and reverse temp list, then append it to icicle-candidates-alist.
;;     Added: icicle-search-word, icicle-search-read-word.
;; 2007/04/29 dadams
;;     icicle-search, icicle-search-text-property, icicle-text-properties-in-buffers:
;;       Allow search of multiple files.  Change prefix arg accordingly.
;; 2007/04/28 dadams
;;     Added: icicle-compilation-search-in-context-fn, icicle-search-in-context-default-fn,
;;            icicle-search-highlight-and-maybe-replace.
;;     icicle-search-action: Move code to the new functions.  Use icicle-search-in-context-fn.
;;     icicle-compilation-search: Rewrote.
;;       No longer use compile-goto-error as icicle-search-hook.
;;       Use icicle-compilation-search-in-context-fn.
;;       Cannot-replace error msg if not Emacs 22, since no compilation-highlight-overlay.
;;       Provide .* regexp and do not highlight - as in icicle-occur.
;; 2007/04/22 dadams
;;     icicle-search: Allow for pre-bound icicle-candidate(-alternative)-action-fn.
;;     icicle-search-replace-search-hit:
;;       Call icicle-candidate-action-fn, not necessarily icicle-search-action.
;; 2007/04/20 dadams
;;     Added: icicle-search-highlight-context-levels.  Use in icicle-search-action.
;;     icicle-search-highlight-cleanup: Delete icicle-search-level-overlays.
;;     icicle-search: Set icicle-search-context-regexp to nil if scan-fn-or-regexp is not a string.
;;     icicle-search-replace-fixed-case-p: Return nil if arg is nil.
;;     icicle-search-read-context-regexp: Use default with read-number, and protect it with fboundp.
;;     Added soft require of strings.el.
;; 2007/04/18 dadams
;;     Added: icicle-search-replace-fixed-case-p.  Use in icicle-search-action.
;;     icicle-search-action:
;;       Set match data to region when icicle-search-replace-whole-candidate-flag is t.
;;       Don't search for icicle-search-context-regexp.
;;     icicle-search-replace-match: Added fixedcase arg.  Use icicle-search-replace-literally-flag.
;;     Use replace-match-maybe-edit (Emacs 22), not replace-match, and save and restore stuff.
;;     icicle-search-replace-search-hit:
;;       Use regexp-history in read-string.  Use icicle-search-define-replacement.
;; 2007/04/17 dadams
;;     Added: icicle-search-replace-match.  Treat Emacs < 22 also.  Use in icicle-search-action.
;;     icicle-search-replace-search-hit: Use regexp-history in read-string.
;; 2007/04/16 dadams
;;     icicle-search-action: Use replace-count.
;;     icicle-search: Initialize replace-count to 0.
;; 2007/04/15 dadams
;;     icicle-search-action: Allow \,... etc. replacement.
;;       Use query-replace-compile-replacement and replace-match, with error treatment.
;;       Removed unwind-protect.  Removed %s from error-msg return.
;;     icicle-search: Save search string as icicle-search-context-regexp.
;;                    Use "%s" for error message in failure error call.
;;                    Updated doc string for lisp-eval replacement etc.
;;     *(-all)-input-matches(-here): save-match-data around input matching.
;;     icicle-search-highlight-input-matches-here: Added not eobp to loop test.
;; 2007/04/13 dadams
;;     icicle-search-highlight-input-matches-here: Bound free var ov.
;; 2007/04/10 dadams
;;     icicle-search-regexp-scan: Use match indicated by icicle-search-context-level as context.
;;     Added: icicle-search-read-context-regexp.
;;     icicle-search(-region|-regexp-scan): Use icicle-search-read-context-regexp.
;; 2007/04/08 dadams
;;     icicle-map-action, icicle-search-action:
;;       Return nil for success, error msg for failure (it was reversed).
;;       Use icicle-highlight-candidate-in-Completions.
;;     icicle-map-action: Moved minibuffer frame selection to unwind-protect final clause.
;;     icicle-search: Give focus to original frame, in unwinding clause (C-g).
;;     Added: icicle-search-highlight-input-matches-here.
;;     icicle-search-highlight-all-input-matches: Highlight all input matches in candidate.
;;     icicle-search-replace-search-hit:
;;       Use 0 if icicle-candidate-nb is nil.  Display completions.
;;       Save and restore: icicle-last-completion-command, icicle-last-input.
;;       Prevent using same string as candidate for replacement.
;;     icicle-search-action: Rewrote.  Enable repeated replacment of input matches.
;;       Save and restore icicle-candidate-nb.  Wrap it around if at end.
;;       Warn about empty input for whole-candidate replacement.
;;       Update icicle-last-completion-candidate.  Display completions.
;; 2007/04/07 dadams
;;     icicle-search: Added query-replace functionality.
;;       Navigate first, then act (C-next).
;;       Give focus to last frame searched.
;;       Bind: icicle-candidate-alternative-action-fn, icicle-inhibit-sort-p, icicle-searching-p,
;;             icicle-expand-input-to-common-match-flag.
;;       Bind icicle-buffer-require-match-flag to partial-match-ok in interactive spec.
;;     icicle-search-action:
;;       Added optional replace arg: if non-nil, then fn becomes a replace action.
;;     Added: icicle-search-replace-search-hit, icicle-search-replace-candidate, 
;;     icicle-buffer-list: Bind all the stuff that is bound in icicle-buffer.
;; 2007/04/06 dadams
;;     icicle-execute-extended-command-1:
;;       Bind this-command, but only around call to cmd - don't set it.
;; 2007/04/02 dadams
;;     Added: icicle-search-text-property, icicle-search-text-property-scan,
;;            icicle-text-properties-in-buffer(s).
;;     icicle-search:
;;       Added &rest args.  Updated doc string.  Don't read regexp or reverse alist here.
;;       Use icicle-search-define-candidates, not icicle-search-regexp-scan.  
;;     icicle-search-regexp-scan: Read regexp here.  Reverse alist here.
;;     Moved to icicles-fn.el: icicle-filter-alist, icicle-first-matching-candidate.
;;     Renamed: icicle-search-region-beg-end to icicle-region-or-buffer-limits.
;; 2007/04/01 dadams
;;     icicle-repeat-complex-command:
;;       Remove duplicates and sort entries, but only for reading command to eval.
;; 2007/03/31 dadams
;;     icicle-lisp-complete-symbol: Bind icicle-top-level-when-sole-completion-flag to t.
;; 2007/03/27 dadams
;;     icicle-search: Hard-code C-next key in message.
;;     icicle-search-regexp-scan: Initialize last-beg for first time.
;; 2007/03/23 dadams
;;     icicle-execute-extended-command-1:
;;       Don't restore last-command.  Run (pre|post)-command-hook.  Set, don't bind, this-command.
;;       enable-recursive-minibuffers for interactive call of cmd.
;;     icicle-execute-extended-command, icicle-execute-named-keyboard-macro:
;;       Restore last-command at end.
;; 2007/03/20 dadams
;;     icicle-execute-extended-command-1: When wrong-number-of-arguments, icicle-help-on-candidate.
;; 2007/03/14 dadams
;;     icicle-dired-saved-file-candidates, icicle-buffer, icicle-(find|locate|recent)-file:
;;       Put 200 as value of property icicle-Completions-window-max-height.
;;     Added ;;;###autoload for icicle-define*.
;; 2007/03/09 dadams
;;     icicle-search, icicle-map, icicle-(remove|search|select)-region, icicle-imenu,
;;     icicle-occur, icicle-compilation-search:
;;       Bound icicle-transform-function to nil if interactive-p.
;;     icicle-comint-search: Updated doc string to mention false positives in command matching.
;;     Removed eval-when-compile from requires of Icicles libraries (except icicle-mac.el).
;; 2007/03/08 dadams
;;     icicle-insert-kill: Bound icicle-delete-candidate-object.
;; 2007/03/07 dadams
;;     icicle-delete-windows, icicle-map-action, icicle-search-action,
;;     icicle-choose-candidate-of-type, icicle-complete-keys-help:
;;       Use 0, not t, as frame arg to get-buffer-window.
;; 2007/03/06 dadams
;;     icicle-search, icicle-map, icicle-(remove|search|select)-region:
;;       Bind icicle-inhibit-sort-p to non-nil to prevent user sorting.
;;       Update doc string to mention that you cannot sort candidates.
;;     icicle-(remove|search|select)-region: Sort candidates by buffer and then by tag.
;;     icicle-search-region: Bound icicle-delete-candidate-object.
;;     icicle-search, icicle-map: Don't add or subtract one from candidate # if use action fn.
;;     icicle-search:
;;       If require-match, set icicle-completion-candidates and marker to reflect final choice.
;;     Renamed: icicle-get-current-candidate to icicle-get-alist-candidate.
;;     Added: icicle-region-sorted, icicle-region-add-buffers.
;; 2007/03/04 dadams
;;     icicle-get-current-candidate: Return nil if icicle-candidates-alist is nil.
;; 2007/03/02 dadams
;;     icicle-remove-buffer-config, icicle-remove-buffer-candidate: Removed extraneous quote.
;;     icicle-(find|recent)-file(-other-window): Bound icicle-delete-candidate-object.
;; 2007/02/28 dadams
;;     icicle-buffer-list, icicle-color-theme: Bound icicle-delete-candidate-object.
;; 2007/02/27 dadams
;;     icicle-recent-file(-other-window): Changed INITIAL-INPUT completing-read arg to nil.
;; 2007/02/25 dadams
;;     Added: icicle-remove-buffer-candidate-action, icicle-remove-buffer-config-action.
;;     icicle-add-buffer-candidate, icicle-bookmark, icicle-buffer-config:
;;       Bound icicle-delete-candidate-object.
;; 2007/02/24 dadams
;;     Added: icicle-kill-a-buffer-and-update-completions (was kill-a-buffer), 
;;            icicle-delete-region-from-alist.
;;     icicle-delete-window: Use icicle-kill-a-buffer...-completions, not icicle-kill-a-buffer.
;;     icicle-buffer(-other-window), icicle-choose-candidate-of-type:
;;       Bind icicle-delete-candidate-object to icicle-kill-a-buffer.
;;       Bind icicle-sort-function to icicle-sort-function if icicle-buffer-sort-function is nil.
;;     icicle-select-region:
;;       Bind icicle-delete-candidate-object to icicle-delete-region-from-alist.
;;     icicle-remove-region: Rewrote.
;;       Use icicle-delete-region-from-alist.
;;       Use icicle-remove-candidate-display-others, but don't redisplay unless completing.
;;       Respect icicle-add-buffer-name-flag (append buffer names).
;; 2007/02/20 dadams
;;     Added: icicle-search-region-action.  Open file associated with candidate.
;;     icicle-search-region:
;;       Use icicle-search-region-action.  Updated doc string.  Bind icicle-list-*.
;;       Fix default completing-read value.  Respect icicle-add-buffer-name-flag.
;;     icicle-select-region-action: Open file associated with candidate.
;;     icicle-region-open-all-files: Ignore files that are not readable.
;;     icicle-regions: Remove only non-existent buffers that are not associated with files.
;; 2007/02/19 dadams
;;     Added: icicle-region-open-all-files.
;;     icicle-(search|select)-region, icicle-search: Use icicle-region-open-all-files.
;;     icicle-add-region: Add file name to region entry in alist.
;;     icicle-select-region-action, icicle-region-help, icicle-search:
;;       Updated selectors for point and mark, because of addition of file name.
;;     icicle-region-help: Add file name to help message.
;; 2007/02/18 dadams
;;     Added: icicle-first-matching-candidate.
;;     icicle-get-current-candidate: Use icicle-first-matching-candidate, not assoc.
;; 2007/02/07 dadams
;;     icicle-search-action: Make *Completions* window dedicated.  Thx to Peter Povinec.
;; 2007/02/07 dadams
;;     icicle-search: pop-to-buffer and raise frame. Don't select orig-window.
;; 2007/02/06 dadams
;;     icicle-search, icicle-select-region, icicle-search-regexp-scan:
;;       Respect icicle-add-buffer-name-flag.
;;     icicle-search: Bound icicle-show-Completions-initially-flag to t.
;;                    Bound icicle-candidate-help-fn to icicle-search-help.
;;     icicle-search-regexp-scan: nil BUFFER arg means current buffer now.
;;     icicle-search-action, icicle-filter-alist: Treat multi-completion case also.
;;     Added: icicle-search-help.
;;     icicle-map-action, icicle-search-action: Removed unused local var, curr-cand-string.
;;     icicle-select-region, icicle-remove-region, icicle-search-region:
;;       Use icicle-candidates-alist, not regions-w-buffers (but bind it locally too).
;;     Renamed: icicle-get-current-region-candidate to icicle-get-current-candidate.
;;     icicle-region-help: Provide region limits also.
;;     Added note to some doc strings that the command is for Icicle mode only.
;; 2007/02/02 dadams
;;     icicle-search: Test for buffers, not buffer names, in WHERE.  Thx to Peter Povinec.
;;     icicle-buffer-list: Return the list of buffers.
;; 2007/01/28 dadams
;;     icicle-complete-keys:
;;       Bind icicle-sort-functions-alist, using icicle-command-names-alphabetic-p.
;; 2007/01/22 dadams
;;     icicle-read-color:
;;       Removed free var NTH-PARTS.
;;       Bound icicle-list-nth-parts-join-string the same as icicle-list-join-string.
;;       Remove duplicates: AliceBlue and alice blue etc.: downcase and remove whitespace.
;;     Added: icicle-remove-color-duplicates.
;;     Added soft require of hexrgb.el.
;;     icicle-*doc, icicle-plist: Bound icicle-candidate-properties-alist.
;; 2007/01/21 dadams
;;     Added: icicle-read-color, icicle-make-color-candidate.
;; 2007/01/18 dadams
;;     Added: icicle-get-current-region-candidate.
;;       Use current cand if cycled or `mouse-2'.  Else if single match, use that.  Else error.
;;     icicle-remove-region, icicle-search-region, icicle-region-help:
;;       Use icicle-get-current-region-candidate.
;;     icicle-add-region: Added optional TAG arg and prefix arg treatment.
;;     icicle-remove-region: Update completions list.  Bind regions-w-buffers.
;;     icicle-remove-all-regions-in-buffer: Use current buffer name as default.
;;     Added: icicle-select-region-action.
;;     icicle-select-region: Use icicle-select-region-action.
;;     Renamed: option icicle-regions to icicle-region-alist.
;;     icicle-regions: Sort entries.
;; 2007/01/17 dadams
;;    icicle-filter-alist: Reversed argument order.  Return alist arg if filter-keys arg is empty.
;; 2007/01/12 dadams
;;    icicle-delete-window: Do icicle-remove-Completions-window if in minibuffer.
;;    icicle-yank-insert: Do icicle-yank if in minibuffer.
;;    icicle-(fundoc|vardoc|doc|plist): Added condition-case to protect symbols that raise errors.
;; 2007/01/01 dadams
;;    Added: icicle-(add|remove)-candidate-(to|from)-saved-completion-set.
;;    icicle-add-buffer-config: Use nil, not "nil" as default, if icicle-buffer-sort is nil.
;;                              Use icicle-define-add-to-alist-command to define it.
;;    icicle-remove-buffer-config, icicle-remove-saved-completion-set:
;;      Use icicle-assoc-delete-all, not delete of assoc.
;;    icicle-remove-saved-completion-set: Update display after removal.
;;    Reformatted icicle-define(-file)-command, based on setup.el's lisp-indentation-hack.
;; 2006/12/25 dadams
;;    Bug fix: icicle-search-action: Use icicle-filter-alist on icicle-candidates-alist.
;;    icicle-(select|search)-region: Use pop-to-buffer and raise-frame, not set-buffer.
;;    icicle-select-region: Activate the region.
;; 2006/12/23 dadams
;;    Added: icicle-region-help.  Use in icicle-*-region.
;;    Added: icicle-remove-all-regions-in-buffer, icicle-remove-all-regions-action.
;;    icicle-(select|search)-region: Ignore regions in non-existent buffers.
;;    icicle-remove-region: Update the persistent value of icicle-regions.
;; 2006/12/22 dadams
;;    Added: icicle-exchange-point-and-mark.
;;    icicle-customize-icicles-group: icicles -> Icicles (group name).
;; 2006/12/18 dadams
;;    icicle-object-action: Remove print arg.  icicle-apply-to-* uses current-prefix-arg now.
;; 2006/12/17 dadams
;;    Added: icicle-object-action, icicle-choose-candidate-of-type,
;;           icicle-read-var-value-satisfying.
;; 2006/12/16 dadams
;;    icicle-map-action: Bug fix: Use icicle-candidate-nb, not assoc.
;;    Added: icicle-goto(-global)-marker, icicle-marker+text, icicle-markers.
;; 2006/12/10 dadams
;;    Moved minibuffer and *Completions* commands to new file, icicles-mcmd.el.
;;    Require icicles-opt.el.
;;    icicle-buffer-list: Added final message.
;; 2006/11/26 dadams
;;    icicle-search-action: Bug fix: Use icicle-candidate-nb, not assoc, to get cand+mrker.
;;    icicle-*-complete-1: Bug fix: Don't set icicle-current-input to icicle-last-input if nil.
;;    Renamed: icicle-search-region to icicle-search-region-beg-end.
;;    Added: icicle-(add|remove|select|search)-region.
;;    icicle-search: Use icicle-regions for numeric prefix arg.  Updated doc string.
;;    Added: icicle-Info-index-20 - thx to Tamas Patrovics.  Use it in icicle-Info-index.
;; 2006/11/25 dadams
;;    icicle-search: After final selection, select orig-window and give its frame input focus.
;; 2006/11/24 dadams
;;    Added: icicle-ensure-overriding-map-is-bound, icicle-universal-argument,
;;           icicle-universal-argument-more, icicle-negative-argument, icicle-digit-argument,
;;           icicle-universal-argument-other-key, icicle-universal-argument-minus,
;;           icicle-kmacro(-action).
;;    icicle-dabbrev-completion: Don't stop at common root, and use lax completion.
;;    Replaced icicle-select-window-or-frame by icicle-other-window-or-frame, which respects C-u 0.
;; 2006/11/23 dadams
;;    icicle-prefix-complete-1: Respect icicle-TAB-shows-candidates-flag.
;;    icicle-execute-extended-command-1: Treat named keyboard macros too.
;;    Added: icicle-execute-named-keyboard-macro.
;; 2006/11/18 dadams icicle-add/update-saved-completion-set, icicle-apropos*, icicle-bookmark,
;;    icicle-buffer-config, icicle-candidate-set-retrieve, icicle-candidate-set-save,
;;    icicle-color-theme, icicle-comint-command, icicle-complete-thesaurus-entry,
;;    icicle-customize-apropos*, icicle-delete-windows, icicle-font, icicle-frame-bg,
;;    icicle-frame-fg, icicle-insert-kill, icicle-insert-string-from-variable,
;;    icicle-insert-thesaurus-entry, icicle-locate-file*, icicle-map, icicle-narrow-candidates,
;;    icicle-remove-buffer-config, icicle-remove-saved-completion-set, icicle-reset-option-to-nil,
;;    icicle-save-string-to-variable, icicle-search, icicle-select-window, icicle-set-option-to-t,
;;    icicle-toggle-option:
;;      Use a specific history variable.
;; 2006/11/17 dadams
;;    Added: icicle-select-(frame|window), icicle-select-window-or-frame.
;; 2006/11/10 dadams
;;    icicle-mouse-candidate-action: read-event to swallow mouse up event.
;;    icicle-map-action: Don't use icicle-filter-alist - find string in icicle-candidates-alist.
;;                       Unwind-protect to reselect minibuffer frame.
;;                       Don't bind case-fold-search.
;;    icicle-map: enable-recursive-minibuffers.
;; 2006/11/09 dadams
;;    icicle-nb-of-candidate-in-Completions: Redefined using binary search, for better performance.
;;    icicle-toggle-ignored-space-prefix: Update doc string to use icicle-dispatch-C-^.
;;    icicle-search:
;;      Bind icicle-update-input-hook unconditionally, after icicle-search-regexp-scan.
;;    icicle-search-regexp-scan: Highlight up to icicle-search-highlight-threshold.
;;    icicle-search-highlight-all-input-matches:
;;      Only update input and highlight if icicle-search-highlight-all-current-flag.
;;    icicle-search-action: Don't use icicle-filter-alist - find string in icicle-candidates-alist.
;;    icicle-search-highlight-cleanup: Bind inhibit-quit to t.
;; 2006/11/07 dadams
;;    Added: icicle-toggle-highlight-all-current.
;; 2006/11/06 dadams
;;    icicle-search-action:
;;      Highlight icicle-current-input, not icicle-current-raw-input (not updated).
;;    Renamed icicle-search-highlight-all-flag to icicle-search-highlight-threshold.
;; 2006/11/05 dadams
;;    Added: icicle-search-regexp-scan.
;;    icicle-search:
;;      Added buffers arg.  Updated doc string.
;;      Use icicle-search-regexp-scan:  Scan each buffer in buffers.
;;                                      Add marker, not position, to icicle-candidates-alist.
;;      Go to candidate in its buffer.
;;      Added progress message.
;;    icicle-search-action: Pop to buffer of candidate (marker) and raise frame.
;;    icicle-occur: Added buffers arg.  Updated doc string.  Call icicle-search-highlight-cleanup.
;;    icicle-search-highlight-all-input-matches: set-buffer for each ov in dolist (minor opt.).
;;    icicle-search-highlight-cleanup: Added progress messages.  Minor optimization.
;; 2006/10/22 dadams
;;    icicle-complete-keys-action:
;;      Set last-nonmenu-event to non-mouse info, to ignore *Completions* click.
;;    icicle-complete-keys-1: Don't use a default value for completing-read.
;; 2006/10/21 dadams
;;    Added: icicle-insert-char.
;;    icicle-add-key+cmd: Respect icicle-complete-keys-self-insert-flag.
;; 2006/10/20 dadams
;;    icicle-map, icicle-delete-window: Corrected doc string.
;; 2006/10/16 dadams
;;    icicle-add-key+cmd: Protect :enable's eval with condition-case.
;;    icicle-complete-keys-1: No longer use icicle-extra-candidates.
;;                            Use default value of .. for completing-read (except at top level).
;;    icicle-complete-keys-action: Correct no-match case: must match whole and part.
;;    icicle-keys+cmds-w-prefix: Add .. to icicle-complete-keys-alist unless at top level.
;; 2006/10/15 dadams
;;    icicle-complete-keys:
;;      Bind icicle-complete-keys-action, not icicle-complete-keys-help, to icicle-*-action-fn.
;;      Bind orig-buff, orig-window, and icicle-completing-keys-p, for use elsewhere.
;;    Added: icicle-complete-keys-action.  Initial definition from code in icicle-complete-keys.
;;    icicle-complete-keys-action:
;;      Use orig-buff and orig-window; restore to originally selected window.
;;      Error if candidate doesn't match template xxx  =  yyy.
;;    icicle-complete-keys-1:
;;      Call icicle-complete-keys-action on chosen candidate.
;;    icicle-help-on-candidate: Treat key completion also.
;;    Added from cus-edit+.el: custom-variable-p.
;;    Moved to icicles-mode.el: icicle-select-minibuffer-contents, next-history-element.
;;    Moved here from icicles-mode.el: icicle-generic-S-tab.
;;    icicle-generic-S-tab (bug fix): Do not set last-command to icicle-apropos-complete.
;;    Added: eval-when-compile's.
;; 2006/10/13 dadams
;;    icicle-add-key+cmd:
;;      Add actual key to icicle-complete-keys-alist.  Thx to Stefan Monnier.
;;      Don't filter out index (Imenu) keymap.
;;      Treat :enable condition.
;;    icicle-complete-keys-1:
;;      Use actual key recorded in icicle-complete-keys-alist. Don't convert to key description.
;;      Treat digit-argument and negative-argument.
;;    icicle-complete-keys-alist: Updated doc string for new structure.
;; 2006/10/08 dadams
;;    Added: icicle-add-key+cmd, icicle-read-single-key-description.
;;    Added: icicle-complete-keys-alist.
;;           Use in icicle-complete-keys-1, icicle-keys+cmds-w-prefix, icicle-add-key+cmd.
;;    icicle-add-key+cmd: Update binding, depending on its type (menu item etc.).
;;      Push (cons candidate binding), not just candidate, onto icicle-complete-keys-alist.
;;    icicle-complete-keys-1:
;;      Use binding, not just command name.  Call it and put it in (this|last)-command.
;;      Flipped (corrected) use of icicle-key-descriptions-use-<>-flag.
;;      Use icicle-read-single-key-description.
;;    icicle-prefix-keys-first-p, icicle-complete-keys-1, icicle-complete-keys-help,
;;      icicle-keys+cmds-w-prefix: Don't use icicle-list-*-string.
;; 2006/10/05 dadams
;;    icicle-complete-keys-1: Remove icicle-special-candidate property from all candidates.
;;    icicle-keys+cmds-w-prefix:
;;      Intern candidate and, if local binding, put icicle-special-candidate property on it.
;;      Use single string for candidate (don't use multi-completion).
;; 2006/10/03 dadams
;;     icicle-complete-keys-1: Treat "..".
;;     icicle-complete-keys: Updated doc string accordingly.
;;     icicle-prefix-keys-first-p: ".." is less than all other strings.  Don't hard-code "= ".
;;     icicle-keys+cmds-w-prefix:
;;       Filtered out shadowed bindings, icicle-generic-S-tab, and icicle-complete-keys.
;;       Use only map-keymap & lookup-key, not accessible-keymaps, current-active-maps, map-keymap.
;; 2006/10/01 dadams
;;     icicle-complete-keys: Bind sort functions, to put prefix keys first, by default.
;;                           Set last-command, before recursing.
;;     Replaced icicle-alternative-sort with icicle-toggle-alternative-sorting (new).
;;     icicle-(apropos|prefix)-complete-1:
;;       Ensure icicle-(current|last)-input are strings, before compare.
;;     icicle-keys+cmds-w-prefix: Tolerate empty local and global maps.
;; 2006/09/30 dadams
;;     Added: icicle-read-kbd-macro, icicle-edmacro-parse-keys, icicle-toggle-angle-brackets.
;;     icicle-complete-keys-1, icicle-dabbrev-completion:
;;       key-description -> icicle-key-description, with icicle-key-descriptions-use-<>-flag.
;;     icicle-complete-keys-1:
;;       read-kbd-macro -> icicle-read-kbd-macro, with icicle-key-descriptions-use-<>-flag.
;;       Got rid of extra space in prompt before colon, when no prefix.
;;     icicle-keys+cmds-w-prefix: Use single-key-description with icicle-*-use-<>-flag.
;;     icicle-insert-key-description:
;;       Change arg to a toggle, and use icicle-key-descriptions-use-<>-flag.
;;     Bind icicle-candidate-set-retrieve, icicle-candidate-set-save to C-M-<, C-M->, not C-<, C->.
;;     icicle-dired-saved-file-candidates*:
;;       Changed doc strings and messages to use dynamic binding of icicle-candidate-set-save.
;; 2006/09/24 dadams
;;     Added: icicle-complete-keys-help.
;;     icicle-complete-keys:
;;       Bind icicle-*-action-fn to icicle-complete-keys-help.  Mention help keys in docstring.
;;     icicle-complete-keys-1:
;;       Set last-command to command, so completion doesn't think candidate was last-command.
;;     icicle-keys+cmds-w-prefix: Provide placeholder for future insertion of generic characters.
;; 2006/09/23 dadams
;;     icicle-complete-keys-1:
;;       Error if there are no keys for the prefix.
;;       Error, not self-insert-command, for key read-kbd-macro can't convert. Use condition-case.
;;       Report error if calling cmd fails.
;;       Use vconcat for recursive call.
;;       Read cmd, don't intern it - it might be a lambda or byte-compiled function.
;;       Remove duplicates.
;;       Provide KEYS arg to call-interactively, for error reporting. 
;;       No longer bind icicle-must-not-match-regexp to "^Character set .*=  self-insert-command"
;;     icicle-keys+cmds-w-prefix:
;;       Treat also local keymap and current minor maps.
;;       Do nothing if keys+maps is nil.
;;       Only map-keymap if the target is a keymap.
;;       Use keymapp, not functionp, as the binding test.
;;       Only add binding if it is a command or keymap.
;;       Only add self-insert-command binding if the key is char-valid-p.
;;       Use format %S, not %s for a command binding.
;;     icicle-insert-key-description: Added no-angle-brackets-p arg.
;; 2006/09/22 dadams
;;     icicle-complete-keys-1: Filter out keys described "Character set ...= self-insert-command".
;; 2006/09/20 dadams
;;     icicle-complete-keys-1: Treat self-insert-command specially.
;; 2006/09/17 dadams
;;     Added: icicle-complete-keys(-1), icicle-*-keys-prefix, icicle-keys+cmds-w-prefix, 
;;     icicle-doc: Removed one \n from each candidate.
;; 2006/09/12 dadams
;;     Renamed icicle-switch-to-minibuffer to icicle-insert-completion.
;;     Added: icicle-switch-to/from-minibuffer.
;;     icicle-completion-help: Keep focus in the minibuffer after displaying help.
;; 2006/09/02 dadams
;;     icicle-help-on-(next|previous)-(apropos|prefix)-candidate,
;;       icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Use save-selected-window, not save-window-excursion.
;;     icicle-find-file*: In Dired, ignore errors picking up current-line's file name.
;;     icicle-mouse-choose-completion: Error if minibuffer is not active.
;; 2006/08/27 dadams
;;     icicle-abort-minibuffer-input: If minibuffer is not active, just kill buffer *Completions*.
;;     icicle-execute-extended-command-1, icicle-insert-thesaurus-entry, icicle-search-action:
;;       Ensure orig-window is live before using it.
;; 2006/08/23 dadams
;;     Added: icicle-delete-window(s).
;;     Added soft require of frame-cmds.el.
;; 2006/08/22 dadams
;;     icicle-execute-extended-command-1: Bind this-command, don't set it (fixes C-next).
;;     icicle-help-on-candidate: If no last candidate, then reset to first matching candidate.
;;     icicle-*-*-candidate-action, icicle-help-on-*-*-candidate: save-window-excursion. (good?)
;; 2006/08/20 dadams
;;     icicle-find-file*: Use diredp-find-a-file* in Dired mode (Emacs 22 or later).
;;     Bug fix: icicle-candidate-action: Use icicle-*-candidates, not icicle-next-*-candidate.
;;              icicle-next-*-candidate(-action): Set icicle-current-completion-mode.
;; 2006/08/18 dadams
;;     Added: icicle-Info-goto-node(-(action|cmd)).
;;     icicle-candidate-action: If no icicle-last-completion-candidate, use the first candidate.
;; 2006/08/15 dadams
;;     Added: icicle-help-on-*-*-candidate,icicle-mouse-help-on-candidate.
;;     No longer put icicle-candidate-action-command property on symbols (not used).
;;     Added: icicle-raise-Completions-frame.
;;     icicle*-candidate-action, icicle-help-on-candidate: Use icicle-raise-Completions-frame.
;;     icicle-help-on-candidate: Can use it from *Completions* too now.
;;                               Use icicle-barf-if-outside-Completions-and-minibuffer.
;; 2006/08/13 dadams
;;     Added: icicle-Info-index(-(action|cmd)).
;; 2006/08/04 dadams
;;     icicle-*-complete-1, icicle-prefix-word-complete, icicle-keep-only-past-inputs:
;;       Set icicle-last-completion-command to the explicit command, not this-command.
;;     icicle-history: Call icicle-last-completion-command, not icicle-apropos-complete.
;;     icicle-apropos-complete-1, icicle-narrow-candidates:
;;       Removed binding of icicle-apropos-completing-p (not used).
;;     Added: icicle-plist, icicle-remove-Completions-window, icicle-pp-eval-expression.
;;     Added soft require of pp+.el.
;;     icicle-exit-minibuffericicle-minibuffer-complete-and-exit, icicle-mouse-choose-completion,
;;     icicle-abort-minibuffer-input, icicle-(apropos|prefix)-complete-1,
;;     icicle-keep-only-past-inputs, icicle-insert-thesaurus-entry-cand-fn:
;;       Use icicle-remove-Completions-window.
;;     icicle-doc: Treat doc of faces also.
;;     icicle-non-whitespace-string-p: Added doc string.
;; 2006/08/03 dadams
;;     Added:
;;       icicle-comint-command, icicle-insert-kill, icicle-insert-for-yank,icicle-yank-insert.
;;     Bound icicle-comint-command to C-c TAB in comint-mode.
;;     icicle-search, icicle-comint-search: Cleaned up doc string.
;; 2006/08/02 dadams
;;     icicle-comint-search: Mention *-prompt-pattern.  Thx to Kevin Rodgers.
;;     icicle-insert-string-from-variable: Added some more variables to the completing-read alist.
;; 2006/07/29 dadams
;;     Added: icicle-dispatch-C-., toggle-icicle-search-cleanup, icicle-toggle-search-cleanup.
;; 2006/07/23 dadams
;;     Added: icicle-toggle-transforming.
;;     icicle-comint-search: Bind icicle-transform-function to icicle-remove-duplicates.
;; 2006/07/22 dadams
;;     Added: icicle-comint-search, icicle-comint-send-input, icicle-comint-get-minibuffer-input,
;;            icicle-comint-get-final-choice, icicle-search-generic.
;;     icicle-search: Added require-match arg for non-interactive calls.
;;                    Run the hooks if no match and no match required, and if we didn't cycle.
;;                    Return final choice as value (not used yet).
;;     icicle-insert-string-from-variable: Use buffer-local value of variable, if there is one.
;;     icicle-insert-string-from-variable:
;;       Make sure we use the buffer-local value of the variable, if there is one
;;       Added comint-prompt-regexp to regexp list.
;;     Added mode hooks for icicle-compilation-search and icicle-comint-send-input.
;; 2006/07/20 dadams
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>:
;;       Added: icicle-(next|previous)-context-candidate, icicle-scroll-completions.
;;       icicle-switch-to-completions, icicle-switch-to-Completions-buf,
;;         icicle-move-to-next-completion, icicle-map-action, icicle-search-action:
;;           Use icicle-start-of-completions.
;;       icicle-(apropos|prefix)-complete-1:
;;         Set icicle-current-completion-type vs use icicle-arrows-respect-completion-type-flag.
;;         Use icicle-scroll-completions.
;;       icicle-current-completion-in-Completions: Use point-min if no previous prop change.
;;       icicle-keep-only-past-inputs: Use icicle-scroll-completions.
;;     Renamed icicle-start-of-completions to icicle-start-of-candidates-in-Completions,
;;             icicle-current-completion-type to icicle-current-completion-mode,
;;             icicle-*-context-candidate to icicle-(next|previous)-candidate-per-mode,
;;             icicle-scroll-completions to icicle-scroll-Completions.
;;     icicle-(next|previous)-context-candidate: Use icicle-barf-if-outside-minibuffer.
;;     icicle-scroll-Completions: Changed with-selected-window to Emacs 20 equivalent.
;; 2006/07/18 dadams
;;     icicle-search: Bind completion-ignore-case to case-fold-search.
;;     icicle-search-highlight-all-input-matches, icicle-search-action:
;;       Put search inside condition-case, for bad regexp.
;;     Added: icicle-toggle-case-sensitivity, toggle-icicle-case-sensitivity.
;; 2006/07/10 dadams
;;     Added: icicle-search-region.  Use in search functions.  Thx to Le Wang.
;; 2006/07/08 dadams
;;     icicle-search-highlight-all-input-matches: Bug fix - Use *-current-*, not *-current-raw-*.
;;     icicle-execute-extended-command-1:
;;       First try a string candidate as arg, then read it to convert it to symbol or number.
;;       Reset focus back to the minibuffer, in action function.
;; 2006/07/07 dadams
;;     Added: icicle-alternative-sort.
;;     icicle-imenu: Show *Completions* initially for submenu choice (only).
;;     icicle-execute-extended-command: Echo prefix arg in prompt.  Thx: *.dhcp.mdsn.wi.charter.com
;; 2006/07/06 dadams
;;     Added (eval-when-compile (require 'icicles-mac)).
;; 2006/07/05 dadams
;;     Renamed: icicle-current-regexp-input to icicle-current-raw-input.
;;     icicle-prefix-complete-1: Don't set icicle-current-raw-input.
;; 2006/07/04 dadams
;;     icicle-prefix-complete-1: No longer calculate common prefix and set current input to it.
;;     Added plist entries to categorize commands:
;;       icicle-(cycling|completing|candidate-action)-command.
;;     icicle-(apropos|prefix)-complete-1, icicle-prefix-word-complete,
;;     icicle-switch-to-Completions-buf, icicle-keep-only-past-inputs, icicle-history:
;;       Use icicle-cycling-command property.
;;     icicle-apropos-complete-1: Removed regexp-p arg in call to icicle-save-or-restore-input.
;; 2006/07/03 dadams
;;     icicle-(apropos|prefix)-complete-1: deactivate mark after inserting current input.
;; 2006/06/18 dadams
;;     icicle-apropos-complete-1, icicle-narrow-candidates: Bind icicle-apropos-completing-p.
;; 2006/06/09 dadams
;;     Bug fixes: Picked up matching subdir as default dir, even if there other files match. 
;;                  Thx to Andrey Zhdanov.
;;                Empty directory not treated as a match.
;;     icicle-(apropos|prefix)-complete-1:
;;       If input matches an empty directory, then use that directory as the sole completion.
;;       Do not expand file-name input before call icicle-file-name-(apropos|prefix)-candidates.
;;     icicle-retrieve-last-input: Use insert, not icicle-insert-input (no longer used).
;;                                 (Input backslashes reverted to slashes.)
;; 2006/06/08 dadams
;;     Bug fix: Could not complete after cycling file names.  Thx to Andrey Zhdanov.
;;     icicle-insert-input: Use icicle-expand-file-name.
;;     icicle-prefix-complete-1:
;;       Expand file-name input before call icicle-file-name-prefix-candidates.
;;       Expand icicle-last-completion-candidate if it is a directory name.
;; 2006/05/30 dadams
;;     icicle-erase-minibuffer-or-history-element: Fix for consecutive deletions.
;; 2006/05/26 dadams
;;     Added: icicle-erase-minibuffer-or-history-element.
;; 2006/05/19 dadams
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;;     icicle-narrow-candidates: Bind icicle-reminder-prompt-flag to nil, not t.
;; 2006/05/16 dadams
;;     Added: icicle-kill(-a)-buffer.
;; 2006/05/15 dadams
;;     Renamed: icicle-completion-nospace-flag to icicle-ignore-space-prefix-flag.
;;     icicle-candidate-set-complement: Put back icicle-ignore-space-prefix-flag.
;;     icicle-buffer(-other-window): Bind icicle-buffer-ignore-space-prefix-flag.
;;     Added: icicle-toggle-ignored-space-prefix, toggle-icicle-ignored-space-prefix.
;; 2006/05/13 dadams
;;     icicle-occur: Make icicle-search-main-regexp-others unnoticeable instead of
;;                   setting icicle-search-highlight-all-flag to nil.
;;     icicle-candidate-set-complement: Use nil, not icicle-completion-nospace-flag.
;;     Renamed: icicle-search-imenu to icicle-imenu,
;;              icicle-search-imenu-in-buffer-p to icicle-imenu-in-buffer-p.
;; 2006/05/12 dadams
;;     icicle-search-imenu: Remove unmatched submenus.  Error if no imenu for the buffer.
;;     Added: icicle-search-imenu-in-buffer-p.
;;     icicle-insert-string-at-point: Use icicle-barf-if-outside-minibuffer.
;;     Moved to icicles-fn.el: icicle-barf-if-outside-*.
;;     Moved some commands to minibuffer-cmds section from top-level cmds section.
;; 2006/05/09 dadams
;;     Added: icicle-customize-icicles-group, icicle-send-bug-report, icicle-customize-button.
;; 2006/04/30 dadams
;;     Added: icicle-map, icicle-map-action.
;;     icicle-filter-alist: Corrected and simplified.
;;     icicle-search: Corrected cand-nb adjustment when cycle with action fns.
;;     Renamed: icicle-search-action-fn to icicle-search-action,
;;              icicle-search-candidates to icicle-candidates-alist.
;; 2006/04/28 dadams
;;     icicle-retrieve-last-input, icicle-(apropos|prefix)-complete-1:
;;       Use icicle-highlight-initial-whitespace.
;; 2006/04/25 dadams
;;     icicle-completion-help: Emacs 21.3's help-insert-xref-button signature is different.
;; 2006/04/16 dadams
;;     Added: icicle-search-imenu.
;;     icicle-search: Bug fixes:
;;       Treat completion without cycling: error or singleton go-to.
;;       Only subtract one from candidate number for C- cycling, not regular cycling.
;; 2006/04/14 dadams
;;     icicle-search:
;;       Bug fix: Position was off by one.
;;       Highlight input match inside each main regexp match (or not).
;;         Bind icicle-update-input-hook and icicle-incremental-completion-flag.
;;       Extract code to define icicle-search-action-fn.
;;       Use icicle-search-candidates instead of local variable search-candidates.
;;       Respect icicle-search-cleanup-flag.
;;     Added: icicle-search-highlight-*, icicle-search-action-fn,
;;            icicle-(insert|save)-text-(from|to)-variable.
;;     Renamed icicle-search-refined-regexp to icicle-search-current-input.
;; 2006/04/09 dadams
;;     icicle-(apropos|prefix)-complete-1: Deal with icicle-arrows-respect-completion-type-flag.
;;     Moved here from icicles-fn.el: icicle-customize-apropos*, icicle-repeat-complex-command.
;; 2006/04/07 dadams
;;     icicle-search: Highlight all occurrences at once (like isearch highlighting, but not lazy).
;;                    Error if no match for initial regexp.
;;     icicle-occur: Bind icicle-search-highlight-all-flag to nil, so don't highlight each line.
;; 2006/04/02 dadms
;;     Added: icicle-toggle-regexp-quote, icicle-find-file*-w-wildcards.
;;     icicle-find-file*: Use icicle-find-file*-w-wildcards.
;; 2006/03/31 dadams
;;     icicle-search: Wrap action function with unwind-protect to select minibuffer frame.
;;                    Use completion-ignore-case when highlighting search hits.
;;                    Protect delete-overlay with overlayp.
;;                    Turn off region highlighting (so can see highlighting done here).
;;                    Removed sit-for-period argument.
;;     icicle-candidate-set-save: Use prin1 instead of pp.
;; 2006/03/27 dadams
;;     Added: icicle-occur.
;;     icicle-search: Highlight also match of current regexp, inside that of regexp arg.
;;                    Use new faces icicle-search-*-regexp.
;;     icicle-search, icicle-switch-to-Completions-buf, icicle-move-to-next-completion:
;;       Use new, generic icicle-place-overlay.
;;     Removed icicle-place-search-overlay.
;; 2006/03/26 dadams
;;     icicle-search: Use icicle-search-overlay.  Ensure don't match twice at same position.
;;                    Added regexp arg.  Use 0 as sit-for default.
;;     Added: icicle-place-search-overlay.
;; 2006/03/25 dadams
;;     icicle-prefix-complete: Minor bug fix: Don't save try-completion if not a string.
;;     icicle-candidate-set-(save|retrieve): Allow use of a variable to save/retrieve.
;;     Added: icicle-candidate-set-(retrieve-from|save-to)-variable, icicle-*-complete-no-display,
;;            icicle-prefix-complete-1.
;;     icicle-apropos-complete-1: Added no-display-p optional arg.
;;     Use no-display-p arg in calls to icicle-display-candidates-in-Completions.
;;     icicle-candidate-set-(retrieve-from|save-to)-cache-file: Pass a consp, not t.
;;     icicle-candidate-set-retrieve: Don't display *Completions*.
;; 2006/03/24 dadams
;;     Added icicle-delete-char.
;; 2006/03/23 dadams
;;     icicle-candidate-set-define: Rewrote.  Can also use at top level.
;;       Error if wrong result type.  Sort result.  Use display-completion-list and
;;       icicle-narrow-candidates (unless at top level).
;;     icicle-narrow-candidates: Can call from top-level (but not interactively).
;;     icicle-candidate-set-complement: Use icicle-maybe-sort-and-strip-candidates.
;;     Mention in doc strings of minibuffer and *Completions* functions: where, key.
;; 2006/03/22 dadams
;;     icicle-find-file*: Use default-directory as default, so opens directory on empty input.
;;     icicle-prefix-complete:
;;       Save icicle-current-regexp-input.
;;       Set icicle-current-input to common prefix.  Use it everywhere here.
;;     Calls to icicle-display-candidates-in-Completions: no root arg now.
;; 2006/03/21 dadams
;;     icicle-insert-input: Bug fix: Use directory of input, not default-directory.
;;                                   Append a slash if input itself is a directory.
;; 2006/03/20 dadams
;;     icicle-retrieve-last-input: Insert icicle-current-regexp-input if repeat C-l.
;;     Added: icicle-insert-input.
;; 2006/03/19 dadams
;;     icicle-apropos-complete-1: Call icicle-save-or-restore-input with non-nil regexp-p arg.
;; 2006/03/17 dadams
;;     Added: icicle-add/update-saved-completion-set, icicle-remove-saved-completion-set,
;;            icicle-retrieve-candidates-from-set.
;;     Removed: icicle-cache-file.
;;     icicle-candidate-set-retrieve: Read candidates set and use its cache file.
;;                                    Enable recursive minibuffers.
;;     icicle-candidate-set-save: Read candidates set and cache-file names.
;;                                Use icicle-add/update-saved-completion-set.
;;     icicle-barf-if-outside-minibuffer: Move interactive test to calling functions.
;;     icicle-files-within: Moved to icicle-fn.el.
;; 2006/03/16 dadams
;;     Added: icicle*-saved-completion-set.
;; 2006/03/14 dadams
;;     icicle-narrow-candidates: Handle no-catch error.  Don't use icicle-completing-p.
;;     icicle-candidate-set-complement:
;;       Do what we do in icicle-candidate-set-retrieve: call icicle-narrow-candidates.
;;     icicle-candidate-set-(retrieve|complement): Msg when display.
;;     icicle-(apropos|prefix)-complete-1:
;;       Removed test for last-command = icicle-candidate-set-complement.
;; 2006/03/13 dadams
;;     Added: icicle-candidate-set-(retrieve-from|save-to)-cache-file.
;;     icicle-candidate-set-(retrieve|save): C-u uses cache file.
;; 2006/03/12 dadams
;;     Added: icicle-dired-saved-file-candidates(-other-window), icicle-locate-file*,
;;            icicle-files-within.
;; 2006/03/11 dadams
;;     icicle-find-file*, icicle-delete-file*:
;;       Reverted to simple form (moved directory control to icicles-mac.el).
;;     icicle-keep-only-past-inputs: Expand file name relative to directory of last input.
;; 2006/03/10 dadams
;;     icicle-find-file*, icicle-delete-file*: Expand file name relative to dir of last input.
;;     Renamed icicle-minibuffer-contents to icicle-minibuffer-contents-from-minibuffer.
;; 2006/03/09 dadams
;;     icicle-barf-if-outside-*: Removed argument - use this-command instead.
;; 2006/03/08 dadams
;;     icicle-bookmark: Use default value, not init value, for completing-read.
;; 2006/03/07 dadams
;;     icicle-doc: Save table in minibuffer-completion-table, so can access via C-RET too.
;;     icicle-insert-thesaurus-entry, icicle-*doc:
;;       Removed binding of icicle-incremental-completion-flag to nil.
;;     Added: icicle-barf-if-outside-(minibuffer|Completions).  Use in appropriate commands.
;;     Added: icicle-non-whitespace-string-p.  Use in icicle-*doc.
;; 2006/03/06 dadams
;;     Update doc strings of *-thesaurus*.
;; 2006/03/05 dadams
;;     Added: icicle-toggle-incremental-completion, toggle-icicle-incremental-completion.
;; 2006/03/03 dadams
;;     icicle-*doc: Clarified doc strings.  Updated prompts.
;;     Added: icicle-help-button.  Use in icicle-completion-help.
;; 2006/03/02 dadams
;;     icicle-insert-thesaurus-entry, icicle-complete-thesaurus-entry:
;;       Use synonyms-ensure-synonyms-read-from-cache.  Clarified doc strings.
;;     icicle-complete-thesaurus-entry: Error if no word at point.  Corrected looking-at regexp.
;; 2006/03/01 dadams
;;     Added: icicle-insert-thesaurus-entry, icicle-insert-thesaurus-entry-cand-fn,
;;            icicle-complete-thesaurus-entry.
;;     icicle-(previous|next)-(apropos|prefix)-candidate-action: Wrap in save-excursion.
;;     Use icicle-clear-minibuffer instead of icicle-erase-minibuffer non-interactively.
;;     icicle-erase-minibuffer: Use icicle-call-then-update-Completions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; loop
                                  ;; plus, for Emacs < 21: dolist, push
                                  ;; plus, for Emacs < 20: when, unless
(eval-when-compile (when (>= emacs-major-version 22) (require 'edmacro))) ;; edmacro-subseq
(eval-when-compile (when (>= emacs-major-version 21) (require 'recentf))) ;; recentf-mode

(eval-when-compile (require 'dabbrev))
  ;; dabbrev-case-fold-search, dabbrev-upcase-means-case-search, dabbrev--last-obarray,
  ;; dabbrev--last-completion-buffer, dabbrev--last-abbreviation, dabbrev--check-other-buffers,
  ;; dabbrev-case-replace, dabbrev--reset-global-variables, dabbrev--abbrev-at-point,
  ;; dabbrev--minibuffer-origin, dabbrev--find-all-expansions, dabbrev--substitute-expansion
(eval-when-compile (require 'bookmark))
  ;; bookmark-all-names, bookmark-buffer-name, bookmark-current-bookmark
(eval-when-compile (require 'comint))
  ;; comint-prompt-regexp, comint-input-ring, comint-mode-map, comint-check-proc,
  ;; comint-copy-old-input, comint-send-input
(eval-when-compile (require 'imenu)) ;; imenu-syntax-alist
(eval-when-compile (require 'compile)) ;; compilation-find-buffer
(eval-when-compile (require 'info)) ;; Info-goto-node

;; Commented out because `synonyms.el' soft-requires Icicles.
;; (eval-when-compile (require 'synonyms nil t)) ;; (no error if not found):
  ;; synonyms-ensure-synonyms-read-from-cache, synonyms-obarray
(eval-when-compile (require 'misc-cmds nil t)) ;; (no error if not found):
  ;; kill-buffer-and-its-windows
(require 'cus-edit)
  ;; customize-apropos, customize-apropos-faces, customize-apropos-groups,
  ;; customize-apropos-options, custom-buffer-create, custom-buffer-order-groups, customize-face,
  ;; customize-face-other-window, custom-sort-items
(require 'misc-fns nil t)   ;; (no error if not found): another-buffer
(require 'apropos-fn+var nil t) ;; (no error if not found):
  ;; apropos-command, apropos-function, apropos-option, apropos-variable
(require 'dired+ nil t) ;; (no error if not found):
                        ;; diredp-find-a-file, diredp-find-a-file-other-window
(require 'frame-cmds nil t) ;; (no error if not found): delete-windows-on
(when window-system (require 'hexrgb nil t))
                         ;; (no error if not found): hexrgb-read-color, hexrgb-color-name-to-hex
(require 'strings nil t) ;; (no error if not found): read-number

(eval-when-compile (require 'icicles-mac))
  ;; icicle-define-command, icicle-define-file-command, icicle-define-add-to-alist-command
(require 'icicles-mcmd) ;; icicle-remove-candidate-display-others,
                        ;; icicle-search-define-replacement, icicle-yank
(require 'icicles-var)
  ;; icicle-bookmark-history, icicle-buffer-config-history, icicle-candidate-action-fn,
  ;; icicle-candidate-entry-fn, icicle-candidate-nb, icicle-candidates-alist,
  ;; icicle-char-property-value-history, icicle-color-history, icicle-color-theme-history,
  ;; icicle-completion-candidates, icicle-completion-set-history, icicle-current-input,
  ;; icicle-current-raw-input, icicle-dictionary-history, icicle-extra-candidates,
  ;; icicle-font-name-history, icicle-frame-name-history, icicle-function-name-history,
  ;; icicle-incremental-completion-p, icicle-kill-history, icicle-kmacro-alist,
  ;; icicle-kmacro-history, icicle-must-match-regexp, icicle-must-not-match-regexp,
  ;; icicle-must-pass-predicate, icicle-re-no-dot, icicle-saved-completion-candidates,
  ;; icicle-search-command, icicle-search-current-overlay, icicle-search-final-choice,
  ;; icicle-search-history, icicle-search-overlays, icicle-search-refined-overlays,
  ;; icicle-variable-name-history
(require 'icicles-opt)
  ;; icicle-alternative-sort-function, icicle-buffer-configs, icicle-buffer-extras,
  ;; icicle-buffer-ignore-space-prefix-flag, icicle-buffer-match-regexp,
  ;; icicle-buffer-no-match-regexp, icicle-buffer-predicate, icicle-buffer-require-match-flag,
  ;; icicle-buffer-sort, icicle-color-themes, icicle-complete-keys-self-insert-flag, 
  ;; icicle-ignore-space-prefix-flag, icicle-incremental-completion-flag, icicle-input-string,
  ;; icicle-key-descriptions-use-<>-flag, icicle-region-alist, icicle-regions-name-length-max,
  ;; icicle-require-match-flag, icicle-saved-completion-sets, icicle-search-cleanup-flag,
  ;; icicle-search-highlight-all-current-flag, icicle-search-highlight-threshold,
  ;; icicle-search-hook, icicle-show-Completions-initially-flag, icicle-sort-function, 
  ;; icicle-transform-function, icicle-update-input-hook
(require 'icicles-fn) ;; icicle-assoc-delete-all, icicle-completing-read-history,
                      ;; icicle-get-alist-candidate, icicle-read-from-minibuf-nil-default



;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:

(when (< emacs-major-version 22)
  (defvar compilation-highlight-overlay)
  (defvar kmacro-ring)
  (defvar recentf-list)
  (defvar cookie-cache)
  (defvar yow-after-load-message)
  (defvar yow-file)
  (defvar yow-load-message))

(defvar replace-count)
(defvar icicle-kmacro-alist) ;; Defined in icicles-var.el for Emacs 22
(defvar icicle-track-pt) ;; Defined in icicle-insert-thesaurus-entry
(defvar anything-sources)
(defvar anything-candidate-cache)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Commands -----------------------------------------------


;;; Redefined standard commands.............................


;;; REPLACE ORIGINAL `dabbrev-completion' defined in `dabbrev.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-dabbrev-completion)
(fset 'old-dabbrev-completion (symbol-function 'dabbrev-completion)))

;;;###autoload
(defun icicle-dabbrev-completion (&optional arg) ; Bound to `C-M-/' globally.
  "Completion on current word.
Like \\[dabbrev-expand], but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by
`dabbrev-friend-buffer-function', to find the completions.

If the prefix argument is 16 (which comes from `C-u C-u'), then it
searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already."
  (interactive "*P")
  (unless (featurep 'dabbrev)
    (unless (require 'dabbrev nil t) (error "Library `dabbrev' not found"))
    (icicle-mode 1))                    ; Redefine `dabbrev-completion' to Icicles version.
  (dabbrev--reset-global-variables)
  (let* ((dabbrev-check-other-buffers (and arg t))
         (dabbrev-check-all-buffers (and arg (= (prefix-numeric-value arg) 16)))
         (abbrev (dabbrev--abbrev-at-point))
         (ignore-case-p (and (if (eq dabbrev-case-fold-search 'case-fold-search)
                                 case-fold-search
                               dabbrev-case-fold-search)
                             (or (not dabbrev-upcase-means-case-search)
                                 (string= abbrev (downcase abbrev)))))
         (my-obarray dabbrev--last-obarray)
         init)
    ;; If new abbreviation to expand, then expand it.
    (save-excursion
      (unless (and (null arg)
                   my-obarray
                   (or (eq dabbrev--last-completion-buffer (current-buffer))
                       (and (window-minibuffer-p (selected-window))
                            (eq dabbrev--last-completion-buffer
                                (dabbrev--minibuffer-origin))))
                   dabbrev--last-abbreviation
                   (>= (length abbrev) (length dabbrev--last-abbreviation))
                   (string= dabbrev--last-abbreviation
                            (substring abbrev 0 (length dabbrev--last-abbreviation)))
                   (setq init (try-completion abbrev my-obarray)))
        (setq dabbrev--last-abbreviation abbrev)
        (let ((completion-list (dabbrev--find-all-expansions abbrev ignore-case-p))
              (completion-ignore-case ignore-case-p))
          ;; Make an obarray with all expansions
          (setq my-obarray (make-vector (length completion-list) 0))
          (unless (> (length my-obarray) 0)
            (error "No dynamic expansion for \"%s\" found%s" abbrev
                   (if dabbrev--check-other-buffers "" " in this-buffer")))
          (dolist (string completion-list)
            (cond ((or (not ignore-case-p) (not dabbrev-case-replace))
                   (intern string my-obarray))
                  ((string= abbrev (upcase abbrev))
                   (intern (upcase string) my-obarray))
                  ((string= (substring abbrev 0 1) (upcase (substring abbrev 0 1)))
                   (intern (capitalize string) my-obarray))
                  (t (intern (downcase string) my-obarray))))
          (setq dabbrev--last-obarray my-obarray)
          (setq dabbrev--last-completion-buffer (current-buffer))
          ;; Find the longest common string.
          (setq init (try-completion abbrev my-obarray)))))
    ;; Let the user choose between the expansions
    (unless (stringp init) (setq init abbrev))
    (cond
      ((and (not (string-equal init ""))
            (not (string-equal (downcase init) (downcase abbrev)))
            (<= (length (all-completions init my-obarray)) 1))
       (message "Completed (no other completions)")
       (if (< emacs-major-version 21)
           (dabbrev--substitute-expansion nil abbrev init)
         (dabbrev--substitute-expansion nil abbrev init nil))
       (when (window-minibuffer-p (selected-window)) (message nil)))
;;$$       ;; Complete text only up through the common root. NOT USED.
;;       ((and icicle-dabbrev-stop-at-common-root-p
;;             (not (string-equal init ""))
;;             (not (string-equal (downcase init) (downcase abbrev))))
;;        (message "Use `%s' again to complete further"
;;                 (icicle-key-description (this-command-keys)
;;                                         (not icicle-key-descriptions-use-<>-flag)))
;;        (if (< emacs-major-version 21)
;;            (dabbrev--substitute-expansion nil abbrev init)
;;          (dabbrev--substitute-expansion nil abbrev init nil))
;;        (when (window-minibuffer-p (selected-window)) (message nil))) ; $$ NEEDED?
      (t
       ;; String is a common root already.  Use Icicles completion.
       (message "Making completion list...")
       (search-backward abbrev)
       (replace-match "")
       (condition-case nil
           (let* ((icicle-show-Completions-initially-flag t)
                  (icicle-incremental-completion-p 'display)
                  (minibuffer-completion-table my-obarray)
                  (choice (completing-read "Complete: " my-obarray nil nil init nil init)))
             (when choice (insert choice)))
         (quit (insert abbrev)))))))


;;; REPLACE ORIGINAL `lisp-complete-symbol' defined in `lisp.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-lisp-complete-symbol)
(fset 'old-lisp-complete-symbol (symbol-function 'lisp-complete-symbol)))

;;;###autoload
(defun icicle-lisp-complete-symbol ()   ; Bound to `ESC TAB' globally.
  "Complete the Lisp symbol preceding point against known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

The context determines which symbols are considered.
If the symbol starts just after an open-parenthesis, only symbols
with function definitions are considered.  Otherwise, all symbols with
function definitions, values or properties are considered."
  (interactive)
  (let* ((end (point))
	 (buffer-syntax (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    (set-syntax-table emacs-lisp-mode-syntax-table)
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point))
		(set-syntax-table buffer-syntax)))
	 (pattern (buffer-substring beg end))
	 (predicate
	  (if (eq (char-after (1- beg)) ?\()
	      'fboundp
	    (function (lambda (sym)
			(or (boundp sym) (fboundp sym)
			    (symbol-plist sym))))))
         (enable-recursive-minibuffers (active-minibuffer-window))
         (icicle-top-level-when-sole-completion-flag t)
         (completion (completing-read "Complete Lisp symbol: " obarray predicate t pattern nil)))
    (delete-region beg end)
    (insert completion)))


;;; REPLACE ORIGINAL `customize-face' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Multi-command version.
;;;
(or (fboundp 'old-customize-face)
(fset 'old-customize-face (symbol-function 'customize-face)))

;;;###autoload
(defun icicle-customize-face (face)
  "Customize face FACE.
Input-candidate completion and cycling are available.  While cycling,
these keys with prefix `C-' act on the current candidate:

`C-mouse-2', `C-RET' - Act on current completion candidate only
`C-down'  - Act, then move to next prefix-completion candidate
`C-up'    - Act, then move to previous prefix-completion candidate
`C-next'  - Act, then move to next apropos-completion candidate
`C-prior' - Act, then move to previous apropos-completion candidate
`C-!'     - Act on *all* candidates (customize all in the one buffer)

With prefix `C-M-' instead of `C-', the same keys (`C-M-mouse-2',
`C-M-RET', `C-M-down', and so on) provide help about the current
candidate.

Use `mouse-2', `RET', or `S-RET' to finally choose a candidate,
or `C-g' to quit.

With a prefix argument, you can enter multiple faces at the same time
with `RET' (in Emacs 22 or later).  This gives you the completion
behavior of `customize-face' in vanilla Emacs.  The advantage is that
the default value is the list of all faces under the cursor.  The
disadvantage is that face candidates are not WYSIWYG in buffer
*Completions*.

This is an Icicles command - see `icicle-mode'."
  (interactive
   (list (let ((icicle-candidate-action-fn (lambda (x)
                                             (old-customize-face (intern x))
                                             (select-frame-set-input-focus
                                              (window-frame (minibuffer-window)))))
               (icicle-all-candidates-action-fn 'icicle-customize-faces))
           (if (and (> emacs-major-version 21) current-prefix-arg)
               (read-face-name "Customize face: " "all faces" t)
             (read-face-name "Customize face: ")))))
  (old-customize-face face))


;;; REPLACE ORIGINAL `customize-face-other-window' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Multi-command version.
;;;
(or (fboundp 'old-customize-face-other-window)
(fset 'old-customize-face-other-window (symbol-function 'customize-face-other-window)))

;;;###autoload
(defun icicle-customize-face-other-window (face)
  "Customize face FACE in another window.
Same as `icicle-customize-face', but uses another window."
  (interactive
   (list (let ((icicle-candidate-action-fn (lambda (x)
                                             (old-customize-face-other-window (intern x))
                                             (select-frame-set-input-focus
                                              (window-frame (minibuffer-window)))))
               (icicle-all-candidates-action-fn 'icicle-customize-faces))
           (if (and (> emacs-major-version 21) current-prefix-arg)
               (read-face-name "Customize face: " "all faces" t)
             (read-face-name "Customize face: ")))))
  (old-customize-face-other-window face))

(defun icicle-customize-faces (faces)
  "Open Customize buffer on all faces in list FACES."
  (let ((icicle-list-nth-parts-join-string ": ")
        (icicle-list-join-string ": ")
        (icicle-list-end-string "")
        (icicle-list-use-nth-parts '(1)))
    (custom-buffer-create
     (custom-sort-items
      (mapcar (lambda (f) (list (intern (icicle-transform-multi-completion f)) 'custom-face))
              faces)
      t custom-buffer-order-groups)
     "*Customize Apropos*")))


;;; REPLACE ORIGINAL `customize-apropos' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos)
(fset 'old-customize-apropos (symbol-function 'customize-apropos)))

;;;###autoload
(defun icicle-customize-apropos (regexp &optional all)
  "Customize all user options matching REGEXP.
If ALL is `options', include only options.
If ALL is `faces', include only faces.
If ALL is `groups', include only groups.
If ALL is t (interactively, with prefix arg), include options which
  are not user-settable, as well as faces and groups.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg current-prefix-arg))
     (list (completing-read "Customize (regexp): " obarray
                            (lambda (symbol)
                              (or (get symbol 'custom-group)
                                  (custom-facep symbol)
                                  (and (boundp symbol)
                                       (or (get symbol 'saved-value)
                                           (custom-variable-p symbol)
                                           (if (null pref-arg)
                                               (user-variable-p symbol)
                                             (get symbol 'variable-documentation))))))
                            nil nil 'regexp-history)
           pref-arg)))
  (let ((found nil))
    (mapatoms (lambda (symbol)
                (when (string-match regexp (symbol-name symbol))
                  (when (and (not (memq all '(faces options))) ; groups or t
                             (get symbol 'custom-group))
                    (push (list symbol 'custom-group) found))
                  (when (and (not (memq all '(options groups))) ; faces or t
                             (custom-facep symbol))
                    (push (list symbol 'custom-face) found))
                  (when (and (not (memq all '(groups faces))) ; options or t
                             (boundp symbol)
                             (or (get symbol 'saved-value)
                                 (custom-variable-p symbol)
                                 (if (memq all '(nil options))
                                     (user-variable-p symbol)
                                   (get symbol 'variable-documentation))))
                    (push (list symbol 'custom-variable) found)))))
    (if (not found)
        (error "No matches")
      (custom-buffer-create (custom-sort-items found t custom-buffer-order-groups)
                            "*Customize Apropos*"))))

;; Define this for Emacs 20 and 21
(unless (fboundp 'custom-variable-p)
  (defun custom-variable-p (variable)
    "Return non-nil if VARIABLE is a custom variable."
    (or (get variable 'standard-value) (get variable 'custom-autoload))))


;;; REPLACE ORIGINAL `customize-apropos-faces' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos-faces)
(fset 'old-customize-apropos-faces (symbol-function 'customize-apropos-faces)))

;;;###autoload
(defun icicle-customize-apropos-faces (regexp)
  "Customize all user faces matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize faces (regexp): " obarray 'custom-facep
1f000
 nil nil
                          'regexp-history)))
  (customize-apropos regexp 'faces))


;;; REPLACE ORIGINAL `customize-apropos-groups' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos-groups)
(fset 'old-customize-apropos-groups (symbol-function 'customize-apropos-groups)))

;;;###autoload
(defun icicle-customize-apropos-groups (regexp)
  "Customize all user groups matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize groups (regexp): " obarray
                          (lambda (symbol) (get symbol 'custom-group)) nil nil 'regexp-history)))
  (customize-apropos regexp 'groups))


;;; REPLACE ORIGINAL `customize-apropos-options' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos-options)
(fset 'old-customize-apropos-options (symbol-function 'customize-apropos-options)))

;;;###autoload
(defun icicle-customize-apropos-options (regexp &optional arg)
  "Customize all user options matching REGEXP.
With prefix arg, include options which are not user-settable.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg current-prefix-arg))
     (list (completing-read "Customize options (regexp): " obarray
                            (lambda (symbol)
                              (and (boundp symbol)
                                   (or (get symbol 'saved-value)
                                       (custom-variable-p symbol)
                                       (if (null pref-arg)
                                           (user-variable-p symbol)
                                         (get symbol 'variable-documentation)))))
                            nil nil 'regexp-history)
           pref-arg)))
  (customize-apropos regexp (or arg 'options)))


;;; REPLACE ORIGINAL `repeat-complex-command' defined in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the command to repeat, letting you use `S-TAB' and
;;; `TAB' to see the history list and `C-,' to toggle sorting that display.
;;;
(or (fboundp 'old-repeat-complex-command)
(fset 'old-repeat-complex-command (symbol-function 'repeat-complex-command)))

;;;###autoload
(defun icicle-repeat-complex-command (arg) ; Bound to `C-x ESC ESC', `C-x M-:' in Icicle mode.
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history.
You can use the minibuffer history commands \\<minibuffer-local-map>\\[next-history-element] and \
\\[previous-history-element]
to get different commands to edit and resubmit.

Use `S-TAB', [next], and [prior], to match regexp input - this gives
you the functionality of `repeat-matching-complex-command'."
  (interactive "p")
  (let ((elt (nth (1- arg) command-history))        
        newcmd)
    (if elt
        (progn
          (setq newcmd
                (let ((print-level nil)
                      (minibuffer-history-position arg)
                      (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
                  (unwind-protect
                       (let ((icicle-transform-function 'icicle-remove-duplicates))
                         (read (completing-read
                                "Redo: " (mapcar (lambda (entry) (list (prin1-to-string entry)))
                                                 command-history)
                                nil nil (prin1-to-string elt) (cons 'command-history arg)
                                (prin1-to-string elt))))
                    ;; If command was added to command-history as a string, get rid of that.
                    ;; We want only evaluable expressions there.
                    (if (stringp (car command-history))
                        (setq command-history (cdr command-history))))))
          ;; If command to be redone does not match front of history, add it to the history.
          (or (equal newcmd (car command-history))
              (setq command-history (cons newcmd command-history)))
          (eval newcmd))
      (if command-history
          (error "Argument %d is beyond length of command history" arg)
        (error "There are no previous complex commands to repeat")))))

;;;###autoload
(defun icicle-add-candidate-to-saved-completion-set (set string)
  "Add candidate STRING to saved completion-candidates set SET."
  (interactive
   (list (completing-read "Saved completion set: " icicle-saved-completion-sets
                          nil t nil 'icicle-completion-set-history
                          (caar icicle-saved-completion-sets))
         (completing-read "Candidate to add: "
                          (mapcar #'list icicle-saved-completion-candidates))))
  (let ((file-name (cdr (assoc set icicle-saved-completion-sets))))
    (unless (icicle-file-readable-p file-name) (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf (find-file-noselect file-name 'nowarn 'raw))
          candidates)
      (unwind-protect (setq candidates (read list-buf)) (kill-buffer list-buf))
      (unless (consp candidates) (error "Bad data in cache file `%s'" file-name))
      (push string candidates)
      (with-temp-message (format "Writing candidate string to cache file `%s'..." file-name)
        (with-temp-file file-name (prin1 candidates (current-buffer))))
      (icicle-msg-maybe-in-minibuffer
       (format "`%s' added to saved set `%s', file `%s'" string set file-name)))))

;;;###autoload
(defun icicle-remove-candidate-from-saved-completion-set (set)
  "Remove a candidate from saved completion-candidates set SET."
  (interactive
   (list (completing-read "Saved completion set: " icicle-saved-completion-sets
                          nil t nil 'icicle-completion-set-history
                          (caar icicle-saved-completion-sets))))
  (let ((file-name (cdr (assoc set icicle-saved-completion-sets))))
    (unless (icicle-file-readable-p file-name)
      (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf (find-file-noselect file-name 'nowarn 'raw))
          string candidates)
      (unwind-protect (setq candidates (read list-buf)) (kill-buffer list-buf))
      (unless (consp candidates) (error "Bad data in cache file `%s'" file-name))
      (setq string (completing-read "Candidate to remove: " (mapcar #'list candidates)
                                    nil t nil nil (car candidates)))
      (setq candidates (delete string candidates))
      (with-temp-message (format "Writing remaining candidates to cache file `%s'..." file-name)
        (with-temp-file file-name (prin1 candidates (current-buffer))))
      (icicle-msg-maybe-in-minibuffer
       (format "`%s' removed from saved set `%s', file `%s'" string set file-name)))))

;;;###autoload
(icicle-define-command icicle-remove-saved-completion-set ; Command name
  "Remove an entry from `icicle-saved-completion-sets'.
This does not remove the associated cache file.
You can add entries to `icicle-saved-completion-sets' using command
`icicle-add/update-saved-completion-set'." ; Doc string
  (lambda (set-name)                    ; Action function
    (setq icicle-saved-completion-sets
          (icicle-assoc-delete-all set-name icicle-saved-completion-sets))
    (customize-save-variable 'icicle-saved-completion-sets icicle-saved-completion-sets)
    (message "Candidate set `%s' removed" set-name)
    ;; Update the set of completions, then update *Completions*.
    (setq minibuffer-completion-table icicle-saved-completion-sets)
    (icicle-update-completions))
  "Remove set of completion candidates named: " ; `completing-read' args
  icicle-saved-completion-sets nil t nil 'icicle-completion-set-history
  (caar icicle-saved-completion-sets))

(put 'icicle-dired-saved-file-candidates 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-dired-saved-file-candidates (prompt-for-dir-p)
  "Open Dired on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
With a prefix argument, you are prompted for the directory."
  (interactive "P")
  (unless icicle-saved-completion-candidates
    (error "No saved completion candidates.  Use \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]' to save candidates"))
  (let* ((default-directory (if prompt-for-dir-p
                                (read-file-name "Directory: " nil default-directory nil)
                              default-directory))
         (existing-dired-buffer (get-buffer (file-name-nondirectory
                                             (directory-file-name default-directory)))))
    (unless (file-exists-p (car icicle-saved-completion-candidates))
      (error "Bad directory? No file `%s' in `%s'"
             (car icicle-saved-completion-candidates) default-directory))
    (when (and existing-dired-buffer
               (y-or-n-p (format "Replace existing Dired buffer `%s'? "
                                 (buffer-name existing-dired-buffer))))
      (kill-buffer existing-dired-buffer))
    (dired (cons default-directory icicle-saved-completion-candidates))))

;;;###autoload
(defun icicle-dired-saved-file-candidates-other-window (prompt-for-dir-p)
  "Open Dired on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
Open in another window.
With a prefix argument, you are prompted for the directory."
  (interactive "P")
  (unless icicle-saved-completion-candidates
    (error "No saved completion candidates.  Use \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]' to save candidates"))
  (let* ((default-directory (if prompt-for-dir-p
                                (read-file-name "Directory: " nil default-directory nil)
                              default-directory))
         (existing-dired-buffer (get-buffer (file-name-nondirectory
                                             (directory-file-name default-directory)))))
    (unless (file-exists-p (car icicle-saved-completion-candidates))
      (error "Bad directory? No file `%s' in `%s'"
             (car icicle-saved-completion-candidates) default-directory))
    (when (and existing-dired-buffer
               (y-or-n-p (format "Replace existing Dired buffer `%s'? "
                                 (buffer-name existing-dired-buffer))))
      (if (fboundp 'kill-buffer-and-its-windows)
          (kill-buffer-and-its-windows existing-dired-buffer) ; Defined in `misc-cmds.el'
        (kill-buffer existing-dired-buffer)))
    (dired-other-window (cons default-directory icicle-saved-completion-candidates))))
 
;;(@* "Icicles multi-commands")
;;; Icicles multi-commands .   .   .   .   .   .   .   .   .

;;;###autoload
(icicle-define-command icicle-execute-extended-command ; Bound to `M-x' in Icicle mode.
  "Read command name, then read its arguments and call it.
This is `execute-extended-command', turned into a multi-command." ; Doc string
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute command%s: "         ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray 'commandp t nil 'extended-command-history nil nil
  ((last-command last-command)          ; Bindings: save and restore the last command.
   new-last-cmd)
  nil nil
  (setq this-command new-last-cmd))     ; Final code: this will update `last-command'.
  

(defun icicle-execute-extended-command-1 (cmd-name)
  "Candidate action function for `icicle-execute-extended-command'."
  (when (get-buffer orig-buff) (set-buffer orig-buff)) ; `orig-buff', `orig-window' are free vars.
  (when (window-live-p orig-window) (select-window orig-window))
  (when (string= "" cmd-name) (error "No command given"))

  ;; Rebind `icicle-candidate-action-fn' to a function that calls the
  ;; candidate CMD-NAME on a single argument that it reads.  This is
  ;; used only if CMD-NAME is a command that, itself, reads an input
  ;; argument with completion.  When that is the case, you can use
  ;; completion on that input, and if you do that, you can use `C-RET'
  ;; to use command CMD-NAME as a multi-command.  In other words, this
  ;; binding allows for two levels of multi-commands.
  (let* ((cmd (intern cmd-name))
         (icicle-candidate-action-fn
          (and icicle-candidate-action-fn ; This is nil after the command name is read.
               (lambda (arg)
                 (condition-case nil
                     (funcall cmd arg)  ; Try to use string candidate `arg'.
                   ;; If that didn't work, use a symbol or number candidate.
                   (wrong-type-argument (funcall cmd (car (read-from-string arg))))
                   (wrong-number-of-arguments ; Punt - show help.
                    (funcall #'icicle-help-on-candidate)))
                 (select-frame-set-input-focus (window-frame (minibuffer-window))))))
         (fn (symbol-function cmd))
         (count (prefix-numeric-value current-prefix-arg)))
    (cond ((arrayp fn)
           (let ((this-command cmd)) (execute-kbd-macro fn count))
           (when (> count 1) (message "(%d times)" count)))
          (t
           (run-hooks 'post-command-hook)
           (run-hooks 'pre-command-hook)
           (let ((enable-recursive-minibuffers t)
                 ;; Bind, don't set `this-command'.  When you use `C-next', `this-command' needs
                 ;; to be `cmd' during the `C-RET' part, but `last-command' must not be `cmd'
                 ;; during the `next' part.
                 (this-command cmd))
             (call-interactively cmd 'record-it))))
    ;; After `M-x' `last-command' must be the command finally entered with `RET' or, if you end
    ;; with `C-g', the last command entered with `C-RET'.
    ;; `new-last-cmd' is free here - bound in `icicle-execute-extended-command'.
    (setq new-last-cmd cmd)))

;;;###autoload
(icicle-define-command icicle-execute-named-keyboard-macro ; Bound to `C-x M-e' in Icicle mode.
  "Read the name of a keyboard macro, then execute it."
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute keyboard macro%s: "  ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray (lambda (fn) (and (commandp fn) (arrayp (symbol-function fn))))
  t nil 'icicle-kmacro-history nil nil
  ((last-command last-command)))        ; Bindings: save and restore the last command.

;;;###autoload
(when (require 'kmacro nil t)
  (icicle-define-command icicle-kmacro  ; Bound to `f5' in Icicle mode (Emacs 22).
    "Execute a keyboard macro according to its position in `kmacro-ring'.
Macros in the keyboard macro ring are given names \"macro #1\",
\"macro #2\", and so on, for use as completion candidates."
    icicle-kmacro-action                ; Function to perform the action
    (format "Execute keyboard macro%s: " ; `completing-read' args
            (if current-prefix-arg
                (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
              ""))
    (let ((count 0))
      (setq icicle-kmacro-alist
            (mapcar (lambda (x) (cons (format "macro #%d" (setq count (1+ count))) x))
                    (nreverse (cons (kmacro-ring-head) kmacro-ring)))))
    nil t nil 'icicle-kmacro-history nil nil
    nil                                 ; Additional bindings
    (unless (or (kmacro-ring-head) kmacro-ring) ; First code
      (error "No keyboard macro defined")))

  (defun icicle-kmacro-action (cand)
    "Action function for `icicle-kmacro'."
    (when (get-buffer orig-buff) (set-buffer orig-buff)) ; `orig-buff', `orig-window' are free vars
    (when (window-live-p orig-window) (select-window orig-window))
    (let ((count (prefix-numeric-value current-prefix-arg)))
      (execute-kbd-macro (cadr (assoc cand icicle-kmacro-alist)) count
                         #'kmacro-loop-setup-function)
      (when (> count 1) (message "(%d times)" count)))))

;;;###autoload
(icicle-define-command icicle-set-option-to-t ; Command name
  "Set option to t.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt)                         ; Function to perform the action
    (set (intern opt) t) (message "`%s' is now t" opt))
  "Set option to t: " obarray           ; `completing-read' args
  (cond ((and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
         (lambda (x) (and (user-variable-p x) (eq nil (symbol-value x)))))
        (current-prefix-arg (lambda (x) (and (boundp x) (eq nil (symbol-value x)))))
        (t (lambda (x) (and (icicle-binary-option-p x) (eq nil (symbol-value x))))))
  'must-confirm nil
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers t)   ; Additional bindings
   (icicle-use-candidates-only-once-flag t)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'clear-option)))
  (defalias 'clear-option 'icicle-reset-option-to-nil))
;;;###autoload
(icicle-define-command icicle-reset-option-to-nil ; Command name
  "Set option to nil.  This makes sense for binary and list options.
By default, the set of completion candidates is limited to user
options.  Note: it is *not* limited to binary and list options.
With a prefix arg, all variables are candidates." ; Doc string
  (lambda (opt) (set (intern opt) nil) (message "`%s' is now nil" opt)) ; Action function
  "Clear option (set it to nil): " obarray ; `completing-read' args
  (if current-prefix-arg
      (lambda (x) (and (boundp x) (symbol-value x)))
    (lambda (x) (and (user-variable-p x) (symbol-value x))))
  t nil (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history)
  nil nil
  ((enable-recursive-minibuffers t)     ; Additional bindings
   (icicle-use-candidates-only-once-flag t)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'toggle)))
  (defalias 'toggle 'icicle-toggle-option))
;;;###autoload
(icicle-define-command icicle-toggle-option ; Command name
  "Toggle option's value.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt)                         ; Function to perform the action
    (let ((sym (intern opt)))
      (set sym (not (eval sym))) (message "`%s' is now %s" opt (eval sym))))
  "Toggle value of option: " obarray    ; `completing-read' args
  (cond ((and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
         'user-variable-p)
        (current-prefix-arg 'boundp)
        (t 'icicle-binary-option-p))
  'must-confirm nil
  (if (boundp 'variable-name-history) 'variable-name-history 'icicle-variable-name-history) nil nil
  ((enable-recursive-minibuffers t)))   ; Additional bindings

(defun icicle-binary-option-p (symbol)
  "Non-nil if SYMBOl is a user option that has custom-type `boolean'."
  (eq (get symbol 'custom-type) 'boolean))

;;;###autoload
(icicle-define-command icicle-bookmark  ; Command name
  "Jump to a bookmark.
You can use `S-delete' on any candidate bookmark to delete it instead." ; Doc string
  bookmark-jump                         ; Function to perform the action
  "Bookmark: " (mapcar #'list (bookmark-all-names)) ; `completing-read' args
  nil t nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (or (and (boundp 'bookmark-current-bookmark) bookmark-current-bookmark) (bookmark-buffer-name))
  nil
  ((icicle-delete-candidate-object 'bookmark-delete)))

;;;###autoload
(icicle-define-command icicle-find-tag  ; Command name
  "Find tag (in current tags table) whose name matches your input.
Select the buffer containing the tag's definition, and move point
there.  The default for TAGNAME is the expression in the buffer around
or before point.

A marker representing the point where this command is invoked is
pushed onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'." ; Doc string
  find-tag                              ; Function to perform the action
  "Find tag: " 'tags-complete-tag nil nil nil nil ; `completing-read' args
  (funcall (or find-tag-default-function (get major-mode 'find-tag-default-function)
               'find-tag-default))
  nil
  ((completion-ignore-case (if (and (boundp 'tags-case-fold-search)
                                    (memq tags-case-fold-search '(t nil)))
                               tags-case-fold-search
                             case-fold-search)))
  (require 'etags))                     ; First code.

;;;###autoload
(icicle-define-command icicle-find-tag-other-window ; Command name
  "Find tag (in current tags table) whose name matches your input.
Select the buffer containing the tag's definition in another window,
and move point there.  The default for TAGNAME is the expression in
the buffer around or before point.

A marker representing the point where this command is invoked is
pushed onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'." ; Doc string
  find-tag-other-window                 ; Function to perform the action
  "Find tag other window: " 'tags-complete-tag nil nil nil nil ; `completing-read' args
  (funcall (or find-tag-default-function (get major-mode 'find-tag-default-function)
               'find-tag-default))
  nil
  ((completion-ignore-case (if (and (boundp 'tags-case-fold-search)
                                    (memq tags-case-fold-search '(t nil)))
                               tags-case-fold-search
                             case-fold-search)))
  (require 'etags))                     ; First code.

;;;###autoload
(defun icicle-other-window-or-frame (arg) ; Bound to `C-x o' in Icicle mode.
  "`other-window'/`other-frame', or, with `C-u 0', multi-command version.
With non-zero prefix argument, this is `other-window', or `other-frame'
if `one-window-p'.

With a zero prefix argument, this is `icicle-select-window', or
`icicle-select-frame' if `one-window-p'."
  (interactive "p")
  (if (zerop arg)
      (if (one-window-p) (icicle-select-frame) (icicle-select-window))
    (if (one-window-p) (other-frame arg) (other-window arg))))

;;;###autoload
(icicle-define-command icicle-select-frame ; Bound to `C-x 5 o' in Icicle mode.
  "Select frame by name and raise it."  ; Doc string
  select-frame-by-name                  ; Function to perform the action
  "Select frame: "                      ; `completing-read' args
  (make-frame-names-alist) nil t nil
  (if (boundp 'frame-name-history) 'frame-name-history 'icicle-frame-name-history)
  (cdr (assq 'name (frame-parameters (next-frame (selected-frame))))))

;;;###autoload
(icicle-define-command icicle-select-window ; Command name
  "Select window by its buffer name."   ; Doc string
  (lambda (buf) (select-window (get-buffer-window buf))) ; Function to perform the action
  "Select window: "                     ; `completing-read' args
  (let ((bufs (buffer-list (selected-frame)))
        (cand-bufs nil))
    (dolist (buf bufs) (when (get-buffer-window buf) (push (list (buffer-name buf)) cand-bufs)))
    cand-bufs)
  nil t nil 'buffer-name-history (buffer-name (window-buffer (other-window 1))))

;;;###autoload
(icicle-define-command icicle-delete-windows ; Command name
  "Delete windows showing a buffer, anywhere." ; Doc string
  delete-windows-on                     ; Function to perform the action
  "Delete windows on buffer: "          ; `completing-read' args
  (let ((all-bufs (buffer-list))
        (cand-bufs nil))
    (dolist (buf all-bufs)
      (when (get-buffer-window buf 0) (push (list (buffer-name buf)) cand-bufs)))
    cand-bufs)
  nil t nil 'buffer-name-history (buffer-name (current-buffer)) nil
  ((icicle-use-candidates-only-once-flag t)))

;;;###autoload
(defun icicle-delete-window (bufferp)   ; Bound to `C-x 0' in Icicle mode.
  "`delete-window' or prompt for buffer and delete all its windows.
When called from the minibuffer, remove the *Completions* window.

Otherwise:
 With no prefix arg, delete the selected window.
 With a prefix arg, prompt for a buffer and delete all windows, on
   any frame, that show that buffer.

 With a prefix arg, this is an Icicles multi-command - see
 `icicle-mode'.  Input-candidate completion and cycling are
 available.  While cycling, these keys act on the current
 candidate:

 `C-RET'   - Act on current completion candidate only
 `C-down'  - Act, then move to next prefix-completion candidate
 `C-up'    - Act, then move to previous prefix-completion candidate
 `C-next'  - Act, then move to next apropos-completion candidate
 `C-prior' - Act, then move to previous apropos-completion candidate
 `up'      - Move to next prefix-completion candidate
 `down'    - Move to previous prefix-completion candidate
 `next'    - Move to next apropos-completion candidate
 `prior'   - Move to previous apropos-completion candidate
 `C-!'     - Act on *all* candidates, successively (careful!)

 Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit."
  (interactive "P")
  (if (window-minibuffer-p (selected-window))
      (icicle-remove-Completions-window)
    (if bufferp (icicle-delete-windows) (delete-window))))

;;;###autoload
(icicle-define-command icicle-kill-buffer ; Bound to `C-x k' in Icicle mode.
  "Kill a buffer."                      ; Doc string
  icicle-kill-a-buffer-and-update-completions ; Function to perform the action
  "Kill buffer: "                       ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list)) nil t nil 'buffer-name-history
  (buffer-name (current-buffer)) nil
  ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate icicle-buffer-predicate)
   (icicle-extra-candidates icicle-buffer-extras)
   (icicle-sort-function icicle-buffer-sort)
   (icicle-require-match-flag icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)))

(defun icicle-kill-a-buffer-and-update-completions (buf)
  "Kill buffer BUF and update the set of completions."
  (setq buf (get-buffer buf))
  (if buf
      (condition-case err
          (if (not (buffer-live-p buf))
              (message "Buffer already deleted: `%s'" buf)
            (if (fboundp 'kill-buffer-and-its-windows)
                (kill-buffer-and-its-windows buf) ; Defined in `misc-cmds.el'.
              (kill-buffer buf))
            ;; Update the set of completions, then update *Completions*.
            (setq minibuffer-completion-table (mapcar (lambda (buf) (list (buffer-name buf)))
                                                      (buffer-list)))
            (icicle-update-completions))
        (error nil))
    (message "No such live buffer: `%s'" buf)))

(defun icicle-kill-a-buffer (buf)
  "Kill buffer BUF."
  (setq buf (get-buffer buf))
  (if buf
      (condition-case err
          (if (not (buffer-live-p buf))
              (message "Buffer already deleted: `%s'" buf)
            (if (fboundp 'kill-buffer-and-its-windows)
                (kill-buffer-and-its-windows buf) ; Defined in `misc-cmds.el'.
              (kill-buffer buf)))
        (error nil))
    (message "No such live buffer: `%s'" buf)))

(put 'icicle-buffer 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-buffer    ; Bound to `C-x b' in Icicle mode.
  "Switch to a different buffer.
You can use `S-delete' during completion to kill a candidate buffer.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

See also command `icicle-buffer-config'." ; Doc string
  switch-to-buffer                      ; Function to perform the action
  "Switch to buffer: "                  ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history (buffer-name (if (fboundp 'another-buffer)
                                                    (another-buffer nil t)
                                                  (other-buffer (current-buffer))))
  nil
  ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate icicle-buffer-predicate)
   (icicle-extra-candidates icicle-buffer-extras)
   (icicle-sort-function (or icicle-buffer-sort icicle-sort-function))
   (icicle-delete-candidate-object 'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)))

;;;###autoload
(icicle-define-command icicle-buffer-other-window ; Bound to `C-x 4 b' in Icicle mode.
  "Switch to a different buffer in another window.
You can use `S-delete' during completion to kill a candidate buffer.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

See also command `icicle-buffer-config'" ; Doc string
  switch-to-buffer-other-window         ; Function to perform the action
  "Switch to buffer: "                  ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history (buffer-name (if (fboundp 'another-buffer)
                                                    (another-buffer nil t)
                                                  (other-buffer (current-buffer))))
  nil
  ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate icicle-buffer-predicate)
   (icicle-extra-candidates icicle-buffer-extras)
   (icicle-sort-function (or icicle-buffer-sort icicle-sort-function))
   (icicle-delete-candidate-object 'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)))

;;;###autoload
(icicle-define-command icicle-add-buffer-candidate ; Command name
  "Add buffer as an always-show completion candidate.
This just adds the buffer to `icicle-buffer-extras'.
You can use `S-delete' on any candidate to remove it from
`icicle-buffer-extras' instead."        ; Doc string
  (lambda (buf)
    (add-to-list 'icicle-buffer-extras buf) ; Action function
    (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
    (message "Buffer `%s' added to always-show buffers" buf))
  "Buffer candidate to show always: "   ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history (buffer-name (if (fboundp 'another-buffer)
                                                    (another-buffer nil t)
                                                  (other-buffer (current-buffer))))
  nil
  ((icicle-delete-candidate-object 'icicle-remove-buffer-candidate-action) ; Additional bindings
   (icicle-use-candidates-only-once-flag t)))

;;;###autoload
(icicle-define-command icicle-remove-buffer-candidate ; Command name
  "Remove buffer as an always-show completion candidate.
This just removes the buffer from `icicle-buffer-extras'." ; Doc string
  icicle-remove-buffer-candidate-action ; Action function
  "Remove buffer from always-show list: " ; `completing-read' args
  (mapcar #'list icicle-buffer-extras) nil t nil 'buffer-name-history (car icicle-buffer-extras) nil
  ((icicle-use-candidates-only-once-flag t))) ;  ; Additional bindings

(defun icicle-remove-buffer-candidate-action (buf)
  "Action function for command `icicle-remove-buffer-candidate'."
  (setq icicle-buffer-extras (delete buf icicle-buffer-extras))
  (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
  (message "Buffer `%s' removed from always-show buffers" buf))

;;;###autoload
(icicle-define-command icicle-buffer-config ; Command name
  "Choose a configuration of user options for `icicle-buffer'.
You can use `S-delete' on any candidate configuration to remove it
instead.  See user option `icicle-buffer-configs'.  See also commands
`icicle-add-buffer-config' and `icicle-remove-buffer-config'." ; Doc string
  (lambda (config-name)                 ; Function to perform the action
    (let ((config (assoc config-name icicle-buffer-configs)))
      (setq icicle-buffer-match-regexp (elt config 1))
      (setq icicle-buffer-no-match-regexp (elt config 2))
      (setq icicle-buffer-predicate (elt config 3))
      (setq icicle-buffer-extras (elt config 4))
      (setq icicle-buffer-sort (elt config 5))))
  "Configuration: " icicle-buffer-configs nil t nil ; `completing-read' args
  'icicle-buffer-config-history nil nil
  ((icicle-delete-candidate-object 'icicle-remove-buffer-config-action))) ; Additional bindings

;;;###autoload
(icicle-define-add-to-alist-command icicle-add-buffer-config ; Command name
  "Add buffer configuration to `icicle-buffer-configs'.
You are prompted for the buffer configuration components.
For the list of extra buffers to always display, you can choose them
using `C-mouse-2', `C-RET', and so on, just as you would make any
Icicles multiple choice."
  (lambda ()
    (let ((name (read-from-minibuffer "Add buffer configuration.  Name: "))
          (match-regexp (icicle-read-from-minibuf-nil-default
                         "Regexp to match: " nil nil nil 'regexp-history
                         icicle-buffer-match-regexp))
          (nomatch-regexp (icicle-read-from-minibuf-nil-default
                           "Regexp not to match: " nil nil nil 'regexp-history
                           icicle-buffer-no-match-regexp))
          (pred (icicle-read-from-minibuf-nil-default
                 "Predicate to satify: " nil nil nil
                 (if (boundp 'function-name-history)
                     'function-name-history
                   'icicle-function-name-history)
                 icicle-buffer-predicate))
          (sort-fn (icicle-read-from-minibuf-nil-default
                    "Sort function: " nil nil t
                    (if (boundp 'function-name-history)
                        'function-name-history
                      'icicle-function-name-history)
                    (and icicle-buffer-sort (symbol-name icicle-buffer-sort))))
          (extras (progn (message "Choose extra buffers to show...") (sit-for 1)
                         (icicle-buffer-list)))) ; Do last, for convenience.
      (list name match-regexp nomatch-regexp pred extras sort-fn)))
  icicle-buffer-configs)

;;;###autoload
(icicle-define-command icicle-buffer-list ; Command name
  "Choose a list of buffer names.
You can use `S-delete' during completion to kill a candidate buffer.
The list of names (strings) is returned.

With a prefix argument, only buffers visiting files are candidates.
Note: The prefix arg is tested, even when this is called
noninteractively.  Lisp code can bind `current-prefix-arg' to control
the behavior." ; Doc string
  (lambda (name) (push name buf-names)) ; Function to perform the action
  "Choose buffer (`RET' when done): "   ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf)))
          (if current-prefix-arg
              (icicle-delete-if-not (lambda (bf) (buffer-file-name bf)) (buffer-list))
            (buffer-list)))
  nil nil nil 'buffer-name-history nil nil
  ((buf-names nil)                     ; Additional bindings
   (icicle-must-match-regexp icicle-buffer-match-regexp)
   (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate icicle-buffer-predicate)
   (icicle-extra-candidates icicle-buffer-extras)
   (icicle-sort-function (or icicle-buffer-sort icicle-sort-function))
   (icicle-delete-candidate-object 'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)
   (icicle-use-candidates-only-once-flag t))
  nil nil                               ; First code, undo code
  (prog1 (setq buf-names (delete "" buf-names)) ; Last code - return the list of buffers
    (when (interactive-p) (message "Buffers: %S" buf-names))))

;;;###autoload
(icicle-define-command icicle-remove-buffer-config ; Command name
  "Remove buffer configuration from `icicle-buffer-configs'." ; Doc string
  icicle-remove-buffer-config-action    ; Action function
  "Remove buffer configuration: "       ; `completing-read' args
  (mapcar (lambda (config) (list (car config))) icicle-buffer-configs)
  nil t nil 'icicle-buffer-config-history (caar icicle-buffer-configs) nil
  ((icicle-use-candidates-only-once-flag t)))

(defun icicle-remove-buffer-config-action (config-name)
  "Action function for command `icicle-remove-buffer-config'."
  (setq icicle-buffer-configs (icicle-assoc-delete-all config-name icicle-buffer-configs))
  (customize-save-variable 'icicle-buffer-configs icicle-buffer-configs)
  (message "Buffer configuration `%s' removed" config-name))

;;;###autoload
(icicle-define-command icicle-face-list ; Command name
  "Choose a list of face names.  The list of names (strings) is returned." ; Doc string
  (lambda (name) (push name face-names)) ; Function to perform the action
  "Choose face (`RET' when done): "     ; `completing-read' args
  (mapcar #'icicle-make-face-candidate (face-list))
  nil t nil (if (boundp 'face-name-history) 'face-name-history 'icicle-face-name-history)
  nil nil
  ((icicle-list-nth-parts-join-string ": ") ; Additional bindings
   (icicle-list-join-string ": ")
   (icicle-list-end-string "")
   (icicle-list-use-nth-parts '(1))
   (icicle-use-candidates-only-once-flag t)
   (face-names nil))
  nil nil                               ; First code, undo code
  (prog1 (setq face-names (delete "" face-names)) ; Last code - return the list of faces.
    (when (interactive-p) (message "Faces: %S" face-names))))

;;;###autoload
(icicle-define-command icicle-color-theme ; Command name
  "Change color theme.
You can use `S-delete' during completion to remove the current
candidate from the list of color themes.

To use this command, you must have loaded library `color-theme.el',
available from http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme." ; Doc string
  (lambda (theme) (funcall (intern theme))) ; Action - just call the theme.
  "Theme: " icicle-color-themes nil t nil ; `completing-read' args
  (if (boundp 'color-theme-history) 'color-theme-history 'icicle-color-theme-history)
  nil nil
  ((icicle-delete-candidate-object 'icicle-color-themes))) ; Additional bindings  

;;;###autoload
(icicle-define-command icicle-insert-kill ; Command name
  "Insert previously killed text from the `kill-ring'.
This is like `yank', but it does not rotate the `kill-ring'.
You can use `S-delete' during completion to delete a candidate entry
from the `kill-ring'." ; Doc string
  icicle-insert-for-yank                ; Function to perform the action
  "Insert: " (mapcar #'list kill-ring) nil t nil 'icicle-kill-history ; `completing-read' args
  (car kill-ring) nil
  ((icicle-transform-function 'icicle-remove-duplicates)
   (icicle-delete-candidate-object 'kill-ring))) ; Additional bindings

(defun icicle-insert-for-yank (string)
  "`insert-for-yank', if defined; else, `insert' with `read-only' removed."
  (if (fboundp 'insert-for-yank)        ; Defined in `subr.el' (not required).
      (insert-for-yank string)
    (let ((opoint (point)))
      (insert string)
      (let ((inhibit-read-only t)) (remove-text-properties opoint (point) '(read-only nil))))))

;;;###autoload
(defun icicle-yank-insert (&optional arg) ;  Bound to `C-y' (or whatever `yank' was bound to).
  "`icicle-insert-kill' if prefix arg < 0; else `icicle-yank-function'.
However, when called from the minibuffer, this calls `icicle-yank':
The prefix arg is then ignored and *Completions* is updated with
regexp input matches.
By default, user option `icicle-yank-function' is bound to `yank'."
  (interactive "*P")
  (if (window-minibuffer-p (selected-window))
      (icicle-yank arg)
    (if (wholenump (prefix-numeric-value arg))
        (funcall icicle-yank-function arg)
      (icicle-insert-kill))))

;;;###autoload
(icicle-define-file-command icicle-delete-file ; Command name
  "Delete a file or directory."         ; Doc string
  icicle-delete-file-or-directory       ; Function to perform the action
  "Delete file or directory: " default-directory nil t nil nil ; `read-file-name' args
  ((icicle-use-candidates-only-once-flag t))) ; Additional bindings

(defun icicle-delete-file-or-directory (file)
  "Delete file (or directory) FILE."
  (condition-case i-delete-file
      (if (eq t (car (file-attributes file)))
          (delete-directory file)
        (delete-file file))
    (error (message (error-message-string i-delete-file))
           (error (error-message-string i-delete-file)))))

;;;###autoload
(icicle-define-command icicle-file-list ; Command name
  "Choose a list of file names.
You can use `S-delete' during completion to delete a candidate file.
The list of names (strings) is returned." ; Doc string
  (lambda (name) (push name file-names)) ; Function to perform the action
  "Choose file (`RET' when done): "     ; `completing-read' args
  (mapcar #'list (directory-files default-directory nil icicle-re-no-dot))
  nil nil nil 'file-name-history nil nil
  ((file-names nil)                     ; Additional bindings
   (icicle-delete-candidate-object 'icicle-delete-file-or-directory) ; `S-delete' deletes file.
   (icicle-use-candidates-only-once-flag t))
  nil nil                               ; First code, undo code
  (prog1 (setq file-names (delete "" file-names)) ; Last code - return the list of files.
    (when (interactive-p) (message "Files: %S" file-names))))

(put 'icicle-find-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-file-command icicle-find-file ; Bound to `C-x C-f' in Icicle mode.
  "Visit a file or directory.
You can use `S-delete' during completion to delete a candidate file." ; Doc string
  icicle-find-file-w-wildcards          ; Function to perform the action
  "File or directory: " nil             ; `read-file-name' args
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file)) ; Defined in `dired+.el'.
      (condition-case nil               ; E.g. error because not on file line (ignore)
          (abbreviate-file-name (dired-get-file-for-visit))
        (error default-directory))
    default-directory)
  nil nil nil
  ((icicle-delete-candidate-object 'icicle-delete-file-or-directory))) ; Additional bindings

(defun icicle-find-file-w-wildcards (filename)
  "Find file FILENAME, where the name possibly includes shell wildcards."
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file))
      (diredp-find-a-file filename t)
    (find-file filename t)))

;;;###autoload
(icicle-define-file-command icicle-find-file-other-window ; Bound to `C-x 4 f' in Icicle mode.
  "Visit a file or directory in another window.
You can use `S-delete' during completion to delete a candidate file." ; Doc string
  icicle-find-file-other-window-w-wildcards ; Function to perform the action
  "File or directory: " nil             ; `read-file-name' args
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file-other-window)) ; In `dired+.el'
      (condition-case nil               ; E.g. error because not on file line (ignore)
          (abbreviate-file-name (dired-get-file-for-visit))
        (error default-directory))
    default-directory)
  nil nil nil
  ((icicle-delete-candidate-object 'icicle-delete-file-or-directory))) ; Additional bindings

(defun icicle-find-file-other-window-w-wildcards (filename)
  "Find file FILENAME, where the name possibly includes shell wildcards."
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file-other-window))
      (diredp-find-a-file-other-window filename t)
    (find-file-other-window filename t)))

(put 'icicle-recent-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-recent-file ; Command name
  "Open a recently used file.
You can use `S-delete' during completion to delete a candidate file."          ; Doc string
  find-file                             ; Function to perform the action
  "Recent file: " (mapcar #'list recentf-list) ; `completing-read' args
  nil t nil 'file-name-history (car recentf-list) nil
  ((icicle-delete-candidate-object 'icicle-delete-file-or-directory)) ; Additional bindings
  (progn (unless (boundp 'recentf-list) (require 'recentf)) ;  First code
         (when (fboundp 'recentf-mode) (recentf-mode 99))
         (unless (consp recentf-list) (error "No recently accessed files"))))

;;;###autoload
(icicle-define-command icicle-recent-file-other-window ; Command name
  "Open a recently used file in another window.
You can use `S-delete' during completion to delete a candidate file." ; Doc string
  find-file-other-window                ; Function to perform the action
  "Recent file: " (mapcar #'list recentf-list) ; `completing-read' args
  nil t nil 'file-name-history (car recentf-list) nil
  ((icicle-delete-candidate-object 'icicle-delete-file-or-directory)) ; Additional bindings
  (progn (unless (boundp 'recentf-list) (require 'recentf)) ;  First code
         (when (fboundp 'recentf-mode) (recentf-mode 99))
         (unless (consp recentf-list) (error "No recently accessed files"))))

(put 'icicle-locate-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-locate-file ; Command name
  "Visit a file within a directory or its subdirectories.
With a prefix argument, you are prompted for the directory.
Otherwise, the current directory is used.

The absolute names of all files within the directory and all of its
subdirectories are targets for completion.  Regexp input is matched
against all parts of the absolute name, not just the file-name part.

You can use this to find all files within your file system that match
a regexp, but be aware that gathering and matching the file names will
take some time.

Remember that you can save the set of files matching your input using \
`\\<minibuffer-local-completion-map>\\[icicle-candidate-set-save]' or \
`\\[icicle-candidate-set-save-to-cache-file]'.
You can then retrieve quickly them later using `\\[icicle-candidate-set-retrieve]' or \
`\\[icicle-candidate-set-retrieve-from-cache-file]'. " ; Doc string
  find-file                             ; Function to perform the action
  "File: " (mapcar #'list (icicle-files-within ; `completing-read' args
                           (directory-files dir 'full icicle-re-no-dot) nil))
  nil t nil 'file-name-history nil nil
  ((dir (if current-prefix-arg          ; Additional bindings
            (read-file-name "Locate under which directory: " nil default-directory nil)
          default-directory)))
  (message "Gathering files within `%s' (this could take a while)..." dir)) ;  First code

;;;###autoload
(icicle-define-command icicle-locate-file-other-window ; Command name
  "Like `icicle-locate-file' but the file is visited in another window." ; Doc string
  find-file-other-window                ; Function to perform the action
  "File: "                              ; `completing-read' args
  (mapcar #'list (icicle-files-within (directory-files dir 'full icicle-re-no-dot) nil))
  nil t nil 'file-name-history nil nil
  ((dir (if current-prefix-arg          ; Additional bindings
            (read-file-name "Locate under which directory: " nil default-directory nil)
          default-directory)))
  (message "Gathering files within `%s' (this could take a while)..." dir))

;;;###autoload
(icicle-define-command icicle-font      ; Command name
  "Change font of current frame."       ; Doc string
  (lambda (font) (modify-frame-parameters orig-frame (list (cons 'font font)))) ; Action function
  "Font: " (mapcar #'list (x-list-fonts "*")) ; `completing-read' args
  nil t nil (if (boundp 'font-name-history) 'font-name-history 'icicle-font-name-history) nil nil
  ((orig-frame (selected-frame))        ; Additional bindings
   (orig-font (frame-parameter nil 'font)))
  nil                                   ; First code
  (modify-frame-parameters orig-frame (list (cons 'font orig-font))) ; Undo code
  nil)                                  ; Last code

;;;###autoload
(icicle-define-command icicle-frame-bg  ; Command name
  "Change background of current frame." ; Doc string
  (lambda (color)                       ; Function to perform the action
    (modify-frame-parameters orig-frame (list (cons 'background-color color))))
  "Background color:: " (mapcar #'list (x-defined-colors)) ; `completing-read' args
  nil t nil (if (boundp 'color-history) 'color-history 'icicle-color-history) nil nil
  ((orig-frame (selected-frame))        ; Additional bindings
   (orig-bg (frame-parameter nil 'background-color)))
  nil                                   ; First code
  (modify-frame-parameters orig-frame (list (cons 'background-color orig-bg))) ; Undo code
  nil)                                  ; Last code

;;;###autoload
(icicle-define-command icicle-frame-fg  ; Command name
  "Change foreground of current frame." ; Doc string
  (lambda (color)                       ; Function to perform the action
    (modify-frame-parameters orig-frame (list (cons 'foreground-color color))))
  "Foreground color:: " (mapcar #'list (x-defined-colors)) ; `completing-read' args
  nil t nil (if (boundp 'color-history) 'color-history 'icicle-color-history) nil nil
  ((orig-frame (selected-frame))        ; Additional bindings
   (orig-bg (frame-parameter nil 'foreground-color)))
  nil                                   ; First code
  (modify-frame-parameters orig-frame (list (cons 'foreground-color orig-bg))) ; Undo code
  nil)                                  ; Last code

;; Bind this, not `icicle-Info-index', to `i' in Info mode,
;; so plain `Info-index' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-index-cmd ()         ; Bound to `i' in Info mode.
  "If in Icicle mode, run `icicle-Info-index'; else, run `Info-index'.
Note: In Emacs versions prior to version 22, this runs `Info-index'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-index 'Info-index)))

;;;###autoload
(defun icicle-Info-index ()
  "Like `Info-index', but you can use Icicles keys `C-RET', `C-up' etc."
  (interactive)
  (let ((info-buf (current-buffer))
        (info-window (selected-window))
        (icicle-candidate-action-fn 'icicle-Info-index-action))
    (call-interactively (if (> emacs-major-version 21) 'Info-index 'icicle-Info-index-20))))

;; Thx to Tamas Patrovics for this Emacs 20 version.
;;;###autoload
(defun icicle-Info-index-20 ()
  "Like `Info-index', but you can use completion for the index topic."
  (interactive)
  (Info-index "")
  (let ((pattern "\\* +\\([^:]*\\):.")
        (candidates nil))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t) (push (list (match-string 1)) candidates))
    (Info-index (completing-read "Index topic: " candidates nil t))))

(defun icicle-Info-index-action (topic)
  "Completion action function for `icicle-Info-index'."
  (let ((minibuf-win (selected-window)))
    (set-buffer info-buf)               ; `info-buf', `info-window' defined in `icicle-Info-index'.
    (select-window info-window)
    (Info-index topic)
    (select-window minibuf-win)))

;; Bind this, not `icicle-Info-goto-node', to `g' in Info mode,
;; so plain `Info-goto-node' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-goto-node-cmd ()     ; Bound to `g' in Info mode.
  "In Icicle mode, run `icicle-Info-goto-node'; else, `Info-goto-node'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-goto-node 'Info-goto-node)))

;;;###autoload
(defun icicle-Info-goto-node ()
  "Like `Info-goto-node', but you can use keys `C-RET', `C-up' etc."
  (interactive)
  (let ((info-buf (current-buffer))
        (info-window (selected-window))
        (icicle-candidate-action-fn 'icicle-Info-goto-node-action))
    (call-interactively 'Info-goto-node)))

(defun icicle-Info-goto-node-action (node)
  "Completion action function for `icicle-Info-goto-node'."
  (let ((minibuf-win (selected-window)))
    (set-buffer info-buf) ; `info-buf', `info-window' defined in `icicle-Info-goto-node'.
    (select-window info-window)
    (Info-goto-node node)
    (select-window minibuf-win)))

;;;###autoload
(icicle-define-command icicle-insert-thesaurus-entry ; Command name
  "Insert an entry from a thesaurus.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-insert-thesaurus-entry-cand-fn ; Function to perform the action
  "Thesaurus entry to match: " synonyms-obarray ; `completing-read' args
  nil nil nil 'icicle-dictionary-history nil nil
  ((icicle-track-pt (point)))           ; Additional bindings
  (progn                                ; First code
    (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
      (error "You must first load library `synonyms.el'"))
    (synonyms-ensure-synonyms-read-from-cache))
  (when (window-live-p orig-window)     ; Undo code
    (select-frame-set-input-focus (window-frame orig-window))
    (goto-char icicle-track-pt))       
  (when (window-live-p orig-window)     ; Last code
    (select-frame-set-input-focus (window-frame orig-window))
    (goto-char icicle-track-pt)))      

(defun icicle-insert-thesaurus-entry-cand-fn (string)
  "Action function for `icicle-insert-thesaurus-entry'.
Insert STRING, followed by a space, at position TRACK-PT of buffer
ORIG-BUFF."
  (set-buffer orig-buff)                ; `orig-buff' is a free variable here.
  (goto-char icicle-track-pt)
  (insert string " ")
  (setq icicle-track-pt (point))
  (save-excursion (set-buffer (window-buffer (minibuffer-window))) (icicle-clear-minibuffer))
  (save-selected-window (icicle-remove-Completions-window)))

;;;###autoload
(defun icicle-complete-thesaurus-entry (word) ; Bound to `C-c /' in Icicle mode.
  "Complete WORD to an entry from a thesaurus.
The default value of WORD is the word at the cursor.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'."
  (interactive (list (word-at-point)))
  (unless word (error "No word at point to complete"))
  (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
    (error "You must first load library `synonyms.el'"))
  (synonyms-ensure-synonyms-read-from-cache)
  (when (and (looking-at "\\b") (not (looking-at "\\s-"))) (forward-word 1))
  (delete-region (progn (forward-word -1) (point)) (progn (forward-word 1) (point)))
  (insert (completing-read "Thesaurus entry to match: " synonyms-obarray
                           nil nil word 'icicle-dictionary-history word))
  (unless (looking-at "\\s-") (insert " ")))

;;;###autoload
(icicle-define-command icicle-plist     ; Command name
  "Choose a symbol and its property list.
Each candidate for completion is a symbol name plus its property list
\(as a string).  They are separated by `icicle-list-join-string'
\(^G^J, by default).  You can match an input regexp against the symbol
name or the property list or both.  Use `C-q C-g C-q C-j' to input the
default separator.

Remember that you can use `[^^G]' to match any character except ^G,
which includes newline. Remember also that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion."
  (lambda (entry) (with-output-to-temp-buffer "*Help*" (princ entry))) ; Action function
  "SYMB `C-q C-g C-q C-j' PLIST (`RET' when done): " ; `completing-read' args
  (let ((result nil))                   ; TABLE arg is an alist with items ((symb plist-string))
    (mapatoms (lambda (symb)            ; Each completion candidate is a list of strings.
                (condition-case nil
                    (let ((plist (symbol-plist symb)))
                      (when plist
                        (push (list (list (symbol-name symb) (format "%s" plist))) result)))
                  (error nil))))        ; Ignore symbols that produce errors.
    result)
  nil nil nil nil nil nil
  ((icicle-candidate-properties-alist '((1 (face 'icicle-candidate-part))))) ; Bindings
  (message "Gathering property lists...")) ; First code

;;;###autoload
(icicle-define-command icicle-vardoc    ; Command name
  "Choose a variable description.
Each candidate for completion is a variable name plus its
documentation.  They are separated by `icicle-list-join-string' (^G^J,
by default).  You can match an input regexp against the variable name
or the documentation or both.  Use `C-q C-g C-q C-j' to input the
default separator.

For example, use input

\"dired.*^G
\[^^G]*list\"

with \\<minibuffer-local-completion-map>`\\[icicle-apropos-complete]' to match all variables whose
names contain \"dired\" and whose documentation contains \"list\".
Here, `[^^G]' matches any character except ^G, which includes newline.
If you use `.*' here, instead, then only the first lines of doc
strings are searched.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  (lambda (entry)                       ; Action function
    (with-output-to-temp-buffer "*Help*" (princ entry)))
  "VAR `C-q C-g C-q C-j' DOC (`RET' when done): " ; `completing-read' args
  (let ((result nil))                   ; TABLE arg is an alist whose items are ((symb doc)).
    (mapatoms (lambda (symb)            ; Each completion candidate is a list of strings.
                (condition-case nil
                    (when (boundp symb)
                      (let ((doc (documentation-property symb 'variable-documentation)))
                        (when (icicle-non-whitespace-string-p doc)
                          (push (list (list (symbol-name symb) doc)) result))))
                  (error nil))))        ; Ignore symbols that produce errors.
    result)
  nil nil nil nil nil nil
  ((icicle-candidate-properties-alist '((1 (face 'icicle-candidate-part))))) ; Bindings
  (message "Gathering variable descriptions...")) ; First code

;;;###autoload
(icicle-define-command icicle-fundoc    ; Command name
  "Choose a function description.
Each candidate for completion is a function name plus its
documentation.  They are separated by `icicle-list-join-string' (^G^J,
by default).  You can match an input regexp against the function name
or the documentation or both.  Use `C-q C-g C-q C-j' to input the
default separator.

For example, use input

\"dired.*^G
\[^^G]*file\"

with \\<minibuffer-local-completion-map>`\\[icicle-apropos-complete]' to match all functions whose
names contain \"dired\" and whose documentation contains \"file\".
Here, `[^^G]' matches any character except ^G, which includes newline.
If you use `.*' here, instead, then only the first lines of doc
strings are searched.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  (lambda (entry) (with-output-to-temp-buffer "*Help*" (princ entry))) ; Action function
  "FUNC `C-q C-g C-q C-j' DOC (`RET' when done): " ; `completing-read' args
  (let ((result nil))                   ; TABLE arg is an alist whose items are ((symb doc)).
    (mapatoms (lambda (symb)            ; Each completion candidate is a list of strings.
                (when (fboundp symb)
                  (condition-case nil
                      (let ((doc (documentation symb)))
                        (when (icicle-non-whitespace-string-p doc)
                          (push (list (list (symbol-name symb) doc)) result)))
                    (error nil)))))     ; Ignore symbols that produce errors.
    result)
  nil nil nil nil nil nil
  ((icicle-candidate-properties-alist '((1 (face 'icicle-candidate-part))))) ; Bindings
  (message "Gathering function descriptions...")) ; First code

;;;###autoload
(icicle-define-command icicle-doc       ; Command name
  "Choose documentation for a symbol.
Each candidate for completion is the description of a function,
variable, or face.  Displays the documentation and returns the
symbol.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  (lambda (entry)                       ; Action function: display the doc.
    (let ((symb (cdr (assoc entry minibuffer-completion-table))))
      (when (and symb (boundp symb)) (describe-variable symb))
      (when (fboundp symb) (describe-function symb))
      (when (facep symb) (describe-face symb))
      symb))                            ; Return the symbol.
  "Find doc with regexp: "              ; `completing-read' args
  (let ((result nil)                    ; TABLE arg is an alist whose items are (doc . symb).
        doc)                            ; Each completion candidate is a list of strings.
    (mapatoms (lambda (symb)
                (condition-case nil
                    (progn
                      (when (fboundp symb)
                        (setq doc (concat (documentation symb) "\n\n"))
                        (when (icicle-non-whitespace-string-p doc) (push (cons doc symb) result)))
                      (when (boundp symb)
                        (setq doc (concat (documentation-property symb 'variable-documentation)
                                          "\n\n"))
                        (when (icicle-non-whitespace-string-p doc) (push (cons doc symb) result))))
                  (error nil))))        ; Ignore symbols that produce errors.
    ;; `icicle-candidate-action-fn' is used in the main body of command
    ;;`icicle-doc' and is also bound to `C-RET'.  We need to refer to the
    ;; TABLE arg to `completing-read' within the body of the function.
    ;; So, we cheat and pre-assign `minibuffer-completion-table' to it here.
    (setq minibuffer-completion-table result))
  nil nil nil nil nil nil
  ((icicle-candidate-properties-alist '((1 (face 'icicle-candidate-part))))) ; Bindings
  (message "Gathering documentation...")) ; First code

;;;###autoload
(defun icicle-non-whitespace-string-p (string)
  "Return non-nil if STRING contains a non-whitespace character.
The `standard-syntax-table' definition of whitespace is used."
  (interactive "s")
  (let ((orig-syntable (syntax-table)))
    (unwind-protect
       (progn
         (set-syntax-table (standard-syntax-table))
         (and (stringp string) (> (length string) 0) (string-match "\\S-" string)))
      (set-syntax-table orig-syntable))))

;;;###autoload
(defun icicle-apropos (apropos-regexp &optional do-all)
  "Like `apropos', but lets you see the list of matches (with `S-TAB')."
  (interactive (list (completing-read "Apropos symbol (regexp or words): " obarray
                                      nil nil nil 'regexp-history)
                     current-prefix-arg))
  (apropos apropos-regexp do-all))

;;;###autoload
(cond
  ;; Use my versions of the `apropos*' commands, defined in `apropos-fn+var.el'.
  ;; Note that unlike my versions of `apropos-option' and `apropos-command', the `icicle-'
  ;; versions here do not respect `apropos-do-all': they always work with options and commands.
  ((fboundp 'apropos-option)
   (defun icicle-apropos-variable (pattern)
     "Show variables that match PATTERN.
This includes variables that are not user options.
You can see the list of matches with `S-TAB'.
See `apropos-variable' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos variable (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray
             #'(lambda (symbol) (and (boundp symbol) (get symbol 'variable-documentation)))
             nil nil 'regexp-history)))
     (apropos-variable pattern))

   (defun icicle-apropos-option (pattern)
     "Show user options (variables) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-option' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos user option (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'user-variable-p nil nil 'regexp-history)))
     (let ((apropos-do-all nil))
       (apropos-option pattern)))

   (defun icicle-apropos-function (pattern)
     "Show functions that match PATTERN.
This includes functions that are not commands.
You can see the list of matches with `S-TAB'.
See `apropos-function' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos function (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'functionp nil nil 'regexp-history)))
     (apropos-function pattern))

   (defun icicle-apropos-command (pattern)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-command' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos command (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'commandp nil nil 'regexp-history)))
     (let ((apropos-do-all nil))
       (apropos-command pattern))))

  ;; My versions are not available.  Use the vanilla Emacs versions of the `apropos...' commands.
  (t
   (defun icicle-apropos-variable (pattern &optional do-all)
     "Show variables that match PATTERN.
You can see the list of matches with `S-TAB'.

See `apropos-variable' for a description of PATTERN.

With optional prefix DO-ALL or if `apropos-do-all' is non-nil, also
show normal variables."
     (interactive
      (list (progn
              (unless (or (boundp 'apropos-do-all) (require 'apropos nil t))
                (error "Library `apropos' not found"))
              (completing-read
               (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                      "variable" "user option")
                       " (regexp" (and (>= emacs-major-version 22) " or words") "): ")
               obarray (if (or current-prefix-arg apropos-do-all)
                           #'(lambda (symbol) (and (boundp symbol)
                                                   (get symbol 'variable-documentation)))
                         'user-variable-p)
               nil nil 'regexp-history))
            current-prefix-arg))
     (apropos-variable pattern do-all))

   (defun icicle-apropos-command (pattern &optional do-all var-predicate)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.

See `apropos-command' for a description of PATTERN.

With \\[universal-argument] prefix, or if `apropos-do-all' is non-nil,
also show noninteractive functions.

If VAR-PREDICATE is non-nil, show only variables, and only those that
satisfy the predicate VAR-PREDICATE."
     (interactive
      (list (progn
              (unless (boundp 'apropos-do-all)
                (unless (require 'apropos nil t) (error "Library `apropos' not found")))
              (completing-read
               (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                      "command or function" "command")
                       "(regexp" (and (>= emacs-major-version 22) " or words") "): ")
               obarray (if current-prefix-arg 'functionp 'commandp) nil nil 'regexp-history))
            current-prefix-arg))
     (apropos-command pattern do-all var-predicate))))

;;;###autoload
(defun icicle-apropos-zippy (regexp)
  "Show all Zippy quotes matching the regular-expression input.
Return the list of matches."
  (interactive (progn (unless (boundp 'yow-file)
                        (unless (require 'yow nil t) (error "Library `yow' not found")))
                      (cookie yow-file yow-load-message yow-after-load-message)
                      (let* ((case-fold-search t)
                             (cookie-table-symbol (intern yow-file cookie-cache))
                             (string-table (symbol-value cookie-table-symbol))
                             (table (nreverse (mapcar #'list string-table))))
                        (list (completing-read "Apropos Zippy (regexp): " table
                                               nil nil nil 'regexp-history)))))
  (let ((matches (apropos-zippy icicle-current-input)))
    (when (interactive-p)
      (with-output-to-temp-buffer "*Zippy Apropos*"
        (while matches
          (princ (car matches))
          (setq matches (cdr matches))
          (and matches (princ "\n\n")))))
    matches))                           ; Return matching Zippyisms.

;;;###autoload
(defun icicle-map (alist fn)
  "Selectively apply a function to items in an alist.
FN is a function.  ALIST is an alist - interactively, it is a variable
whose value is an alist.

You are prompted for both arguments.  Completion is available, without
sorting of candidates.  The completion list for ALIST is a set of
variables whose value is a cons.  With no prefix arg, the names of
these variables must end with \"alist\".  With a prefix argument, the
first car of each variable value must itself be a cons.

Examples: If ALIST is `auto-mode-alist' and FN is `cdr', then the
completion candidates are the keys of the alist and the result of
applying FN to an alist entry is simply the value of that key.  If you
choose, for example, candidate \"\\.el\\'\", then the result is
`emacs-lisp-mode'.  In this case, the function performs simple lookup.

If, instead, FN were (lambda (x) (describe-function (cdr x))), then
the result of choosing \"\\.el\\'\" would be to display the help for
function `emacs-lisp-mode'.

During completion you can use these keys also.  Each displays the
value of applying FN to the current completion candidate.

`C-RET'   - Act on current completion candidate only
`C-down'  - Act, then move to next prefix-completion candidate
`C-up'    - Act, then move to previous prefix-completion candidate
`C-next'  - Act, then move to next apropos-completion candidate
`C-prior' - Act, then move to previous apropos-completion candidate
`up'      - Move to next prefix-completion candidate
`down'    - Move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`C-!'     - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'.

`icicle-map' overrides `icicle-ignore-space-prefix-flag', binding it
to nil so that candidates with initial spaces can be matched."
  (interactive
   (list (symbol-value
          (intern (completing-read
                   "Alist (variable): " obarray
                   `(lambda (symb)
                     (and
                      (boundp symb) (consp (symbol-value symb))
                      ,(if current-prefix-arg
                           '(consp (car (symbol-value symb)))
                           '(string-match "alist$" (symbol-name symb)))))
                   t nil (if (boundp 'variable-name-history)
                             'variable-name-history
                           'icicle-variable-name-history))))
         (read (completing-read
                "Function: " obarray 'functionp nil nil
                (if (boundp 'function-name-history)
                    'function-name-history
                  'icicle-function-name-history)))))
  (setq icicle-candidates-alist         ; Make keys of ALIST be strings.  Save in global variable.
        (mapcar (lambda (key+val) (cons (format "%s" (car key+val)) (cdr key+val))) alist))
  (setq icicle-candidate-entry-fn fn)   ; Save in global variable.
  (let ((icicle-candidate-action-fn 'icicle-map-action)
        (icicle-incremental-completion-flag 'always)
        (icicle-sort-function nil)
        (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
        (icicle-inhibit-sort-p t)
        (icicle-ignore-space-prefix-flag nil)
        (enable-recursive-minibuffers t))
    (condition-case failure
        (let* ((cand-nb 0)
               (icicle-whole-candidate-as-text-prop-p t)
               (choice (completing-read "Choose an occurrence: " icicle-candidates-alist nil t))
               candidate-entries result)
          (setq candidate-entries (icicle-filter-alist icicle-candidates-alist
                                                       icicle-completion-candidates))
          (let ((whole-cand
                 (cond (;; The display candidates have the full candidates as a text property.
                        icicle-whole-candidate-as-text-prop-p
                        (icicle-get-alist-candidate choice))

                       ;; $$$ The rest should no longer be necessary, since we bind
                       ;; `icicle-whole-candidate-as-text-prop-p' to non-nil here.

                       ((not (wholenump icicle-candidate-nb)) ; Didn't cycle to choose candidate.
                        (when (cdr candidate-entries) ; Multiple entries with the same key
                          (error (substitute-command-keys "Ambiguous choice. Try again.")))
                        ;; Do nothing if only one entry with the chosen key.
                        (elt candidate-entries cand-nb))
                       (t
                        (setq cand-nb (mod icicle-candidate-nb (length icicle-candidates-alist)))
                        ;; Since didn't cycle to choose candidate, `candidate-entries' is singleton.
                        (elt candidate-entries cand-nb)))))
            (setq result (funcall icicle-candidate-entry-fn whole-cand))
            (message "Key: %s,  Result: %s" (car whole-cand) result))
          result)
      (error (error (error-message-string failure))))))

(defun icicle-map-action (string)
  "Completion action function for `icicle-map'."
  (unwind-protect
       (condition-case icicle-map-action
           (progn
             (icicle-highlight-candidate-in-Completions)             
             ;; Apply function to candidate entry and display it.
             (let ((key+result
                    (cond
                      ;; The display candidates have the full candidates as a text property.
                      (icicle-whole-candidate-as-text-prop-p (icicle-get-alist-candidate string))
                      ;; $$$ Should no longer be necessary, since
                      ;; `icicle-whole-candidate-as-text-prop-p' is bound in `icicle-map'.
                      (icicle-candidate-nb (elt icicle-candidates-alist icicle-candidate-nb))
                      (t (error "No such occurrence")))))
               (message "Key: %s,  Result: %s" (car key+result)
                        (funcall icicle-candidate-entry-fn key+result)))               
             nil)                       ; Return nil for success.
         (error "%s" (error-message-string icicle-map-action))) ; Return error msg.
    (select-frame-set-input-focus (window-frame (minibuffer-window)))))

;;;###autoload
(defun icicle-goto-marker ()
  "Go to a marker in this buffer, choosing it by the line that includes it.
During completion you can use these keys to navigate among marks:
`C-RET'   - Act on current completion candidate only
`C-down'  - Act, then move to next prefix-completion candidate
`C-up'    - Act, then move to previous prefix-completion candidate
`C-next'  - Act, then move to next apropos-completion candidate
`C-prior' - Act, then move to previous apropos-completion candidate
`up'      - Move to next prefix-completion candidate
`down'    - Move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`C-!'     - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to choose a candidate as the final destination,
or `C-g' to quit.  This is an Icicles command - see `icicle-mode'."
  (interactive)
  (let ((markers (icicle-markers mark-ring)))
    (unless (consp markers) (error "No markers in this buffer"))
    (icicle-map (mapcar #'icicle-marker+text markers)
                (lambda (cand)
                  (pop-to-buffer (marker-buffer (cdr cand)))
                  (goto-char (cdr cand))))))

;;;###autoload
(defun icicle-goto-global-marker ()
  "Go to a global marker, choosing it by the line that includes it.
During completion you can use these keys to navigate among marks:
`C-RET'   - Act on current completion candidate only
`C-down'  - Act, then move to next prefix-completion candidate
`C-up'    - Act, then move to previous prefix-completion candidate
`C-next'  - Act, then move to next apropos-completion candidate
`C-prior' - Act, then move to previous apropos-completion candidate
`up'      - Move to next prefix-completion candidate
`down'    - Move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`C-!'     - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to choose a candidate as the final destination,
or `C-g' to quit.  This is an Icicles command - see `icicle-mode'."
  (interactive)
  (let ((markers (icicle-markers global-mark-ring)))
    (unless (consp markers) (error "No global markers"))
    (icicle-map (mapcar #'icicle-marker+text markers)
                (lambda (cand)
                  (pop-to-buffer (marker-buffer (cdr cand)))
                  (goto-char (cdr cand))))))

(defun icicle-marker+text (marker)
  "Cons of text line that includes MARKER with MARKER itself.
If the marker is on an empty line, then text \"<EMPTY LINE>\" is used."
  (save-excursion
    (set-buffer (marker-buffer marker))
    (goto-char marker)
    (let ((line (buffer-substring-no-properties (save-excursion (beginning-of-line) (point))
                                                (save-excursion (end-of-line) (point)))))
      (when (string= "" line) (setq line "<EMPTY LINE>"))
      (cons line marker))))

(defun icicle-markers (ring)
  "Marks in mark RING that are in live buffers other than a minibuffer."
  (let ((markers nil))
    (dolist (mkr ring)
      (when (and (buffer-live-p (marker-buffer mkr))
                 (not (string-match "\\` \\*Minibuf-[0-9]+\\*\\'"
                                    (buffer-name (marker-buffer mkr)))))
        (push mkr markers)))
    markers))

;;;###autoload
(defun icicle-region-open-all-files ()
  "Visit all files that contain regions in `icicle-region-alist'.
The files are visited, but not displayed.
If a file listed in `icicle-region-alist' does not exist or is not
readable, then it is ignored."
  (interactive)
  (let ((alist (icicle-delete-if-not (lambda (entry) (car (cddr entry))) icicle-region-alist)))
    (unless alist (error "There are no file buffers in `icicle-region-alist'"))
    (dolist (entry alist)
      (let ((file (car (cddr entry))))
      (when (file-readable-p file) (find-file-noselect file))))))

;;;###autoload
(defun icicle-exchange-point-and-mark (&optional arg) ; Bound to `C-x C-x'.
  "`exchange-point-and-mark', `icicle-add-region', or `icicle-select-region'.
With no prefix arg: `exchange-point-and-mark'.
With a numeric prefix arg:`icicle-add-region'.
With a plain `C-u' prefix arg: `icicle-select-region'."
  (interactive "P")
  (if arg
      (if (atom current-prefix-arg)
          (call-interactively #'icicle-add-region)
        (unless (consp icicle-region-alist)
          (error "`icicle-region-alist' is empty; try again, with a numeric prefix arg"))
        (call-interactively #'icicle-select-region))
    (call-interactively #'exchange-point-and-mark)))

;;;###autoload
(defun icicle-add-region (start end &optional tag) ; Bound to `C-N C-x C-x', N = whole number.
  "Add current region to list of regions, `icicle-region-alist'.
This saves (the limits of) the current region.
Updates the persistent value of user option `icicle-region-alist'.

With a prefix argument, you are prompted for a tag to name the region.
Otherwise, the first `icicle-regions-name-length-max' characters of
the region itself serve as the name.

To remove a region from `icicle-region-alist', use command
`icicle-remove-region' or customize `icicle-region-alist'."
  (interactive "r\nP")
  (when (= start end) (error "Cannot add region; it is empty"))
  (when (> start end) (setq end (prog1 start (setq start end))))
  (let ((region-prefix
         (buffer-substring-no-properties start (+ start (min icicle-regions-name-length-max
                                                             (- end start))))))
    (add-to-list 'icicle-region-alist
                 (list (setq tag (if tag
                                     (icicle-completing-read-history "Region name (tag): " nil nil
                                                                     nil region-prefix)
                                   region-prefix))
                       (buffer-name)
                       (buffer-file-name)
                       start
                       end))
    (customize-save-variable 'icicle-region-alist icicle-region-alist)
    (message "Region added to `icicle-region-alist' with tag `%s'"
             (if (> (length tag) 20) (concat (substring tag 0 17) "...") tag))))

;;;###autoload
(icicle-define-command icicle-select-region ; Bound to `C-u C-x C-x'
  "Choose a region from the list of Icicles regions, and activate it.
Completion is available.  The regions are sorted alphabetically by
buffer, and then by tag; you cannot change the sort order.

Regions in `icicle-region-alist' that are in buffers that do not
currently exist are ignored.

Note that each region is defined by its limits, so that if the
region's buffer has changed, then the text used to identify the region
might no longer correspond to the text at the beginning of the
region.

If user option `icicle-add-buffer-name-flag' is non-nil, then each
completion candidate is annotated with the name of the region's
buffer, to facilitate orientation.  Note that even when the value is
nil, you can use `C-M-mouse-2' and so on to see the buffer name, as
well as the start and end points of the region and its length.

Completion is lax if `icicle-add-buffer-name-flag' is non-nil;
otherwise, it is strict.

You can use `S-delete' during completion to delete a region from
`icicle-region-alist'.

You can use `icicle-add-region' to define the list of regions,
`icicle-region-alist'."                 ; Doc string
  icicle-select-region-action           ; Function to perform the action
  "Select region: " icicle-candidates-alist ; `completing-read' args
  nil (not icicle-add-buffer-name-flag) nil nil (if icicle-add-buffer-name-flag
                                                    (car (caar icicle-candidates-alist))
                                                  (caar icicle-candidates-alist))
  nil
  ((ignored (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))) ; Bindings
   (icicle-candidate-help-fn 'icicle-region-help)
   (icicle-delete-candidate-object 'icicle-delete-region-from-alist) ; `S-delete' deletes region.
   (icicle-list-nth-parts-join-string "\t")
   (icicle-list-join-string "\t")
   (icicle-list-end-string "")
   (icicle-list-use-nth-parts '(1))
   (icicle-sort-function nil)
   (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
   (icicle-inhibit-sort-p t)
   (icicle-whole-candidate-as-text-prop-p t)
   (icicle-candidates-alist
    (setq icicle-candidates-alist (icicle-region-add-buffers (icicle-region-sorted))))))

(defun icicle-select-region-action (reg-name)
  "Action function for `icicle-select-region'."
  (let* ((reg (icicle-get-alist-candidate reg-name))
         (buf (cadr reg))
         (file (car (cddr reg))))
    (when (and (not (get-buffer buf)) file) ; If no buffer, try to open the file.  If no file, msg.
      (if (file-readable-p file) (find-file-noselect file) (message "No such file: `%s'" file)))
    (when (get-buffer buf)
      (pop-to-buffer buf) (raise-frame) (goto-char (cadr (cddr reg)))
      (push-mark (car (cddr (cddr reg))) 'nomsg 'activate)))
  (setq deactivate-mark nil))

(defun icicle-region-sorted ()
  "`icicle-region-alist', sorted first by buffer name and then by tag."
  (sort (icicle-regions) (lambda (x1 x2)
                           (let ((buf1 (cadr x1))
                                 (buf2 (cadr x2)))
                             (or (string-lessp buf1 buf2)
                                 (and (string= buf1 buf2) (string-lessp (car x1) (car x2))))))))

(defun icicle-region-add-buffers (region-list)
  "Add buffer names to REGION-LIST, if `icicle-add-buffer-name-flag'."
  (if icicle-add-buffer-name-flag
      (mapcar (lambda (entry)
                (let ((buf (copy-sequence (cadr entry))))
                  (put-text-property 0 (length buf) 'face 'icicle-candidate-part buf)
                  (cons (list (car entry) buf) (cdr entry))))
              region-list)
    region-list))

;;;###autoload
(icicle-define-command icicle-remove-region ; Command name
  "Remove a region from the list of regions, `icicle-region-alist'.
Update the persistent value of user option `icicle-region-alist'.

Completion is available.  The regions are sorted alphabetically by
buffer, and then by tag; you cannot change the sort order.

To add a region to `icicle-region-alist', do one of the following:

* Use `\\[icicle-exchange-point-and-mark]' with a numeric prefix arg.
* Use command `icicle-add-region'.
* Customize `icicle-region-alist'." ; Doc string
  (lambda (cand)                        ; Function to perform the action
    (icicle-delete-region-from-alist cand)
    (if (active-minibuffer-window)
        (icicle-remove-candidate-display-others)
      (setq icicle-completion-candidates ; Finished.  Remove it without redisplaying *Completions*.
            (delete (icicle-get-alist-candidate cand) icicle-completion-candidates))))
  "Remove region from saved regions: " icicle-candidates-alist ; `completing-read' args
  nil (not icicle-add-buffer-name-flag) nil nil (if icicle-add-buffer-name-flag
                                                    (car (caar icicle-candidates-alist))
                                                  (caar icicle-candidates-alist))
  nil
  ((icicle-candidate-help-fn 'icicle-region-help) ; Additional bindings
   (icicle-list-nth-parts-join-string "\t")
   (icicle-list-join-string "\t")
   (icicle-list-end-string "")
   (icicle-list-use-nth-parts '(1))
   (icicle-sort-function nil)
   (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
   (icicle-inhibit-sort-p t)
   (icicle-whole-candidate-as-text-prop-p t)
   (icicle-candidates-alist
    (setq icicle-candidates-alist (icicle-region-add-buffers (icicle-region-sorted))))))

(defun icicle-delete-region-from-alist (reg-name)
  "Delete the region named REG-NAME from `icicle-region-alist'."
  (setq icicle-region-alist
        (delete (cons reg-name (cdr (icicle-get-alist-candidate reg-name))) icicle-region-alist))
  (customize-save-variable 'icicle-region-alist icicle-region-alist))

;;;###autoload
(icicle-define-command icicle-remove-all-regions-in-buffer ; Command name
  "Remove all regions in a buffer from `icicle-region-alist'.
To remove individual regions, use command `icicle-remove-region'.
To add a region to `icicle-region-alist', use `icicle-add-region'.
Alternatively, you can customize `icicle-region-alist'." ; Doc string
  icicle-remove-all-regions-action
                                        ; Function to perform the action
  "Buffer: "                            ; `completing-read' args
  (icicle-remove-duplicates (mapcar (lambda (reg) (list (cadr reg))) icicle-region-alist))
  nil t nil nil (buffer-name) nil
  ((icicle-use-candidates-only-once-flag t)))

(defun icicle-remove-all-regions-action (buffer)
  "Action function for `icicle-remove-all-regions-in-buffer'.
Remove all regions in BUFFER from `icicle-region-alist'.
BUFFER is the name of a buffer."
  (dolist (reg icicle-region-alist)
    (when (string= buffer (cadr reg)) (setq icicle-region-alist (delete reg icicle-region-alist))))
  (customize-save-variable 'icicle-region-alist icicle-region-alist)
  (message "Removed all regions in buffer `%s'" buffer))

;;;###autoload
(icicle-define-command icicle-search-region ; Command name
  "Search a region from the list of regions, `icicle-region-alist'.
Completion is available.  The regions are sorted alphabetically by
buffer, and then by tag; you cannot change the sort order.

You can use `S-delete' during completion to delete a region from
`icicle-region-alist'.

You can use `icicle-add-region' to define the list of regions.
Regions in `icicles-regions' that are in buffers that do not currently
exist are ignored.

Note that each region is defined by its limits, so that if the
region's buffer has changed, then the text used to identify the region
might no longer correspond to the text at the beginning of the
region.

If user option `icicle-add-buffer-name-flag' is non-nil, then each
completion candidate is annotated with the name of the region's
buffer, to facilitate orientation.  Note that even when the value is
nil, you can use `C-M-mouse-2' and so on to see the buffer name, as
well as the start and end points of the region and its length.

Completion is lax if `icicle-add-buffer-name-flag' is non-nil;
otherwise, it is strict."               ; Doc string
  icicle-search-region-action           ; Function to perform the action
  "Search region: " icicle-candidates-alist ; `completing-read' args
  nil (not icicle-add-buffer-name-flag) nil nil (if icicle-add-buffer-name-flag
                                                    (car (caar icicle-candidates-alist))
                                                  (caar icicle-candidates-alist))
  nil
  ((ignored (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))) ; Bindings
   (icicle-candidate-help-fn 'icicle-region-help)
   (icicle-delete-candidate-object 'icicle-delete-region-from-alist) ; `S-delete' deletes region.
   (enable-recursive-minibuffers t)
   (regexp (icicle-search-read-context-regexp))
   (icicle-list-nth-parts-join-string "\t")
   (icicle-list-join-string "\t")
   (icicle-list-end-string "")
   (icicle-list-use-nth-parts '(1))
   (icicle-sort-function nil)
   (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
   (icicle-inhibit-sort-p t)
   (icicle-whole-candidate-as-text-prop-p t)
   (icicle-candidates-alist
    (setq icicle-candidates-alist (icicle-region-add-buffers (icicle-region-sorted))))))

(defun icicle-search-region-action (reg-name)
  "Action function for `icicle-search-region'."
  (let* ((reg (icicle-get-alist-candidate reg-name))
         (buf (cadr reg))
         (file (car (cddr reg))))
    (when (and (not (get-buffer buf)) file (file-readable-p file)) (find-file-noselect file))
    (pop-to-buffer buf) (raise-frame)
    (let ((icicle-show-Completions-initially-flag t)
          (icicle-candidate-action-fn 'icicle-search-action)
          (icicle-candidates-alist icicle-candidates-alist))
      ;; `regexp' is free here.  It is bound in `icicle-search-region'.
      (icicle-search (cadr (cddr reg)) (car (cddr (cddr reg))) regexp t))
    (save-excursion (set-buffer (window-buffer (minibuffer-window))) (icicle-erase-minibuffer))))

(defun icicle-region-help (reg-name)
  "Use as `icicle-candidate-help-fn' for `icicle-region-alist' commands."
  (icicle-msg-maybe-in-minibuffer
   (let* ((reg (icicle-get-alist-candidate reg-name))
          (file (car (cddr reg)))
          (start (cadr (cddr reg)))
          (end (car (cddr (cddr reg)))))
     (or (concat (format "%d to %d" end start) " in buffer `" (cadr reg)
                 (and file (format "', file `%s" file)) (format "', %d chars" (- end start)))
         "No help"))))

(defun icicle-regions ()
  "Variable `icicle-region-alist', but without non-existent non-file buffers."
  (let ((unsorted-regions (icicle-delete-if (lambda (reg) (and (not (car (cddr reg))) ; No file.
                                                               (not (get-buffer (cadr reg)))))
                                            icicle-region-alist)))
    (if icicle-sort-function
        (sort unsorted-regions (lambda (a b) (funcall icicle-sort-function (car a) (car b))))
      unsorted-regions)))

;;;###autoload
(defun icicle-search-generic ()         ; Bound to `C-x `'.
  "Run `icicle-search-command'.  By default, this is `icicle-search'.
In Compilation and Grep modes, this is `icicle-compilation-search'.
In Comint, Shell, GUD, and Inferior Lisp modes, this is
   `icicle-comint-search'."
  (interactive)
  (call-interactively icicle-search-command))

;;;###autoload
(defun icicle-search (beg end scan-fn-or-regexp require-match ; Bound to `C-c `'.
                      &optional where &rest args)
  "Search for matches, with completion, cycling, and hit replacement.
Interactively, search for regexp matches.  You are prompted for a
regexp, which you enter using `RET'.  The search hits (matches) are
available as completion candidates.  You can then use apropos
completion to filter the candidates using a different regexp, which
you can change dynamically (as always).  You can replace individual
matches with another string, as in `query-replace' or
`query-replace-regexp'.  Candidates appear in order of buffer
occurrence; you cannot sort them.

Non-interactively, search can be for regexp matches or any other kind
of matches.  Argument SCAN-FN-OR-REGEXP is the regexp to match, or it
is a function that defines an alist of buffer zones to search.  You
can navigate among the matching buffer zones (defined either as regexp
matches or via function), called search \"contexts\", and you can
match another regexp against the text in a search context.  See the
end of this description for information about the other arguments.

If the search-context regexp contains regexp subgroups, that is,
subexpressions of the form `\(...\)', then you are prompted for the
subgroup to use to define the search contexts.  Subgroup 0 means the
context is whatever matches the whole regexp.  Subgroup 1 means the
context is whatever matches the first subgroup, and so on.  The
subgroup number is the number of occurrences of `\(', starting at the
beginning of the regexp.

You can further limit the set of search contexts by setting user
option `icicle-search-context-match-predicate' to a predicate that
takes a search-context (string) argument.  Only contexts that satisfy
the predicate are used.  For example, if the predicate is (lambda (x)
\(commandp (intern-soft x))), then only contexts that name commands
are kept.

Search respects `icicle-regexp-quote-flag' and
`icicle-search-whole-word-flag'.  You can toggle these during search,
by using `C-`' and `M-q', respectively.  If `icicle-regexp-quote-flag'
is non-nil, then regexp special characters are quoted, so that they
become non-special.  If `icicle-search-whole-word-flag' is non-nil,
then whole-word searching is done.  During word search, all characters
in the search string you type are treated as if they were word
constituents: the search string is matched literally, but only at word
boundaries.  (You can also use `\\[icicle-search-word]' to perform
word search.)

Optional Behaviors: Prefix Argument
-----------------------------------

By default, search only the current buffer.  Search the active region,
or, if there is none, then search the entire buffer.

With a prefix argument, you can search multiple buffers, files, or
regions, as follows:

- With a simple prefix arg (`C-u'), search all of the regions in
`icicle-region-alist'.  Those regions can be in any buffers.  If a
region is in a buffer that does not exist, it is skipped.  You can
always re-create the buffer (e.g. visit the file), and try again.

Note: To search selected regions in `icicle-region-alist'
individually, use multi-command `icicle-search-region'.

- With a non-negative numeric prefix arg, search multiple buffers
completely.  You are prompted for the buffers to search - all of each
buffer is searched.  Any existing buffers can be chosen.  If the
prefix arg is 99, then only buffers visiting files are candidates.

- With a negative numeric prefix arg, search multiple files
completely.  You are prompted for the files to search - all of each
file is searched.  Any existing files in the current directory can be
chosen.

Navigation and Help
-------------------

The use of completion for this command is special.  It is not unusual
in this context to have multiple completion candidates that are
identical - only the positions of their occurrences in the search
buffer(s) differ.  In that case, you cannot choose one simply by
completing it in the minibuffer, because the destination would be
ambiguous.  That is, simply completing your input and entering the
completion with `RET' will not take you to its occurrence in the
search buffer, unless it is unique.

Instead, choose search hits to visit using any of the candidate-action
keys: `C-RET', `C-mouse-2', `C-next', `C-prior', `C-down', and `C-up'.
The last four of these cycle among the search hits.  The current
candidate in *Completions* corresponds to the current location
visited (it is not off by one, as is usually the case in Icicles).

As always, the `C-M-' keys provide help on individual candidates:
`C-M-RET', `C-M-mouse-2', `C-M-next', `C-M-prior', `C-M-down', and
`C-M-up'.  For `icicle-search', they indicate the buffer and position
of the search hit.

You can cycle among candidates without moving to their occurrences in
the search buffer, using `next', `prior', `down', and `up' (no `C-').

Search and Replace
------------------

You can replace the current search match by using any of the
alternative action keys: `C-S-RET', `C-S-mouse-2' (in *Completions*),
`C-S-next', `C-S-prior', `C-S-down', and `C-S-up'.  You can use
`C-S-insert' to replace all matches at once.

At the first use of any of these, you are prompted for the replacement
string; it is used thereafter.  The replacement string can be anything
that is allowed as a replacement by `query-replace-regexp', including
Lisp-evaluation constructs: `\,...'.

Unlike `query-replace', you need not visit each search match - you can
visit and replace selected matches in any order.

What is meant here by a \"search match\"?  It can be either an entire
search context or whatever matches your current minibuffer input.

Key `C-,' toggles option `icicle-search-replace-whole-candidate-flag'.
By default, the entire current search context is replaced, that is,
whatever matches the context regexp that you entered initially, using
`RET'.  However, you can use `C-,' at any time during searching to
toggle between this default behavior and replacement of whatever your
current minibuffer input matches.

Remember this:

 - If `icicle-search-replace-whole-candidate-flag' is non-nil, then
   the granularity of replacement is a complete search context.  In
   this case, replacement behaves similarly to `query-replace-regexp'.
   You can still use minibuffer input to filter the set of search
   contexts, but replacement is on a whole-context basis.

 - If `icicle-search-replace-whole-candidate-flag' is nil, then you
   can replace multiple input matches separately within a search
   context.  This behavior is unique to Icicles.

If `icicle-search-replace-whole-candidate-flag' is non-nil, then you
can use the navigational alternative action keys, `C-S-next',
`C-S-prior', `C-S-down', and `C-S-up', to replace successive search
contexts.

Search traversal using these keys is always by search context, not by
input match.  This means that you cannot use these keys to replace
individual input matches within a search context, except for the first
such match.  That is, if `icicle-search-replace-whole-candidate-flag'
is nil and you use these keys, then only the first match of your input
in each search context is replaced.

If your input matches multiple parts of the search context, and you
want to replace them in order, then use `C-S-RET' repeatedly.  This
replaces successive input matches within a search context, then moves
on to the next context, and so on.  You can traverse all matches of
your input in the order they appear in the buffer by repeating
`C-S-RET'.

Repeated use of `C-S-RET' is generally for the case where you are
replacing input matches, not whole search contexts.  If you repeat
`C-S-RET' when `icicle-search-replace-whole-candidate-flag' is
non-nil, then you will, in effect, just replace the same context over
and over - unless, that is, your current input does not match the
replacement text.  In that case, the replacement is no longer a
matching search context (candidate), and `C-S-RET' moves on to the
next context.

Highlighting
------------

In the search buffer (that is, where the hits are), `icicle-search'
does the following:

- Highlights the current match (buffer zone) using face
  `icicle-search-main-regexp-current'.

- Highlights the match for your current regexp input using face
  `icicle-search-current-input'.  Interactively, this is a different
  regexp from the initial one that defines the match zones.

- Highlights the first `icicle-search-highlight-threshold' matches
  (buffer zones), using face `icicle-search-main-regexp-others'.

If user option `icicle-search-cleanup-flag' is non-nil (the default),
then all search highlighting is removed from the search buffer when
you are finished searching.  If it is nil, then you can remove this
highlighting later using command `icicle-search-highlight-cleanup'.
You can toggle `icicle-search-cleanup-flag' at any time using `C-.'
in the minibuffer.

Using Regexps
-------------

At any time, you can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' (command
`icicle-insert-string-from-variable') to insert text (e.g. a regexp)
from a variable into the minibuffer.  For example, you can search for
ends of sentences by using `C-u \\[icicle-insert-string-from-variable]' and choosing variable
`sentence-end' as the variable.  And you can use
`\\[icicle-save-string-to-variable]' to save a string to a variable
for later use by `\\[icicle-insert-string-from-variable]'.

When employed with useful regexps, `C-=' can turn `icicle-search' into
a general navigator or browser of code, mail messages, and many other
types of buffer.  Imenu regexps work fine, for example - command
`icicle-imenu' simply uses `icicle-search' this way.  See
`icicle-insert-string-from-variable' for more tips on inserting
regexps from variables.

Additional Information
----------------------

If user option `icicle-add-buffer-name-flag' is non-nil, then each
candidate is annotated with the name of the buffer where the hit
occurs, to facilitate orientation.  Note that even when the value is
nil, you can use `C-M-mouse-2' and so on to see the buffer name, as
well as the position of the hit in the buffer.

Completion is lax if `icicle-add-buffer-name-flag' is non-nil;
otherwise, it is strict.

After you visit a completion candidate, the hooks in variable
`icicle-search-hook' are run.

`icicle-search' overrides `icicle-ignore-space-prefix-flag', binding
it to nil, so that candidates with initial spaces can be matched.

`icicle-search' sets `icicle-search-final-choice' to the final user
choice, which might not be one of the search candidates if
REQUIRE-MATCH is nil.

Non-Interactive Use
-------------------

When called non-interactively, these are the `icicle-search'
arguments:

BEG is the beginning of the region to search; END is the end.
SCAN-FN-OR-REGEXP: Regexp or function that determines the set of
  initial candidates (match zones).  If a function, it is passed, as
  arguments, the buffer to search, the beginning and end of the search
  region in that buffer, and ARGS.
REQUIRE-MATCH is passed to `completing-read'.
Optional arg WHERE is either a list of buffers or a list of region
  entries that have the same form as `icicle-region-alist'.  If nil,
  then only the current buffer is used.
ARGS are arguments that are passed to function SCAN-FN-OR-REGEXP.

Note that if SCAN-FN-OR-REGEXP is a regexp string, then function
`icicle-search-regexp-scan' is used to determine the set of match
zones.  You can limit hits to regexp matches that also satisfy a
predicate, by using `(PREDICATE)' as ARGS: PREDICATE is then passed to
`icicle-search-regexp-scan' as its PREDICATE argument.

This command is intended for use only in Icicle mode."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (let ((icicle-incremental-completion-flag 'always)
        (icicle-sort-function nil)
        (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
        (icicle-candidate-alternative-action-fn (or icicle-candidate-alternative-action-fn
                                                    'icicle-search-replace-search-hit))
        (icicle-must-pass-predicate icicle-search-context-match-predicate)
        (save-action-first-p icicle-act-before-cycle-flag)
        (replace-count 0)
        (icicle-inhibit-sort-p t)
        (icicle-searching-p t)
        (icicle-ignore-space-prefix-flag nil)
        (icicle-list-nth-parts-join-string "\t")
        (icicle-list-join-string "\t")
        (icicle-list-end-string "")
        (icicle-list-use-nth-parts '(1))
        (icicle-expand-input-to-common-match-flag nil)
        (icicle-whole-candidate-as-text-prop-p t)
        (completion-ignore-case case-fold-search)
        (orig-point (point))
        (orig-window (selected-window))
        (mark-active nil))              ; So region highlighting doesn't hide highlighting here.
    (setq icicle-candidates-alist nil    icicle-search-replacement nil)
    (when (and icicle-regexp-quote-flag
               (not icicle-search-whole-word-flag)
               (stringp scan-fn-or-regexp))
      (setq scan-fn-or-regexp (regexp-quote scan-fn-or-regexp)))
    (setq icicle-search-context-regexp (and (stringp scan-fn-or-regexp) scan-fn-or-regexp))
    ;; Build `icicle-candidates-alist'. Highlight up to `icicle-search-highlight-threshold' matches
    (message "Finding matches...")
    (cond ((and (consp where) (bufferp (car where))) ; List of buffers - search buffers.
           (dolist (buf where)
             (icicle-search-define-candidates buf nil nil scan-fn-or-regexp args)))
          ((and (consp where)           ; List of files - search files.
                (stringp (car where))
                (file-exists-p (car where)))
           (dolist (file where)
             (icicle-search-define-candidates (find-file-noselect file) nil nil
                                              scan-fn-or-regexp args)))
          ((consp where)                ; Search all regions in `icicle-region-alist'.
           (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))
           (let ((non-existent-buffers ()))
             (dolist (reg where)
               (if (bufferp (get-buffer (cadr reg)))
                   (icicle-search-define-candidates (get-buffer (cadr reg)) (cadr (cddr reg))
                                                    (car (cddr (cddr reg))) scan-fn-or-regexp args)
                 (push (cadr reg) non-existent-buffers)))
             (when non-existent-buffers
               (message "Skipping regions in non-existent buffers: `%s'"
                        (mapconcat 'identity (icicle-remove-duplicates non-existent-buffers)
                                   "', `"))
               (sit-for 3))))
          (t                            ; Search this buffer only.
           (icicle-search-define-candidates nil beg end scan-fn-or-regexp args)))
    (unless icicle-candidates-alist
      (error (concat "No match" (and (stringp scan-fn-or-regexp)
                                     (format " for regexp `%s'" scan-fn-or-regexp))
                     (and (stringp scan-fn-or-regexp) args (format " (with predicate)")))))
    (let ((icicle-candidate-action-fn (or icicle-candidate-action-fn 'icicle-search-action))
          (icicle-candidate-help-fn 'icicle-search-help)
          (icicle-update-input-hook (list 'icicle-search-highlight-all-input-matches)))
      (setq icicle-search-final-choice nil)
      (unwind-protect
           (condition-case failure
               (let (candidate-entries cand-nb marker buf)
                 (setq icicle-act-before-cycle-flag nil
                       icicle-search-final-choice (completing-read
                                                   "Choose an occurrence: "
                                                   icicle-candidates-alist nil require-match
                                                   nil 'icicle-search-history)
                       icicle-completion-candidates (if require-match
                                                        (list icicle-search-final-choice)
                                                      icicle-completion-candidates)
                       candidate-entries (icicle-filter-alist icicle-candidates-alist
                                                              icicle-completion-candidates))
                 (cond
                   ;; No match required, and not a match - just run the hook.
                   ((and (not require-match) (null icicle-completion-candidates))
                    (run-hooks 'icicle-search-hook))
                   ;; The display candidates have the full candidates as a text property.
                   (icicle-whole-candidate-as-text-prop-p
                    (setq marker (cdr (icicle-get-alist-candidate icicle-search-final-choice)))
                    (unless marker (error "No such occurrence"))
                    (setq buf (marker-buffer marker))
                    (unless (bufferp buf) (error "No such buffer: %s" buf))
                    (pop-to-buffer buf)
                    (raise-frame)
                    (goto-char (marker-position marker))
                    (select-frame-set-input-focus (selected-frame))
                    (run-hooks 'icicle-search-hook))
                   
                   ;; $$$ The rest should no longer be necessary, since we bind
                   ;; `icicle-whole-candidate-as-text-prop-p' to non-nil here.
                   
                   ;; Didn't cycle - completed.
                   ((not (wholenump icicle-candidate-nb))
                    (if (cdr candidate-entries)
                        (error "Ambiguous choice. Try again, using `C-next' to browse.")
                      (goto-char (cdr (car candidate-entries))) ; Go to sole completion.
                      (run-hooks 'icicle-search-hook)))
                   ;; Cycled.
                   (t
                    (if require-match
                        (setq marker (cdar candidate-entries))
                      (setq cand-nb (mod icicle-candidate-nb (length icicle-candidates-alist)))
                      (setq marker (cdr (elt candidate-entries cand-nb))))
                    (unless marker (error "No such occurrence"))
                    (setq buf (marker-buffer marker))
                    (unless (bufferp buf) (error "No such buffer: %s" buf))
                    (pop-to-buffer buf)
                    (raise-frame)
                    (goto-char (marker-position marker))
                    (select-frame-set-input-focus (selected-frame))
                    (run-hooks 'icicle-search-hook))))
             (quit (goto-char orig-point))
             (error (goto-char orig-point)
                    (error "%s" (error-message-string failure))))
        (setq icicle-act-before-cycle-flag save-action-first-p)
        (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
        (when (window-live-p orig-window)
          (select-window orig-window) (select-frame-set-input-focus (selected-frame))))
      icicle-search-final-choice)))

;; This is the same as `region-or-buffer-limits' in `misc-fns.el'.
(defun icicle-region-or-buffer-limits ()
    "Return the start and end of the region as a list, smallest first.
If the region is not active or empty, then bob and eob are used."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

(defun icicle-search-read-context-regexp (&optional prompt pred init hist def i-i-m)
  "Read context regexp and determine `icicle-search-context-level'.
The regexp is read with completion against previous regexp input.
The arguments are for use by `completing-read' to read the regexp.
 HIST (or `regexp-history' if HIST is nil) is used for the
 `completing-read' TABLE argument.  The REQUIRE-MATCH arg is nil.
 A default prompt is used if PROMPT is nil."
  (setq hist     (or hist 'regexp-history)
        prompt   (or prompt "Find (regexp): "))
  (let ((regexp (icicle-completing-read-history prompt 'regexp-history pred init def i-i-m)))
    (while (string= "" regexp)
      (message "Regexp cannot be empty.  Try again...") (sit-for 2)
      (setq regexp (icicle-completing-read-history prompt 'regexp-history pred init def i-i-m)))
    (setq prompt                      "Subgroup to use as search context [0, 1, 2,...]: "
          icicle-search-context-level (if (string-match "\\\\(" regexp)
                                          (truncate (if (fboundp 'read-number)
                                                        (read-number prompt 0)
                                                      (read-from-minibuffer ; Hope for a number.
                                                       prompt nil nil nil nil 0)))
                                        0))
    regexp))

(defun icicle-search-where-arg ()
  "Return WHERE arg for `icicle-search*' commands, based on prefix arg."
  (cond ((consp current-prefix-arg)
         (message "Searching saved regions") (sit-for 1) icicle-region-alist)
        ((wholenump current-prefix-arg)
         (icicle-search-choose-buffers (= 99 (prefix-numeric-value current-prefix-arg))))
        (current-prefix-arg
         (message "Searching multiple files") (sit-for 1)
         (let ((icicle-show-Completions-initially-flag t)) (icicle-file-list)))
        (t nil)))

(defun icicle-search-choose-buffers (files-only-p)
  "Choose multiple buffers to search.
FILES-ONLY-P non-nil means that only buffers visiting files are
candidates."
  (message "Searching multiple buffers") (sit-for 1)
  (let ((icicle-show-Completions-initially-flag t))
    (mapcar #'get-buffer (let ((icicle-buffer-require-match-flag 'partial-match-ok)
                               (current-prefix-arg files-only-p))
                           (icicle-buffer-list)))))

(defun icicle-search-read-word ()
  "Read a word to search for (whole-word search).
Regexp special characters within the word are escaped (quoted)."
  (setq icicle-search-context-level 0)
  (concat "\\b"
          (regexp-quote (icicle-completing-read-history "Search for whole word: "
                                                        'icicle-search-history))
          "\\b"))

(defun icicle-search-define-candidates (buffer beg end scan-fn-or-regexp args)
  "Define completion candidates for `icicle-search'.
BUFFER is a buffer to scan for candidates.
BEG, END, and SCAN-FN-OR-REGEXP are the same as for `icicle-search'.
ARGS are other arguments that are passed to SCAN-FN-OR-REGEXP."
  (if (functionp scan-fn-or-regexp)
      (apply scan-fn-or-regexp buffer beg end args)
    (apply 'icicle-search-regexp-scan buffer beg end scan-fn-or-regexp args)))

(defun icicle-search-regexp-scan (buffer beg end regexp &optional predicate)
  "Scan BUFFER for REGEXP, pushing hits onto `icicle-candidates-alist'.
PREDICATE is nil or a boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context
If PREDICATE is non-nil, then push only the hits for which it holds.
If REGEXP has subgroups, then what the Nth subgroup matches is used as
the search context (hit), where N = `icicle-search-context-level'.  If
N=0, then the overall match of REGEXP is used as the search context.

If BUFFER is nil, scan the current buffer.
Highlight the matches in face `icicle-search-main-regexp-others'.
If BEG and END are non-nil, scan only between positions BEG and END."
  (setq regexp (or regexp (icicle-search-read-context-regexp)))
  (let ((add-bufname-p (and buffer icicle-add-buffer-name-flag))
        (temp-list ())
        (last-beg nil))
    (unless buffer (setq buffer (current-buffer)))
    (when (bufferp buffer)
      (set-buffer buffer)
      (unless (and beg end) (setq beg (point-min) end (point-max)))
      (condition-case icicle-search-regexp-scan
          (save-excursion
            (goto-char (setq last-beg beg))
            (while (and beg (< beg end) (not (eobp)))
              (while (and (setq beg (re-search-forward regexp end t))
                          (eq last-beg beg)
                          (not (eobp)))
                (forward-char) (setq beg (1+ beg))) ; Matched again, same place.  Advance 1 char.
              (when (and beg (not (eobp)))
                (unless (match-beginning icicle-search-context-level)
                  (error "Search context has no subgroup of level %d - try a lower number"
                         icicle-search-context-level))
                (let* ((hit-string (buffer-substring-no-properties
                                    (match-beginning icicle-search-context-level)
                                    (match-end icicle-search-context-level)))
                       (end-marker (copy-marker (match-end icicle-search-context-level))))
                  (when (and (not (string= "" hit-string))
                             (or (not predicate)
                                 (save-match-data (funcall predicate hit-string end-marker))))
                    ;; Add whole candidate to `temp-list'.  Whole candidate is
                    ;; (`hit-string' . `end-marker') or ((`hit-string' BUFNAME) . `end-marker').
                    (push (cons (if add-bufname-p
                                    (list hit-string
                                          (let ((string (copy-sequence (buffer-name))))
                                            (put-text-property 0 (length string) 'face
                                                               'icicle-candidate-part string)
                                            string))
                                  hit-string)
                                end-marker)
                          temp-list)
                    ;; Highlight search context in buffer.
                    (when (<= (+ (length temp-list) (length icicle-candidates-alist))
                              icicle-search-highlight-threshold)
                      (let ((ov (make-overlay (match-beginning icicle-search-context-level)
                                              (match-end icicle-search-context-level))))
                        (push ov icicle-search-overlays)
                        (overlay-put ov 'priority 200) ; > ediff's 100+, < isearch-overlay's 1001.
                        (overlay-put ov 'face 'icicle-search-main-regexp-others))))))
              (setq last-beg beg))
            (setq icicle-candidates-alist (append icicle-candidates-alist (nreverse temp-list))))
        (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
        (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
               (error (error-message-string icicle-search-regexp-scan)))))))

(defun icicle-search-highlight-all-input-matches (&optional input)
  "Highlight, inside each search context, what the current input matches."
  (save-excursion
    ;; Update by deleting (if it exists) and then creating.
    ;; If a single overlay exists, it means that the user just changed
    ;; `icicle-search-highlight-threshold' to non-zero.
    ;; Otherwise, it's nil or a list of overlays.
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays nil))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays (cdr icicle-search-refined-overlays))))
  (when icicle-search-highlight-all-current-flag
    (setq input (or input icicle-current-input))
    (unless (or (string= "" input) (null icicle-search-overlays))
      (save-excursion
        (dolist (ov icicle-search-overlays)
          (set-buffer (overlay-buffer ov))
          (save-restriction             ; Search within the current search context.
            (narrow-to-region (overlay-start ov) (overlay-end ov))
            (goto-char (point-min))
            (save-match-data
              (while (condition-case nil (re-search-forward input nil 'move-to-end) (error nil))
                (setq ov (make-overlay (match-beginning 0) (match-end 0)))
                (push ov icicle-search-refined-overlays)
                (overlay-put ov 'priority 204)
                (overlay-put ov 'face 'icicle-search-current-input)))))))))

(defun icicle-search-replace-search-hit (candidate)
  "Replace search hit CANDIDATE with `icicle-search-replacement'."
  (let ((cand-nb (or icicle-candidate-nb 0)) ; Replace-all has nil, so use 0.
        (cands icicle-completion-candidates)
        (completion-cmd icicle-last-completion-command)
        (last-input icicle-last-input)
        (compl-win (get-buffer-window "*Completions*" 0)))
    (unless icicle-search-replacement
      (icicle-search-define-replacement)
      (when (and compl-win cands)
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list icicle-completion-candidates))))
    (setq icicle-completion-candidates   cands
          icicle-candidate-nb            cand-nb
          icicle-last-completion-command completion-cmd
          icicle-last-input              last-input))
  (funcall icicle-candidate-action-fn candidate icicle-search-replacement))

(defun icicle-search-action (string &optional replace-string)
  "Completion action function for command `icicle-search'.
1. Highlight the current candidate in *Completions*.
2. Move to the regexp match in the original buffer and highlight it.
3. If `icicle-search-highlight-threshold' is zero, highlight what the
   current input matches, inside the match of the initial regexp.
4. If REPLACE-STRING is non-nil, replace current candidate with it.
   If `icicle-search-replace-whole-candidate-flag' is nil, replace
   only the candidate part that matches the cu
17959
rrent input.

   Note: The replacement can be anything allowed as a replacement by
   `query-replace-regexp', including Lisp-evaluation
   constructs (`\,...')."
  (prog1
      (condition-case icicle-search-action
          (progn
            (icicle-highlight-candidate-in-Completions)
            ;; Move cursor to the match in the original buffer and highlight it.
            (let* ((cand+mrker (elt (icicle-filter-alist icicle-candidates-alist
                                                         icicle-completion-candidates)
                                    icicle-candidate-nb))
                   (candidate (if (consp (car-safe cand+mrker))
                                  (car-safe (car-safe cand+mrker))
                                (car-safe cand+mrker)))
                   (marker (cdr-safe cand+mrker))
                   (icicle-search-in-context-fn
                    (or icicle-search-in-context-fn 'icicle-search-in-context-default-fn)))
              (unless marker (error "No such occurrence"))
              (save-selected-window
                (when (window-live-p orig-window) (select-window orig-window))
                (let ((completion-ignore-case case-fold-search)
                      (buf (marker-buffer marker))
                      (icicle-candidate-nb icicle-candidate-nb)) ; Save and restore this.
                  (unless (bufferp buf) (error "No such buffer: %s" buf))
                  (pop-to-buffer buf)
                  (raise-frame)
                  (goto-char marker)
                  ;; Highlight current search context using `icicle-search-main-regexp-current'.
                  (icicle-place-overlay (- marker (length candidate)) marker
                                        'icicle-search-current-overlay
                                        'icicle-search-main-regexp-current
                                        202 buf)
                  (funcall icicle-search-in-context-fn cand+mrker replace-string)
                  (run-hooks 'icicle-search-hook)))
              nil))                     ; Return nil for success.
        (error (message (error-message-string icicle-search-action))
               (error-message-string icicle-search-action))) ; Return error message.
    (select-frame-set-input-focus (window-frame (minibuffer-window)))))

(defun icicle-search-in-context-default-fn (cand+mrker replace-string)
  "Default value of `icicle-search-in-context-fn'."
  (let ((candidate (if (consp (car-safe cand+mrker))
                       (car-safe (car-safe cand+mrker))
                     (car-safe cand+mrker)))
        (marker (cdr-safe cand+mrker)))
    (save-excursion (save-restriction    ; Search within the current search context.
                      (narrow-to-region (- marker (length candidate)) marker)
                      (icicle-search-highlight-and-maybe-replace cand+mrker replace-string)))))

(defun icicle-search-highlight-and-maybe-replace (cand+mrker replace-string)
  "Highlight within search context and replace using REPLACE-STRING.
If REPLACE-STRING is nil, no replacement occurs.
Arguments are the same as for `icicle-search-in-context-fn'."
  (icicle-search-highlight-context-levels)
  (icicle-search-highlight-input-matches-here)
  (when replace-string
    (goto-char (point-min))
    (cond (icicle-search-replace-whole-candidate-flag
           (let ((candidate (if (consp (car-safe cand+mrker))
                                (car-safe (car-safe cand+mrker))
                              (car-safe cand+mrker))))
             (cond ((string= candidate replace-string) ; Sanity check only.
                    (save-restriction (widen) (message
                                               "Replacement = candidate, and \
current input matches candidate") (sit-for 2)))
                   (t
                    (set-match-data (list (point-min) (point-max)))
                    (icicle-search-replace-match replace-string
                                                 (icicle-search-replace-fixed-case-p
                                                  icicle-search-context-regexp))))))
          (t
           (save-match-data
             (let ((first-p t))
               (while (and (re-search-forward icicle-current-input nil 'move-to-end)
                           (or first-p icicle-all-candidates-action-p))
                 (setq first-p nil)
                 (icicle-search-replace-match replace-string
                                              (icicle-search-replace-fixed-case-p
                                               icicle-current-input)))))))
    (icicle-search-replace-candidate cand+mrker (buffer-substring (point-min) (point-max)))
    (save-selected-window (select-window (minibuffer-window)) (icicle-retrieve-last-input))
    (icicle-update-completions)
    (save-window-excursion (icicle-display-candidates-in-Completions))
    (when (>= icicle-candidate-nb (length icicle-completion-candidates))
      (setq icicle-candidate-nb 0))
    (setq icicle-last-completion-candidate (buffer-substring (point-min)
                                                             (point-max)))
    (icicle-highlight-candidate-in-Completions)
    (icicle-search-highlight-context-levels)
    (icicle-search-highlight-input-matches-here)))

(defun icicle-search-replace-match (replace-string fixedcase)
  "Replace current match with REPLACE-STRING, interpreting escapes.
Treat REPLACE-STRING as it would be by `query-replace-regexp'.
FIXEDCASE is as for `replace-match'.  Non-nil means do not alter case."
  (if (fboundp 'query-replace-compile-replacement) ; Emacs 22.
      (let ((compiled
             (save-match-data
               (query-replace-compile-replacement replace-string
                                                  (not icicle-search-replace-literally-flag)))))
        (condition-case icicle-search-replace-match1
            (let ((enable-recursive-minibuffers t) ; So we can read input from \?.
                  ;; Save and restore these, because we might read input from \?.
                  (icicle-last-completion-command icicle-last-completion-command)
                  (icicle-last-input icicle-last-input))
              (replace-match-maybe-edit
               (if (consp compiled)
                   (funcall (car compiled) (cdr compiled) (setq replace-count (1+ replace-count)))
                 compiled)
               fixedcase icicle-search-replace-literally-flag nil (match-data)))
          (error (icicle-remove-Completions-window) (error "No match for %s" replace-string))))
    (condition-case icicle-search-replace-match2 ; Emacs < 22.  Try to interpret `\'.
        (replace-match replace-string fixedcase icicle-search-replace-literally-flag)
      (error (replace-match replace-string fixedcase t))))) ;   If error, replace literally.

(defun icicle-search-highlight-context-levels ()
  "Highlight context levels differently (up to 8 levels).
No such highlighting is done if any of these conditions holds:
 * `icicle-search-context-level' is not 0 (search context < regexp).
 * `icicle-search-highlight-context-levels-flag' is nil.
 * `icicle-search-context-regexp' is nil (non-regexp searching)."
  (unless (or (/= icicle-search-context-level 0)
              (not icicle-search-highlight-context-levels-flag)
              (not icicle-search-context-regexp)) ; E.g. text-property searching
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays (cdr icicle-search-level-overlays)))
    (save-match-data
      (let ((level 1)
            (max-levels (min (regexp-opt-depth icicle-search-context-regexp) 8)))
        (goto-char (point-min))
        (re-search-forward icicle-search-context-regexp nil t)
        (condition-case nil
            (while (<= level max-levels)
              (let ((ov (make-overlay (match-beginning level) (match-end level))))
                (push ov icicle-search-level-overlays)
                (overlay-put ov 'priority 205) ; > ediff's 100+, < isearch-overlay's 1001.
                (overlay-put ov 'face (intern (concat "icicle-search-context-level-"
                                                      (number-to-string level)))))
              (setq level (1+ level)))
          (error nil))))))

(defun icicle-search-highlight-input-matches-here ()
  "Highlight all input matches in the current search context."
  (unless (or (> 0 icicle-search-highlight-threshold) (string= "" icicle-current-input))
    (goto-char (point-min))
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays nil))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays (cdr icicle-search-refined-overlays)))
    (let ((ov nil))
      (save-match-data
        (while (and (not (eobp)) (re-search-forward icicle-current-input nil 'move-to-end))
          (setq ov (make-overlay (match-beginning 0) (match-end 0)))
          (push ov icicle-search-refined-overlays)
          (overlay-put ov 'priority 204)
          (overlay-put ov 'face 'icicle-search-current-input))))))

(defun icicle-search-replace-fixed-case-p (from)
  "Return non-nil if FROM should be replaced without transferring case.
FROM is a string or nil.  If FROM is nil, then return nil."
  (and from (not (and case-fold-search case-replace (string= from (downcase from))))))

(defun icicle-search-replace-candidate (cand+mrker new-cand)
  "In `icicle-candidates-alist', replace car of CAND+MRKER with NEW-CAND."
  (let ((newlist icicle-candidates-alist))
    (while newlist
      (when (equal (car newlist) cand+mrker) (setcar newlist (cons new-cand (cdr-safe cand+mrker))))
      (setq newlist (cdr newlist)))
  icicle-candidates-alist))

(defun icicle-search-help (cand)
  "Use as `icicle-candidate-help-fn' for `icicle-search' commands."
  (icicle-msg-maybe-in-minibuffer
   (let ((marker (cdr (icicle-get-alist-candidate cand))))
     (or (concat "Buffer: `" (buffer-name (marker-buffer marker))
                 (format "', Position: %d" (marker-position marker)))
         "No help"))))

;;;###autoload
(defun icicle-search-keywords (beg end keywords require-match ; Bound to `C-c `'.
                               &optional where &rest args)
  "Search with one or more keywords, which can each be a regexp.
Text that matches any of the keywords is found.  At the prompt, use
apropos completion (`S-TAB') to choose one or more regexps that you
have entered previously (`C-RET', `C-mouse-2', `C-next', and so on, or
use `C-RET' to enter one or more new regexps.  End with `RET'.

Keywords are interpreted as regexps.  You can change to substring
completion instead, matching regexp special characters literally, by
using `C-`' during completion to toggle `icicle-regexp-quote-flag'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation."
  (interactive
   `(,@(icicle-region-or-buffer-limits)
     ,(icicle-group-regexp (mapconcat #'icicle-group-regexp (icicle-keyword-list) "\\|"))
     ,(not icicle-add-buffer-name-flag)
     ,(icicle-search-where-arg)))
  (icicle-search beg end keywords (not icicle-add-buffer-name-flag) where))

(defalias 'icicle-regexp-list 'icicle-keyword-list)
;;;###autoload
(icicle-define-command icicle-keyword-list ; Command name
  "Choose a list of keywords. The list of keywords (strings) is returned.
You can choose from keywords entered previously or enter new keywords
using `C-RET'.  Each keyword can be a regexp.  The regexps are OR'd,
and the resulting regexp is used for `icicle-search'." ; Doc string
  (lambda (name) (push name keywords))  ; Function to perform the action
  "Choose keyword (regexp) (`RET' when done): " ; `completing-read' args
  (mapcar #'list (icicle-remove-duplicates regexp-history)) nil nil nil 'regexp-history nil nil
  ((keywords nil)                       ; Additional bindings
   (icicle-use-candidates-only-once-flag t))
  nil nil                               ; First code, undo code
  (prog1 (setq keywords (nreverse (delete "" keywords))) ; Last code - return the list of keywords.
    (when (interactive-p) (message "Keywords (regexps): %S" keywords))))

(defun icicle-group-regexp (regexp)
  "Wrap REGEXP between regexp parens, as a regexp group."
  (concat "\\(" regexp "\\)"))

;;;###autoload
(defun icicle-search-char-property (beg end require-match ; Bound to `C-c "'.
                                    &optional where prop values predicate)
  "Search for text that has a character property with a certain value.
If the property is `face' or `font-lock-face', then you can pick
multiple faces, using completion.  Text is then searched that has a
face property that includes any of the selected faces.  If you choose
no face (empty input), then text with any face is found.

By \"character property\" is meant either an overlay property or a
text property.  If you want to search for only an overlay property or
only a text property, then use `icicle-search-overlay-property' or
`icicle-search-text-property' instead.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation.

Non-interactively, arguments BEG, END, REQUIRE-MATCH, and WHERE are as
for `icicle-search'.  Arguments PROP, VALUES, and PREDICATE are passed
to `icicle-search-char-property-scan' to define the search contexts."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values nil
                 predicate))

;;;###autoload
(defun icicle-search-overlay-property (beg end require-match &optional where prop values predicate)
  "Same as `icicle-search-char-property', except only overlay property.
That is, do not also search a text property."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values 'overlay
                 predicate))

;;;###autoload
(defun icicle-search-text-property (beg end require-match ; Bound to `C-c "'.
                                    &optional where prop values predicate)
  "Same as `icicle-search-char-property', except only text property.
That is, do not also search an overlay property."
  (interactive (icicle-search-property-args))
  (icicle-search beg end 'icicle-search-char-property-scan require-match where prop values 'text
                 predicate))

(defun icicle-search-property-args ()
  "Read and return interactive arguments for `icicle-search-*-property'."
  (let* ((where (icicle-search-where-arg))
         (beg+end (icicle-region-or-buffer-limits))
         (beg1 (car beg+end))
         (end1 (cadr beg+end))
         (props (mapcar (lambda (prop) (list (symbol-name prop)))
                        (icicle-char-properties-in-buffers where beg1 end1)))
         (prop (intern (completing-read "Property to search: " props nil nil nil nil "face")))
         (values (if (memq prop '(face font-lock-face))
                     (let ((faces (icicle-face-list)))
                       (if faces (mapcar #'intern faces) (face-list))) ; Default: all faces.
                   (list (intern (icicle-completing-read-history
                                  "Property value: " 'icicle-char-property-value-history))))))
    `(,beg1 ,end1 ,(not icicle-add-buffer-name-flag) ,where ,prop ,values)))

(defun icicle-char-properties-in-buffers (where beg end &optional type)
  "List of all character properties in WHERE.
Only the character properties are included, not their values.
WHERE is a list of buffers, a list of files, or a list of region
  entries that have the same form as `icicle-region-alist'.  If nil,
  then only the current buffer is used.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
  (cond ((and (consp where) (bufferp (car where))) ; List of buffers - search buffers.
         (dolist (buf where) (icicle-char-properties-in-buffer buf nil nil type)))
        ((and (consp where)             ; List of files - search files.
              (stringp (car where))
              (file-exists-p (car where)))
         (dolist (file where)
           (icicle-char-properties-in-buffer (find-file-noselect file) nil nil type)))
        ((consp where)                  ; Search all regions in `icicle-region-alist'.
         (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))
         (dolist (reg where)
           (when (bufferp (get-buffer (cadr reg)))
             (icicle-char-properties-in-buffer
              (get-buffer (cadr reg)) (cadr (cddr reg)) (car (cddr (cddr reg))) type))))
        (t                              ; Search this buffer only.
         (icicle-char-properties-in-buffer (current-buffer) beg end type))))

(defun icicle-char-properties-in-buffer (&optional buffer beg end type)
  "List of all character properties in BUFFER between BEG and END.
Only the character properties are included, not their values.
TYPE can be `overlay', `text', or nil, meaning overlay properties,
text properties, or both, respectively."
  (let ((props nil)
        ovrlays curr-props)
    (save-excursion
      (unless buffer (setq buffer (current-buffer)))
      (set-buffer buffer)
      (unless (and beg end) (setq beg (point-min) end (point-max)))
      (when (or (not type) (eq type 'overlay)) ; Get overlay properties.
        (setq ovrlays (overlays-in beg end))
        (dolist (ovrly ovrlays)
          (setq curr-props (overlay-properties ovrly))
          (while curr-props
            (unless (memq (car curr-props) props) (push (car curr-props) props))
            (setq curr-props (cddr curr-props)))))
      (when (or (not type) (eq type 'text)) ; Get text properties.
        (while (< beg end)
          (setq beg        (or (next-property-change beg nil end) end)
                curr-props (text-properties-at beg))
          (while curr-props
            (unless (memq (car curr-props) props) (push (car curr-props) props))
            (setq curr-props (cddr curr-props))))))
    props))

(defun icicle-search-char-property-scan (buffer beg end prop values type predicate)
  "Scan BUFFER for character property PROP with values VALUES.
Push hits onto `icicle-candidates-alist'.
If BUFFER is nil, scan the current buffer.
Highlight the matches in face `icicle-search-main-regexp-others'.
If BEG and END are non-nil, scan only between positions BEG and END.

Find text with a PROP value that overlaps with VALUES.  That is, if
the value of PROP is an atom, then it must be a member of VALUES; if
it is a list, then at least one list element must be a member of
VALUES.

If PREDICATE is non-nil, then push only the hits for which it holds.
PREDICATE is nil or a Boolean function that takes these arguments:
  - the search-context string
  - a marker at the end of the search-context"
  (let ((add-bufname-p (and buffer icicle-add-buffer-name-flag))
        (temp-list ())
        (zone-end nil))
    (unless buffer (setq buffer (current-buffer)))
    (set-buffer buffer)
    (unless (and beg end) (setq beg (point-min) end (point-max)))
    (condition-case icicle-search-char-property-scan
        (save-excursion
          (while (and (< beg end)
                      (let* ((charval (and (or (not type) (eq type 'overlay))
                                           (get-char-property beg prop)))
                             (textval (and (or (not type) (eq type 'text))
                                           (get-text-property beg prop)))
                             (currval (icicle-flat-list charval textval)))
                        (not (icicle-set-intersection values currval))))
            (setq beg (icicle-next-single-char-property-change beg prop nil end)))
          (while (and beg (< beg end))
            (setq zone-end (or (icicle-next-single-char-property-change beg prop nil end) end))
            (let* ((hit-string (buffer-substring-no-properties beg zone-end))
                   (end-marker (copy-marker zone-end)))
              (when (or (not predicate) (save-match-data (funcall predicate hit-string end-marker)))
                (push (cons (if add-bufname-p
                                (list hit-string
                                      (let ((string (copy-sequence (buffer-name))))
                                        (put-text-property 0 (length string)
                                                           'face 'icicle-candidate-part string)
                                        string))
                              hit-string)
                            end-marker)
                      temp-list)
                ;; Highlight search context in buffer.
                (when (<= (+ (length temp-list) (length icicle-candidates-alist))
                          icicle-search-highlight-threshold)
                  (let ((ov (make-overlay beg zone-end)))
                    (push ov icicle-search-overlays)
                    (overlay-put ov 'priority 200) ; > ediff's 100+, but < isearch overlays
                    (overlay-put ov 'face 'icicle-search-main-regexp-others)))))
            (setq beg zone-end)
            (while (and (< beg end)
                        (let* ((charval (and (or (not type) (eq type 'overlay))
                                             (get-char-property beg prop)))
                               (textval (and (or (not type) (eq type 'text))
                                             (get-text-property beg prop)))
                               (currval (icicle-flat-list charval textval)))
                          (not (icicle-set-intersection values currval))))
              (setq beg (icicle-next-single-char-property-change beg prop nil end))))
          (setq icicle-candidates-alist (append icicle-candidates-alist (nreverse temp-list))))
      (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
      (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
             (error (error-message-string icicle-search-char-property-scan))))))

(defun icicle-flat-list (val1 val2)
  "Return a flat list with all values in VAL1 and VAL2."
  (let ((result nil))
    (unless (listp val1) (setq val1 (list val1)))
    (unless (listp val2) (setq val2 (list val2)))
    (while val1 (add-to-list 'result (pop val1)))
    (while val2 (add-to-list 'result (pop val2)))
    result))

(if (fboundp 'next-single-char-property-change)
    (defalias 'icicle-next-single-char-property-change 'next-single-char-property-change)
  (defun icicle-next-single-char-property-change (position prop &optional object limit)
    "Position of next change of PROP for text property or overlay change.
Scans characters forward from buffer position POSITION until property
PROP changes.  Returns the position of that change.

POSITION is a buffer position (integer or marker).

Optional third arg OBJECT is ignored.  It is present for compatibility
 with Emacs 22.

If optional fourth arg LIMIT is non-nil, search stops at position
LIMIT.  LIMIT is returned if nothing is found before LIMIT.

The property values are compared with `eq'.  If the property is
constant all the way to the end of the buffer, then the last valid
buffer position is returned."
    (save-excursion
      (goto-char position)
      (let ((propval (get-char-property (point) prop))
            (end (min limit (point-max))))
        (while (and (< (point) end) (eq (get-char-property (point) prop) propval))
          (goto-char (min (next-overlay-change (point))
                          (next-single-property-change (point) prop nil end)))))
      (point))))

;;;###autoload
(defun icicle-search-highlight-cleanup ()
  "Remove all highlighting from the last use of `icicle-search'."
  (interactive)
  (let ((inhibit-quit t))
    (message "Removing search highlighting...")
    (while icicle-search-overlays
      (delete-overlay (car icicle-search-overlays))
      (setq icicle-search-overlays (cdr icicle-search-overlays)))
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays (cdr icicle-search-level-overlays)))
    (when (overlayp icicle-search-current-overlay)
      (delete-overlay icicle-search-current-overlay))
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays nil))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays (cdr icicle-search-refined-overlays)))
    (message "Removing search highlighting...done")))

;;;###autoload
(defun icicle-search-word (beg end word-regexp require-match ; Bound to `C-c $'.
                           &optional where &rest args)
  "Search for a whole word.
Word search is literal: regexp special characters are treated as
non-special.  In fact, they are also treated as if they were
word-constituent characters.  That is, your typed input is searched
for literally, but matches must begin and end on a word boundary.
This also means that you can include whitespace within the \"word\"
being sought.

At the prompt for a word, you can use completion against previous
Icicles search inputs to choose the word, or you can enter a new word.

Non-interactively, WORD-REGEXP should be a regexp that matches a word.
The other arguments are the same as for `icicle-search'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(icicle-search-read-word)
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (icicle-search beg end word-regexp (not icicle-add-buffer-name-flag) where))

;;;###autoload
(defun icicle-search-all-regions (scan-fn-or-regexp require-match &rest args)
  "Search all saved regions.
This is the same as using a simple prefix arg, `C-u', with
`icicle-search'.  Args are the same as for `icicle-search', but
without args BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-add-buffer-name-flag)))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match icicle-region-alist args))

;;;###autoload
(defun icicle-search-buffer (scan-fn-or-regexp require-match &rest args)
  "Search multiple buffers completely.
This is the same as using a non-negative numeric prefix arg, such as
`C-9', with `icicle-search'.  You are prompted for the buffers to
search.  All of each buffer is searched.  Any existing buffers can be
chosen.  Args are the same as for `icicle-search', but without args
BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-add-buffer-name-flag)))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match
         (let ((icicle-show-Completions-initially-flag t))
           (mapcar #'get-buffer (let ((icicle-buffer-require-match-flag 'partial-match-ok))
                                  (icicle-buffer-list))))
         args))

;;;###autoload
(defun icicle-search-file (scan-fn-or-regexp require-match &rest args)
  "Search multiple files completely.
This is the same as using a negative numeric prefix arg, such as
`C--', with `icicle-search'.  You are prompted for the files to search.
All of each file is searched.  Any existing files in the current
directory can be chosen.  Args are the same as for `icicle-search',
but without args BEG, END, and WHERE."
  (interactive `(,(if icicle-search-whole-word-flag
                      (icicle-search-read-word)
                      (icicle-search-read-context-regexp))
                 ,(not icicle-add-buffer-name-flag)))
  (apply #'icicle-search nil nil scan-fn-or-regexp require-match
         (let ((icicle-show-Completions-initially-flag t)) (icicle-file-list))
         args))

;;;###autoload
(defun icicle-occur (beg end &optional buffers) ; Bound to `C-c ''.
  "`icicle-search' with a regexp of \".*\".  An `occur' with icompletion.
Type a regexp to match within each line of the buffer.  Use `S-TAB' to
show matching lines.  Use `C-RET' or `C-mouse-2' to go to the line of
the current candidate.  Use `C-next', `C-prior', `C-down', or`C-up' to
cycle among the matching lines.

By default, search only the current buffer.  Search the active region,
or, if none, the entire buffer.  With a prefix argument, you are
prompted for the buffers to search.  You can choose buffers using
completion (`C-RET' and so on).  If the prefix argument is 99, then
only buffers visiting files are candidates.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the `icicle-search'
documentation."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(and current-prefix-arg
                       (icicle-search-choose-buffers (= 99 (prefix-numeric-value
                                                            current-prefix-arg))))))
  (let ((fg (face-foreground 'icicle-search-main-regexp-others))
        (bg (face-background 'icicle-search-main-regexp-others))
        (icicle-transform-function (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn (set-face-foreground 'icicle-search-main-regexp-others nil)
                (set-face-background 'icicle-search-main-regexp-others nil)
                (icicle-search beg end ".*" (not icicle-add-buffer-name-flag) buffers))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload
(defun icicle-comint-search (beg end)   ; Bound to `C-x `' in `comint-mode'.
  "Use `icicle-search' to pick up a previous input for reuse.
Use this in a `comint-mode' buffer, such as *shell* or
*inferior-lisp*.  This searches your interactive history in the buffer
for a match to your current input, which you can change dynamically.
When you choose a previous input, it is copied to the current prompt,
for reuse.  If the region is active, then only it is searched;
otherwise, the entire buffer is searched.

Use `C-RET' or `C-mouse-2' to choose a previous input for reuse.  Use
`C-next', `C-prior', `C-down', or `C-up' to cycle among your previous
inputs.

As for other Icicles search commands, your current input narrows the
set of possible candidates.  See `icicle-search' for more
information.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

Note that previous commands are identified by looking through the
shell buffer for a shell prompt.  This is not foolproof.  If, for
instance you use command `ls', the output includes an auto-save file
such as #foo.el#, and `#' in the first column represents a shell
prompt, then #foo.el# will be misinterpreted as a previous command.

Also, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

Being a search command, `icicle-comint-search' cannot give you access
to previous shell commands that are not visible in the current buffer.
See also \\<comint-mode-map>\\[icicle-comint-command] for another way to reuse commands,
including those from previous sessions.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the `icicle-search'
documentation."
  (interactive (icicle-region-or-buffer-limits))
  ;; Is there a better test we can use, to make sure the current mode inherits from `comint-mode'?
  (unless (where-is-internal 'comint-send-input (keymap-parent (current-local-map)))
    (error "Current mode must be derived from comint mode"))
  (let ((orig-search-hook icicle-search-hook)
        (icicle-transform-function 'icicle-remove-duplicates))
    (add-hook 'icicle-search-hook 'icicle-comint-send-input)
    (icicle-search beg end (concat comint-prompt-regexp "\\S-.*") nil) ; Match not required (edit).
    (remove-hook 'icicle-search-hook 'icicle-comint-send-input))
  (goto-char (point-max)))

(defun icicle-comint-send-input ()
  "Grab current completion input and use that for comint input."
  (unless (comint-check-proc (current-buffer))
    (error "No live process associated with this buffer"))
  (let ((comint-get-old-input
         (if (minibuffer-window-active-p (minibuffer-window))
             'icicle-comint-get-minibuffer-input ; Use minibuffer input (e.g. for action fn).
           'icicle-comint-get-final-choice))) ; Use final choice.
    (comint-copy-old-input))
  (comint-send-input))

(defun icicle-comint-get-minibuffer-input ()
  "Return the minibuffer input, beyond the prompt."
  (let* ((cand (icicle-minibuffer-contents))
         (input-start (and (string-match comint-prompt-regexp cand) (match-end 0))))
    (if input-start (substring cand input-start) cand)))

(defun icicle-comint-get-final-choice ()
  "Return the final choice, beyond the prompt."
  (let ((input-start (and (string-match comint-prompt-regexp icicle-search-final-choice)
                          (match-end 0))))
    (if input-start
        (substring icicle-search-final-choice input-start)
      icicle-search-final-choice)))

;;;###autoload
(icicle-define-command icicle-comint-command ; Bound to `C-c TAB' in `comint-mode'.
  "Retrieve a previously used command.
Use this in a `comint-mode' buffer such as *shell* or *inferior-lisp*.

Note, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

See also \\<comint-mode-map>\\[icicle-comint-search] for another way to reuse commands."
  insert
  "Choose a previous command: "         ; `completing-read' args
  (mapcar #'list (cddr comint-input-ring)) nil nil nil 'shell-command-history
  (aref (cddr comint-input-ring) 0) nil
  ((icicle-transform-function 'icicle-remove-duplicates))) ; Additional bindings

(defun icicle-comint-hook-fn ()
  "Hook giving comint mode Icicles command bindings."
  (set (make-local-variable 'icicle-search-command) 'icicle-comint-search)
  (define-key comint-mode-map "\C-c\C-i" 'icicle-comint-command)
  (define-key comint-mode-map [(control ?c) tab] 'icicle-comint-command))

;;;###autoload
(defun icicle-compilation-search (beg end) ; Bound to `C-c `' in `compilation(-minor)-mode'.
  "Like `icicle-search', but shows the matching compilation-buffer
hit.  Use this in a compilation buffer, such as `*grep*', searching
for a regexp as with `icicle-search'.  Use `C-RET' or `C-mouse-2' to
show the target-buffer hit corresponding to the current completion
candidate.  Use `C-next', `C-prior', `C-down', or `C-up' to cycle
among the target-buffer hits.

As for `icicle-search', you can further narrow the match candidates by
typing a second regexp to search for among the first matches.  See
`icicle-search' for more information.

Altogether, using this with `grep' gives you two or three levels of
regexp searching: 1) the `grep' regexp, 2) the major `icicle-search'
regexp, and optionally 3) the refining `icicle-search' regexp.

In Emacs 22 and later, you can replace search-hit text, as in
`icicle-search'.  In earlier Emacs versions, you cannot replace text.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, see the `icicle-search'
documentation."
  (interactive (icicle-region-or-buffer-limits))
  (unless (condition-case nil (eq (current-buffer) (compilation-find-buffer)) (error nil))
    (error "Current buffer must be a compilation buffer"))
  (save-excursion (goto-char (point-min))
                  (compilation-next-error 1)
                  (setq beg (if beg (max beg (point)) (point))))
  (let ((icicle-transform-function (if (interactive-p) nil icicle-transform-function))
        (icicle-candidate-alternative-action-fn
         (if (boundp 'compilation-highlight-overlay)
             icicle-candidate-alternative-action-fn
           (lambda (cand)
             (message "Cannot replace matching text in Emacs before version 22"))))
        (next-error-highlight 1000000) ; $$$ Bind to t, for indefinite, after Emacs bug is fixed.
        (icicle-search-in-context-fn 'icicle-compilation-search-in-context-fn)
        (fg (face-foreground 'icicle-search-main-regexp-others))
        (bg (face-background 'icicle-search-main-regexp-others)))
    (unwind-protect
         (progn
           (set-face-foreground 'icicle-search-main-regexp-others nil)
           (set-face-background 'icicle-search-main-regexp-others nil)
           (icicle-search beg end ".*" t))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

(defun icicle-compilation-hook-fn ()
  "Hook setting `icicle-search-command' for compilation modes."
  (set (make-local-variable 'icicle-search-command) 'icicle-compilation-search))

(defun icicle-compilation-search-in-context-fn (cand+mrker replace-string)
  "`icicle-search-in-context-fn' used for `icicle-compilation-search'."
  (let ((candidate (if (consp (car-safe cand+mrker))
                       (car-safe (car-safe cand+mrker))
                     (car-safe cand+mrker)))
        (marker (cdr-safe cand+mrker)))
    (compile-goto-error)
    (let ((ov (and (boundp 'compilation-highlight-overlay) compilation-highlight-overlay)))
      (if (overlayp ov)
          (with-current-buffer (overlay-buffer ov)
            (save-excursion
              (save-restriction ; Search within the current search context.
                (narrow-to-region (overlay-start ov) (overlay-end ov))
                (icicle-search-highlight-and-maybe-replace cand+mrker replace-string))))
        (save-excursion
          (save-restriction ; Search within the current search context.
            (narrow-to-region (- marker (length candidate)) marker)
            (icicle-search-highlight-and-maybe-replace cand+mrker replace-string)))))))

;;;###autoload
(defun icicle-imenu (beg end require-match &optional where) ; Bound to `C-c ='.
  "Go to an Imenu entry using `icicle-search'.
Recommended: Use library `imenu+.el' also.
In Emacs-Lisp mode, `imenu+.el' classifies definitions using these
submenus:

 1. Keys         - keys in the global keymap
 2. Keys in Maps - keys in keymaps other than global keymap
 3. Functions    - functions, whether interactive or not
 4. Macros       - macros defined with `defmacro'
 5. User Options - user variables, from `defcustom'
 6. Variables    - other variables (non-options), from `defvar'
 7. Faces        - faces, from `defface'
 8. Other        - other definitions

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation.

Note: If you use this command with a prefix argument, then the Imenu
mode (and `imenu-generic-expression') of the current buffer determines
what kinds of definitions are found.  So, if you want to search for
definitions in a certain language, then invoke this command from a
buffer in that language."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (unless imenu-generic-expression (error "No Imenu for this buffer"))
  (let ((case-fold-search (if (or (local-variable-p 'imenu-case-fold-search)
				  (not (local-variable-p 'font-lock-defaults)))
			      imenu-case-fold-search
			    (nth 2 font-lock-defaults)))
        (old-table (syntax-table))
        (table (copy-syntax-table (syntax-table)))
        (slist imenu-syntax-alist))
    (dolist (syn slist)                 ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
	  (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc (lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus (icicle-delete-if-not
                          #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                          (mapcar (lambda (menu) ; Name an unlabeled menu `Others'.
                                    (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                  imenu-generic-expression)))
                  (submenu (let ((icicle-show-Completions-initially-flag t))
                             (completing-read "Choose: " menus nil t)))
                  (regexp (cadr (assoc submenu menus)))
                  (icicle-transform-function (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No match"))
             (icicle-search beg end regexp require-match where)))
      (set-syntax-table old-table))))

(defun icicle-imenu-in-buffer-p (menu)
  "Return non-nil if the regexp in MENU has a match in the buffer."
  (save-excursion (goto-char (point-min)) (re-search-forward (cadr menu) nil t)))

(defun icicle-imenu-command (beg end require-match &optional where)
  "Go to an Emacs command definition using `icicle-search'.
This uses `commandp', so it finds only currently defined commands.
That is, if the buffer has not been evaluated, then its function
definitions are not considered commands by `icicle-imenu-command'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode."))
  (let ((case-fold-search (if (or (local-variable-p 'imenu-case-fold-search)
				  (not (local-variable-p 'font-lock-defaults)))
			      imenu-case-fold-search
			    (nth 2 font-lock-defaults)))
        (old-table (syntax-table))
        (table (copy-syntax-table (syntax-table)))
        (slist imenu-syntax-alist))
    (dolist (syn slist)                 ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
	  (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc (lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus (icicle-delete-if-not
                          #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                          (mapcar (lambda (menu) ; Name an unlabeled menu `Others'.
                                    (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                  (if (boundp 'emacs-lisp-imenu-generic-expression)
                                      emacs-lisp-imenu-generic-expression
                                    lisp-imenu-generic-expression))))
                  (submenu (or (assoc "Functions" menus) (assoc "Others" menus)
                               (error "No command definitions in buffer")))
                  (regexp (cadr (assoc (car submenu) menus)))
                  (icicle-transform-function (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No command definitions in buffer"))
             (icicle-search beg end regexp require-match where 'icicle-imenu-command-p)))
      (set-syntax-table old-table))))

(defun icicle-imenu-non-interactive-function (beg end require-match &optional where)
  "Go to an Emacs non-interactive function definition with `icicle-search'.
This uses `commandp' to distinguish currently defined commands from
other functions.  This means that if the buffer has not yet been
evaluated, then all of its function definitions are considered
non-interactive by `icicle-imenu-non-interactive-function'.

This command is intended only for use in Icicle mode.  It is defined
using `icicle-search'.  For more information, in particular for the
use of a prefix argument to search multiple regions, buffers, or
files, see the `icicle-search' documentation."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(not icicle-add-buffer-name-flag)
                 ,(icicle-search-where-arg)))
  (unless (or where (eq major-mode 'emacs-lisp-mode))
    (error "This command is only for Emacs-Lisp mode."))
  (let ((case-fold-search (if (or (local-variable-p 'imenu-case-fold-search)
				  (not (local-variable-p 'font-lock-defaults)))
			      imenu-case-fold-search
			    (nth 2 font-lock-defaults)))
        (old-table (syntax-table))
        (table (copy-syntax-table (syntax-table)))
        (slist imenu-syntax-alist))
    (dolist (syn slist)                 ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
	  (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc (lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus (icicle-delete-if-not
                          #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                          (mapcar (lambda (menu) ; Name an unlabeled menu `Others'.
                                    (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                  (if (boundp 'emacs-lisp-imenu-generic-expression)
                                      emacs-lisp-imenu-generic-expression
                                    lisp-imenu-generic-expression))))
                  (submenu (or (assoc "Functions" menus) (assoc "Others" menus)
                               (error "No command definitions in buffer")))
                  (regexp (cadr (assoc (car submenu) menus)))
                  (icicle-transform-function (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No command definitions in buffer"))
             (icicle-search beg end regexp require-match where
                            'icicle-imenu-non-interactive-function-p)))
      (set-syntax-table old-table))))

(defun icicle-imenu-command-p (ignored-hit-string ignored-marker)
  "Return non-nil for a command definition.
Predicate for `icicle-search'."
  (let ((indx (if (< emacs-major-version 21) 6 2)))
    (commandp (intern-soft
               (buffer-substring-no-properties (match-beginning indx) (match-end indx))))))

(defun icicle-imenu-non-interactive-function-p (ignored-hit-string ignored-marker)
  "Return non-nil for a non-interactive function definition.
Predicate for `icicle-search'."
  (let* ((indx (if (< emacs-major-version 21) 6 2))
         (fn (intern-soft
              (buffer-substring-no-properties (match-beginning indx) (match-end indx)))))
    (and (functionp fn) (not (commandp fn)))))

;;;###autoload
(defun icicle-save-string-to-variable (askp)
  "Save a string (text) to a variable.
You are prompted for the string to save.  Typically, you store a
regexp or part of a regexp in the variable.

By default, the variable is user option `icicle-input-string'.
To save to a different variable, use a prefix argument; you are then
prompted for the variable to use.

You can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' to insert a string from a
variable."
  (interactive "P")
  (let* ((enable-recursive-minibuffers t)
         (var (if askp
                  (intern (completing-read "Variable: " obarray 'boundp nil nil
                                           (if (boundp 'variable-name-history)
                                               'variable-name-history
                                             'icicle-variable-name-history)
                                           (symbol-name 'icicle-input-string)))
                'icicle-input-string))
         (text (icicle-completing-read-history (format "Text to save in `%s': " var))))
    (set var text)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'any)))
  (defalias 'any 'icicle-anything))
(when (> emacs-major-version 21)
  (defun icicle-anything (type)
    "Act on an object of type TYPE.
You are prompted for the type, then for an object of that type.  The
type is either the declared `type' of an Anything source, or its
`name' if it has no `type'.

This command is available only if you use library `anything.el'.

This is an Icicles multi-command: You can act on multiple objects in
multiple ways during a single command invocation.  When you choose an
object using `RET' or `mouse-2', the default action is applied to it.
The default action is also applied to the current completion candidate
when you use `C-RET', `C-mouse-2', and so on.

You can apply a different action by using an alternative action key:
`C-S-RET', `C-S-mouse-2', and so on.  This lets you choose the action
to apply using completion. You can use `C-RET', `C-mouse-2', and so
on, to perform multiple actions.

This command is intended for use only in Icicle mode."
    (interactive
     (let ((icicle-show-Completions-initially-flag t)
           (icicle-whole-candidate-as-text-prop-p icicle-anything-transform-candidates-flag))
       (unless (require 'anything nil t) (error "You must load library `anything.el' first"))
       (list (intern (completing-read "What (type): " (icicle-remove-duplicates
                                                       (mapcar #'list (icicle-get-anything-types)))
                                      nil t)))))
    (icicle-object-action type)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'file)))
  (defun file ()
    "Act on a file.  You are prompted for the file and the action.
This is just `icicle-object-action' with type `file'."
    (interactive) (icicle-object-action 'file)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'buffer)))
  (defun buffer ()
    "Act on a buffer.  You are prompted for the buffer and the action.
This is just `icicle-object-action' with type `buffer'."
    (interactive) (icicle-object-action 'buffer)))

(when (and icicle-define-alias-commands-flag (not (fboundp 'a)))
  (defalias 'a 'icicle-object-action))
(when (and icicle-define-alias-commands-flag (not (fboundp 'what-which-how)))
  (defalias 'what-which-how 'icicle-object-action))
;;;###autoload
(defun icicle-object-action (&optional type)
  "Act on an object of type TYPE.
You are prompted for the type (\"What\"), then for an object of that
type (\"Which\"), then for the action function to apply to the
object (\"How\").  For Anything types (see below), you are not
prompted for the action function.

The \"type\" of an object is one of these:

a. A member of the list `icicle-object-predicate-types'.  These are
   type predicates, such as `bufferp', `keywordp', or `atom'.

b. The `type' of an Anything source, or its `name' if it has no
   `type'.  This is available only if you use library `anything.el'.

c. A member of the list `icicle-object-named-types'.

In the case of Anything types (only), this is a multi-command:
* `C-RET', `C-mouse-2', and so on perform the default action.
* `C-S-RET', `C-S-mouse-2', and so on let you choose the action using
  completion.

The objects of types (b) and (c) are easily associated with names.
Their names are the completion candidates.  So, for instance, if you
choose type `buffer', then you can act on a buffer by choosing its
name.

The objects of predicate type (type a) are not necessarily named.  The
completion candidates for these objects are symbols whose values are
the objects acted upon.  So, for instance, if you choose type
`bufferp', then you can choose a symbol whose value is a buffer, in
order to act on that buffer.  While a buffer is always named, an
object of type `stringp' is not.  The value of `emacs-version' is one
such string that you can act on.  

Be aware that the action function you choose must accomodate the
object you choose as its only an argument.  Also, completion of the
function candidate itself is not strict, so you can enter a lambda
form.

With a prefix argument, the result of applying the function to the
object is pretty-printed using `pp-eval-expression'.  Otherwise, the
function is called for its effect only, and its value is not
displayed.

This command is intended for use only in Icicle mode."
  (interactive)
  (let* ((anything-loaded-p (and (> emacs-major-version 21) (require 'anything nil t)))
         (anything-types (and (not type) anything-loaded-p (icicle-get-anything-types)))
         (typ
          (or type
              (let ((icicle-show-Completions-initially-flag t))
                (intern
                 (completing-read
                  "What (type): "
                  (icicle-remove-duplicates (mapcar #'list (append anything-types
                                                                   icicle-object-named-types
                                                                   icicle-object-predicate-types)))
                  nil t)))))
         (predicate-type-p (and (member (symbol-name typ) icicle-object-predicate-types)
                                (not (memq typ anything-types))))
         (anything-candidates (and anything-loaded-p (not predicate-type-p)
                                   (icicle-get-anything-candidates-of-type typ)))
         (anything-default-actions (and anything-candidates
                                        (icicle-get-anything-default-actions-for-type typ)))
         (anything-actions (and anything-candidates (icicle-get-anything-actions-for-type typ)))
         (icicle-saved-completion-candidate (cond (predicate-type-p
                                                   (icicle-read-var-value-satisfying typ))
                                                  (anything-candidates
                                                   (icicle-choose-anything-candidate
                                                    typ anything-candidates anything-default-actions
                                                    anything-actions))
                                                  ((member (symbol-name typ)
                                                           (and anything-loaded-p
                                                                (icicle-get-anything-types)))
                                                   (error "No candidates for type `%s'"
                                                          (symbol-name typ)))
                                                  (t
                                                   (icicle-choose-candidate-of-type typ))))
         (icicle-candidate-action-fn (if anything-candidates
                                         (lambda (fn) (icicle-apply-to-saved-candidate fn t))
                                       'icicle-apply-to-saved-candidate))
         (icicle-candidate-alternative-action-fn (and anything-candidates
                                                      (lambda (fn)
                                                        (icicle-apply-to-saved-candidate fn t)))))
    (if anything-candidates
        (if (null (cdr anything-default-actions))
            (funcall (cdar anything-default-actions) icicle-saved-completion-candidate)
          (funcall (let ((icicle-show-Completions-initially-flag t))
                     (completing-read "How (action): " anything-default-actions nil t))
                   icicle-saved-completion-candidate))
      (icicle-apply-to-saved-candidate
       (completing-read "How (action): " obarray 'functionp)))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-types ()
    "Return list of types defined in `anything-sources'.  See `anything.el'."
    (and (boundp 'anything-sources) (consp anything-sources)
         (let ((types ())
               type)
           (dolist (source (anything-get-sources))
             (if (setq type (assoc-default 'type source))
                 (push (symbol-name type) types)
               (when (setq type (assoc-default 'name source)) (push type types))))
           (setq types
                 (mapcar (lambda (typ)
                           (setq typ (copy-sequence typ))
                           (put-text-property 0 (length typ) 'face 'icicle-special-candidate typ)
                           typ)
                         (icicle-remove-duplicates types)))))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-candidates-of-type (type)
    "Return list of Anything candidates for type TYPE.
Used only when `anything-sources' is non-nil - see `anything.el'."
    (and (boundp 'anything-sources) (consp anything-sources)
         (let* ((candidates ())
                (anything-candidate-cache nil))
           (dolist (source (anything-get-sources))
             (let ((init-fn (assoc-default 'init source))) (when init-fn (funcall init-fn)))
             (when (or (eq type (assoc-default 'type source))
                       (string= (symbol-name type) (assoc-default 'name source)))
               (setq candidates (append (icicle-get-anything-cached-candidates source)
                                        candidates))))
           (mapcar (lambda (cand) (if (consp cand) cand (list cand))) candidates)))))

;; Similar to `anything-get-cached-candidates' in `anything.el', but ignores processes.
(when (> emacs-major-version 21)
  (defun icicle-get-anything-cached-candidates (source)
    "Return cached value of candidates for Anything SOURCE.
Cache the candidates if there is not yet a cached value."
    ;; `anything-candidate-cache' is free here.
    (let* ((source-name (assoc-default 'name source))
           (candidate-cache (assoc source-name anything-candidate-cache))
           candidates)
      (if candidate-cache
          (setq candidates (cdr candidate-cache))
        (setq candidates (icicle-get-anything-candidates source))
        (when (processp candidates) (setq candidates nil))
        (setq candidate-cache (cons source-name candidates))
        (push candidate-cache anything-candidate-cache))
      candidates)))

;; Similar to `anything-get-candidates' in `anything.el', but ignores transformers.
(when (> emacs-major-version 21)
  (defun icicle-get-anything-candidates (source)
    "Return the list of candidates from Anything SOURCE."
    (let* ((candidate-source (assoc-default 'candidates source))
           (candidates
            (cond ((functionp candidate-source) (funcall candidate-source))
                  ((listp candidate-source) candidate-source)
                  ((and (symbolp candidate-source) (boundp candidate-source))
                   (symbol-value candidate-source))
                  (t
                   (error
                    (concat "Source `candidates' value is not a function, variable or list: %s")
                    candidate-source)))))
      (if (or (not icicle-anything-transform-candidates-flag) (processp candidates))
          candidates
        (anything-transform-candidates candidates source)))))

(when (> emacs-major-version 21)
  (defun icicle-get-anything-actions-for-type (type)
    "Set and return `icicle-candidates-alist' of actions for type TYPE."
    (setq icicle-candidates-alist nil)
    (let ((all-sources-actions ())
          this-source-actions)
      (dolist (source (anything-get-sources))
        (when (or (eq type (assoc-default 'type source))
                  (string= (symbol-name type) (assoc-default 'name source)))
          (setq this-source-actions (assoc-default 'action source))
          (dolist (action this-source-actions)
            (unless (memq action all-sources-actions)
              (push action all-sources-actions)))))
      (setq icicle-candidates-alist (sort all-sources-actions
                                          (lambda (a1 a2)
                                            (funcall icicle-sort-function (car a1) (car a2))))))))

(when (> emacs-major-version 21)
  (defun icicle-choose-anything-candidate (type candidates default-actions actions)
    "Read an Anything object of type TYPE with completion, and return it.
During completion, you can act on selected completion candidates, in
turn, using the action keys.
CANDIDATES is the list of candidates of type TYPE.
DEFAULT-ACTIONS is the list of default actions for type TYPE.
ACTIONS is the list of all actions for type TYPE."
    (let* ((win (selected-window))
           (icicle-whole-candidate-as-text-prop-p icicle-anything-transform-candidates-flag)
           (icicle-candidates-alist (if icicle-whole-candidate-as-text-prop-p
                                        candidates
                                      icicle-candidates-alist))
           (icicle-candidate-action-fn
            (lambda (obj)
              (when icicle-whole-candidate-as-text-prop-p
                (setq obj (icicle-anything-candidate-value obj)))
              (let ((enable-recursive-minibuffers t))
                (with-selected-window win
                  (if (null (cdr default-actions))
                      (funcall (cdar default-actions) obj)
                    (funcall (completing-read "How (action): " default-actions nil t) obj))))
              (select-frame-set-input-focus (window-frame (minibuffer-window)))
              (icicle-raise-Completions-frame)))
           (icicle-candidate-alternative-action-fn
            (lambda (obj)
              (when icicle-whole-candidate-as-text-prop-p
                (setq obj (icicle-anything-candidate-value obj)))
              (let ((icicle-show-Completions-initially-flag t)
                    (icicle-saved-completion-candidate obj)
                    (enable-recursive-minibuffers t))
                (with-selected-window win
                  (icicle-apply-to-saved-candidate
                   (let ((enable-recursive-minibuffers t)
                         (icicle-last-completion-candidate icicle-last-completion-candidate)
                         (icicle-candidate-action-fn
                          (lambda (actn) (with-selected-window win
                                           (let ((enable-recursive-minibuffers t))
                                             (icicle-apply-to-saved-candidate actn t)))))
                         (icicle-candidate-alternative-action-fn nil))
                     (completing-read "How (action): " actions nil t))
                   t))))))
      (let* ((icicle-transform-function 'icicle-remove-duplicates)
             (orig-action-fn icicle-candidate-action-fn)
            (icicle-candidate-help-fn
             (if icicle-whole-candidate-as-text-prop-p
                 (lambda (obj)
                   (let ((icicle-candidate-help-fn nil))
                     (icicle-help-on-candidate-symbol
                      (intern (icicle-anything-candidate-value obj)))))
               icicle-candidate-help-fn))
            (icicle-candidate-action-fn
             (if icicle-whole-candidate-as-text-prop-p
                 (lambda (obj)
                   (let ((icicle-last-input (icicle-anything-candidate-value obj)))
                     (funcall orig-action-fn obj)))
               icicle-candidate-action-fn)))
        (if icicle-whole-candidate-as-text-prop-p
            (icicle-anything-candidate-value
             (completing-read (concat "Which (" (symbol-name type) "): ") candidates nil t))
          (completing-read (concat "Which (" (symbol-name type) "): ") candidates nil t))))))

(when (> emacs-major-version 21)
  (defun icicle-anything-candidate-value (candidate)
    "Return the real value associated with string CANDIDATE."
    (or (cdr-safe (icicle-get-alist-candidate candidate)) candidate)))
         
(when (> emacs-major-version 21)
  (defun icicle-get-anything-default-actions-for-type (type)
    "Set and return `icicle-candidates-alist' of default actions for type TYPE."
    (setq icicle-candidates-alist nil)
    (let ((all-sources-actions ())
          this-source-actions)
      (dolist (source (anything-get-sources))
        (when (or (eq type (assoc-default 'type source))
                  (string= (symbol-name type) (assoc-default 'name source)))
          (setq this-source-actions (assoc-default 'action source))
          (unless (memq (car this-source-actions) all-sources-actions)
            (push (car this-source-actions) all-sources-actions))))
      (setq icicle-candidates-alist (sort all-sources-actions
                                          (lambda (a1 a2)
                                            (funcall icicle-sort-function (car a1) (car a2))))))))

(defun icicle-choose-candidate-of-type (type)
  "Read an object of type TYPE with completion, and return it."
  (case type
    (buffer
     (let ((icicle-must-match-regexp icicle-buffer-match-regexp)
           (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
           (icicle-must-pass-predicate icicle-buffer-predicate)
           (icicle-extra-candidates icicle-buffer-extras)
           (icicle-sort-function (or icicle-buffer-sort icicle-sort-function))
           (icicle-delete-candidate-object 'icicle-kill-a-buffer) ; `S-delete' kills buf
           (icicle-require-match-flag icicle-buffer-require-match-flag)
           (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag))
       (get-buffer-create
        (completing-read "Which (buffer): " (mapcar (lambda (buf) (list (buffer-name buf)))
                                                    (buffer-list))
                         nil nil nil 'buffer-name-history nil nil))))
    (command (intern (completing-read "Which (command): " obarray 'commandp)))
    (face (intern (completing-read "Which (face): " (mapcar (lambda (x) (list (format "%s" x)))
                                                            (face-list)))))
    (frame (let ((frame-names-alist (make-frame-names-alist)))
             (cdr (assoc (completing-read "Which (frame): " frame-names-alist)
                         frame-names-alist))))
    (function (intern (completing-read "Which (function): " obarray 'fboundp)))
    (option (intern (completing-read "Which (user option): " obarray 'user-variable-p)))
    (process (get-process (completing-read
                           "Which (process): " (mapcar (lambda (proc) (list (process-name proc)))
                                                       (process-list)))))
    (symbol (intern (completing-read "Which (symbol): " obarray)))
    (variable (intern (completing-read "Which (variable): " obarray 'boundp)))
    (window (let ((buffers nil))
              (walk-windows (lambda (win)
                              (push (list (format "%s" (window-buffer win))) buffers))
                            nil t)
              (get-buffer-window (completing-read "Window showing buffer: " buffers) 0)))
    (otherwise (error "Bad object type: %S" type))))

(defun icicle-read-var-value-satisfying (pred)
  "Reads a variable that satisfies PRED and returns its value."
  (symbol-value (intern (completing-read (format "Which (%s value of symbol): " pred) obarray
                                         `(lambda (symb)
                                            (and (boundp symb)
                                                 (funcall #',pred (symbol-value symb))))))))

;;;###autoload
(defun icicle-customize-icicles-group ()
  "Customize Icicles options and faces.  View their documentation."
  (interactive)
  (customize-group-other-window 'Icicles))

;;;###autoload
(defun icicle-send-bug-report ()
  "Send a bug report about an Icicles problem."
  (interactive)
  (browse-url (concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions.")))
 
;;(@* "Other top-level Icicles commands")
;;; Other top-level Icicles commands   .   .   .   .   .   .

;;;###autoload
(when (fboundp 'map-keymap)             ; Emacs 22.

  (defvar icicle-complete-keys-alist nil "Alist of keys and their bindings.
Each alist element is of the form (NAME KEY . BINDING), where:
 NAME is a symbol naming the key and its binding, whose name has form:
   KEYNAME  =  BINDING-NAME
 KEY is the actual key sequence
 BINDING is the actual binding of KEY.")

  (defun icicle-generic-S-tab ()
    "Treat `S-TAB' in minibuffer, *Completions*, and elsewhere.
Bound to `S-TAB' by default, but whatever it is bound to, does this:
 - Call `icicle-apropos-complete' if in minibuffer during completion.
 - Call `icicle-move-to-previous-completion' if in *Completions*.
 - Call `icicle-complete-keys', otherwise.

If you do not use the default minibuffer completion binding of
`S-TAB' for `icicle-apropos-complete', then you can simply bind
\[S-tab] to nil in `icicle-mode-map' in `icicle-mode-hook'."
    (interactive)
    (cond ((icicle-completing-p)        ; Cannot use the var here, since not sure to be in minibuf.
           (setq this-command 'icicle-apropos-complete)
           (icicle-apropos-complete))   ; Defined in `icicles-cmd.el'
          ((eq (get-buffer "*Completions*") (current-buffer))
           (setq this-command 'icicle-move-to-previous-completion)
           (icicle-move-to-previous-completion (prefix-numeric-value current-prefix-arg)))
          (t
           (setq this-command 'icicle-complete-keys)
           (icicle-complete-keys))))

  ;; This is a quick-and-dirty definition, not an efficient one.
  ;; It gathers all key bindings and then throws most of them away!  Good enough.
  (defun icicle-insert-char ()
    "Insert a character, using key completion.
Keys bound to `self-insert-command' are completion candidates."
    (interactive)
    (barf-if-buffer-read-only)
    (let ((icicle-complete-keys-self-insert-flag t)
          (icicle-must-match-regexp "^.+  =  self-insert-command"))
      (icicle-complete-keys)))

  (defun icicle-complete-keys () ; Bound to prefix keys followed by `S-TAB' (unless defined).
    "Complete a key sequence for the currently invoked prefix key.
Input-candidate completion and cycling are available.

You can navigate the key-binding hierarchy (prefix-key hierarchy),
just as would navigate a file-system hierarchy (to complete directory
and file names) or a menu hierarchy (to complete submenu and menu-item
names).

Completion candidates generally have the form `KEY  =  COMMAND'.

If COMMAND is `...', then KEY is a prefix key; choosing it updates the
completion candidates list to the keys under that prefix.  For
example, choosing `C-x = ...' changes the candidates to those with
prefix `C-x'.

The special candidate `..' means to go up one level of the key-binding
hierarchy and complete candidates there.  For example, if you are
currently completing prefix key `C-x 5', and you choose candidate
`..', then you will be completing prefix `C-x', the parent of `C-x 5'.

Except at the top level, the default value for completion is `..'.

If option `icicle-complete-keys-self-insert-flag' is non-nil, then
keys bound to `self-insert-command' are included as possible
completion candidates; otherwise (the default), they are not.  Command
`icicle-insert-char' works like `icicle-complete-keys', but in
includes only keys bound to `self-insert-command' - use it to insert a
character that is difficult or impossible to type with your keyboard.

You can use `C-M-,' at any time to switch between sorting with local
bindings first and sorting with prefix keys first.  You can use `C-,'
at any time to change the sort order among these two and sorting by
command name.

While cycling, these keys describe the command of the current
candidate:

`C-RET'   - Describe command of current completion candidate only
`C-down'  - Describe; move to next prefix-completion candidate
`C-up'    - Describe; move to previous prefix-completion candidate
`C-next'  - Describe; move to next apropos-completion candidate
`C-prior' - Describe; move to previous apropos-completion candidate
`up'      - Move to next prefix-completion candidate
`down'    - Move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`C-!'     - Describe *all* candidates, successively - use the [back]
            button in buffer *Help* to visit the descriptions

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'."
    (interactive)
    (let* ((icicle-transform-function 'icicle-remove-duplicates)
           (icicle-show-Completions-initially-flag t)
           (icicle-candidate-action-fn 'icicle-complete-keys-action)
           (enable-recursive-minibuffers t)
           (orig-buff (current-buffer)) ; Used as free var in `icicle-complete-keys-action'.
           (orig-window (selected-window)) ; Used as free var in `icicle-complete-keys-action'.
           (icicle-completing-keys-p t) ; Provide a condition to test key completion.
           (icicle-sort-function 'icicle-special-candidates-first-p)
           (icicle-alternative-sort-function 'icicle-prefix-keys-first-p)
           (icicle-sort-functions-alist
            '(("by key name, local bindings first" . icicle-special-candidates-first-p)
              ("by key name, prefix keys first" . icicle-prefix-keys-first-p)
              ("by command name" . icicle-command-names-alphabetic-p)
              ("turned OFF"))))
      (icicle-complete-keys-1 (icicle-this-command-keys-prefix))))

  (defun icicle-this-command-keys-prefix ()
    "Return the prefix of the currently invoked key sequence."
    (let ((this-key (this-command-keys))) (substring this-key 0 (1- (length this-key)))))

  (defun icicle-complete-keys-1 (prefix) ; PREFIX is a free var in `icicle-complete-keys-action'.
    "Complete a key sequence for prefix key PREFIX (a vector)."
    (let ((orig-extra-candidates icicle-extra-candidates)) ; Free in `icicle-complete-keys-action'.
      (unwind-protect
           (progn
             (icicle-keys+cmds-w-prefix prefix)
             (unless icicle-complete-keys-alist (error "No keys for prefix `%s'" prefix))
             (let* ((this-cmd-keys ; For error report - e.g. mouse cmd.
                     (this-command-keys-vector)) ; Free var in `icicle-complete-keys-action'.
                    (prefix-description
                     (icicle-key-description prefix (not icicle-key-descriptions-use-<>-flag)))
                    (candidate (completing-read
                                (concat "Complete keys" (and (not (string= "" prefix-description))
                                                             (concat " " prefix-description))
                                        ": ")
                                icicle-complete-keys-alist nil t nil nil
                                ;;$$ (if (equal [] prefix) nil "\\.\\.")
                                )))
               (icicle-complete-keys-action candidate)))
        (mapc (lambda (cand) (put (car cand) 'icicle-special-candidate nil)) ; Reset the property.
              icicle-complete-keys-alist))))

  (defun icicle-complete-keys-action (candidate)
    "Completion action function for `icicle-complete-keys'."
    (let* ((key+binding (cdr-safe (assq (intern candidate) icicle-complete-keys-alist)))
           (key (car-safe key+binding))
           (binding (cdr-safe key+binding))
           (cmd-name nil)
           (action-window (selected-window)))
      (unwind-protect
           (progn
             ;; `orig-buff' and `orig-window' are free here.  Defined in `icicle-complete-keys'.
             (set-buffer orig-buff)
             (select-window orig-window)
             (if (string= ".." candidate)
                 (setq cmd-name "..")
               (unless (and (string-match "\\(.+\\)  =  \\(.+\\)" candidate) (match-beginning 2))
                 (error "No match"))
               (setq cmd-name (substring candidate (match-beginning 2) (match-end 2))))      
             ;; `prefix', `orig-extra-candidates', and `this-cmd-keys' are free vars here.
             ;; They are defined in `icicle-complete-keys-1'.
             (cond ((string= ".." cmd-name) ; Go back up to parent prefix.
                    (setq last-command 'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat (nbutlast (append prefix nil)))))
                   ((and key (string= "..." cmd-name)) ; Go down to prefix.
                    (setq last-command 'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat prefix key)))
                   (t
                    (setq this-command binding last-command binding)
                    (setq icicle-extra-candidates orig-extra-candidates) ; Restore it.
                    (when (eq 'self-insert-command binding)
                      (unless key (error "Cannot insert `%s'" key))
                      (setq last-command-char (aref key 0)))
                    (when (eq 'digit-argument binding)
                      (setq last-command-char (aref key 0))
                      (icicle-msg-maybe-in-minibuffer "Numeric argument"))
                    (when (eq 'negative-argument binding)
                      (icicle-msg-maybe-in-minibuffer "Negative argument"))
                    (setq last-nonmenu-event 1) ; So *Completions* mouse-click info is ignored.
                    (condition-case try-command
                        (call-interactively binding nil this-cmd-keys)
                      (error (error (error-message-string try-command)))))))
        (select-window action-window))))

  (defun icicle-keys+cmds-w-prefix (prefix)
    "Fill `icicle-complete-keys-alist' for prefix key PREFIX (a vector)."
    (let ((prefix-map nil))
      (setq icicle-complete-keys-alist nil)
      (dolist (map (current-active-maps t))
        (setq prefix-map (lookup-key map prefix))
        ;; NOTE: `icicle-add-key+cmd' Uses `prefix' and `map' as free vars.
        (when (keymapp prefix-map) (map-keymap #'icicle-add-key+cmd prefix-map)))
      (unless (equal [] prefix) (push (list '\.\.) icicle-complete-keys-alist))
      icicle-complete-keys-alist))

  (defun icicle-add-key+cmd (event binding)
    "Add completion for EVENT and BINDING to `icicle-complete-keys-alist'."
    (cond
      ;; (menu-item ITEM-STRING): non-selectable item - skip it.
      ((and (eq 'menu-item (car-safe binding))
            (null (cdr-safe (cdr-safe binding))))
       (setq binding nil))          ; So `keymapp' test, below, fails.
      
      ;; (ITEM-STRING): non-selectable item - skip it.
      ((and (stringp (car-safe binding)) (null (cdr-safe binding)))
       (setq binding nil))          ; So `keymapp' test, below, fails.
      
      ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
      ((eq 'menu-item (car-safe binding))
       (let ((enable-condition (memq ':enable (cdr-safe (cdr-safe (cdr-safe binding))))))
         (if (or (not enable-condition)
                 (condition-case nil ; Don't enable if we can't check the condition.
                     (eval (cadr enable-condition))
                   (error nil)))
             (setq binding (car-safe (cdr-safe (cdr-safe binding))))
           (setq binding nil))))
      
      ;; (ITEM-STRING . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
      ((stringp (car-safe binding))
       (setq binding (cdr binding))
       ;; Skip HELP-STRING
       (when (stringp (car-safe binding)) (setq binding (cdr binding)))
       ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
       (when (and (consp binding) (consp (car binding))) (setq binding (cdr binding)))))
    
    ;; Follow indirections to ultimate symbol naming a command.
    (while (and (symbolp binding) (fboundp binding) (keymapp (symbol-function binding)))
      (setq binding (symbol-function binding)))

    ;; NOTE: `prefix' and `map' are free here.  They are bound in `icicle-keys+cmds-w-prefix'.
    (cond ((and (or (keymapp binding)
                    (and (commandp binding)
                         (equal binding (key-binding (vconcat prefix (vector event))))
                         (not (eq binding 'icicle-generic-S-tab))
                         (not (eq binding 'icicle-complete-keys))))
                (or (not (eq binding 'self-insert-command)) ; Command, keymap.
                    (and icicle-complete-keys-self-insert-flag ; Insert normal char.
                         (char-valid-p event))))
           (let* ((key-desc (propertize (single-key-description
                                         event
                                         (not icicle-key-descriptions-use-<>-flag))
                                        'face 'icicle-candidate-part))
                  (candidate (intern (concat key-desc "  =  " (if (keymapp binding)
                                                                  "..."
                                                                (prin1-to-string binding))))))
             (push (cons candidate (cons (vector event) binding)) icicle-complete-keys-alist)
             (when (eq map (current-local-map)) (put candidate 'icicle-special-candidate t))))
          ((and (integerp event) (generic-char-p event) ; Insert generic char.
                (eq 'self-insert-command  binding))
           (ignore))))                  ; Placeholder for future use.

  ;; $$ No longer used.  Was used in `icicle-complete-keys-1'.
  (defun icicle-read-single-key-description (string need-vector &optional no-angles)
    "If STRING contains a space, then the vector containing the symbol named STRING.
Otherwise, call `icicle-read-kbd-macro'.
Other args are as for `icicle-read-kbd-macro'."
    (cond ((and no-angles (string-match " " string)) (vector (intern string)))
          ((string-match "^<\\([^>]* [^>]*\\)>" string)
           (vector (intern (substring string (match-beginning 1) (match-end 1)))))
          (t (icicle-read-kbd-macro string need-vector no-angles))))

  ;; $$ No longer used.  Was used as `icicle-candidate-action-fn' in `icicle-complete-keys'.
  (defun icicle-complete-keys-help (candidate)
    "Describe the command associated with the current completion candidate."
    (interactive)              ; Interactively, just describes itself.
    (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
    (string-match "\\(.+\\)  =  \\(.+\\)" candidate)
    (let ((frame-with-focus (selected-frame))
          (cmd (intern-soft (substring candidate (match-beginning 2) (match-end 2)))))
      (if (not (functionp cmd))
          (icicle-msg-maybe-in-minibuffer "No help")
        (describe-function cmd))
      (icicle-raise-Completions-frame)
      ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
      ;; because the *Help* frame takes the focus away from the minibuffer frame.
      ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
      (let* ((help-window (get-buffer-window "*Help*" 0))
             (help-frame (and help-window (window-frame help-window))))
        (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
    (message nil))        ; Let minibuffer contents show immmediately.

  (defun icicle-read-kbd-macro (start &optional end no-angles)
    "Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a string if possible, otherwise an event vector.
Second argument NEED-VECTOR means to return an event vector always.

Optional argument NO-ANGLES non-nil means to expect key
descriptions not to use angle brackets (<...>).  For example:
 
 (icicle-read-kbd-macro \"<mode-line>\" t)   returns [mode-line]
 (icicle-read-kbd-macro  \"mode-line\"  t t) returns [mode-line]"
    (interactive "r")
    (if (stringp start)
        (icicle-edmacro-parse-keys start end no-angles)
      (setq last-kbd-macro
            (icicle-edmacro-parse-keys (buffer-substring start end) nil no-angles))))

  (defun icicle-edmacro-parse-keys (string &optional need-vector no-angles)
    "Same as `edmacro-parse-keys', but with added NO-ANGLES argument.
NO-ANGLES is the same as for `icicle-read-kbd-macro'."
    (let ((case-fold-search nil)
          (pos 0)
          (res []))
      (while (and (< pos (length string))
                  (string-match "[^ \t\n\f]+" string pos))
        (let ((word (substring string (match-beginning 0) (match-end 0)))
              (key nil)
              (times 1))
          (setq pos (match-end 0))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times (string-to-number (substring word 0 (match-end 1))))
            (setq word (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key (vconcat (if (eq (key-binding [?\M-x])
                                            'execute-extended-command)
                                        [?\M-x]
                                      (or (car (where-is-internal
                                                'execute-extended-command))
                                          [?\M-x]))
                                    (substring word 2 -2) "\r")))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos (string-match "$" string pos)))
                ((and (string-match (if no-angles
                                        "^\\(\\([ACHMsS]-\\)*\\)\\(..+\\)$"
                                      "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$")
                                    word)
                      (or (not no-angles)
                          (save-match-data (not (string-match "^\\([ACHMsS]-.\\)+$" word))))
                      (progn
                        (setq word (concat (substring word (match-beginning 1)
                                                      (match-end 1))
                                           (substring word (match-beginning 3)
                                                      (match-end 3))))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                              word))))
                 (setq key (list (intern word))))
                (t
                 (let ((orig-word word) (prefix 0) (bits 0))
                   (while (string-match "^[ACHMsS]-." word)
                     (incf bits (cdr (assq (aref word 0)
                                           '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                             (?H . ?\H-\^@) (?M . ?\M-\^@)
                                             (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                     (incf prefix 2)
                     (callf substring word 2))
                   (when (string-match "^\\^.$" word)
                     (incf bits ?\C-\^@)
                     (incf prefix)
                     (callf substring word 1))
                   (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                              ("LFD" . "\n") ("TAB" . "\t")
                                              ("ESC" . "\e") ("SPC" . " ")
                                              ("DEL" . "\177")))))
                     (when found (setq word (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (loop for ch across word
                        for n = 0 then (+ (* n 8) ch -48)
                        finally do (setq word (vector n))))
                   (cond ((= bits 0)
                          (setq key word))
                         ((and (= bits ?\M-\^@) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key (loop for x across word collect (+ x bits))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key (list (+ bits (- ?\C-\^@)
                                             (logand (aref word 0) 31)))))
                         (t
                          (setq key (list (+ bits (aref word 0)))))))))
          (when key
            (loop repeat times do (callf vconcat res key)))))
      (when (and (>= (length res) 4)
                 (eq (aref res 0) ?\C-x)
                 (eq (aref res 1) ?\()
                 (eq (aref res (- (length res) 2)) ?\C-x)
                 (eq (aref res (- (length res) 1)) ?\)))
        (setq res (edmacro-subseq res 2 -2)))
      (if (and (not need-vector)
               (loop for ch across res
                  always (and (char-valid-p ch)
                              (let ((ch2 (logand ch (lognot ?\M-\^@))))
                                (and (>= ch2 0) (<= ch2 127))))))
          (concat (loop for ch across res
                     collect (if (= (logand ch ?\M-\^@) 0)
                                 ch (+ ch 128))))
        res))))

;;;###autoload
;; See also `hexrgb-read-color' in `hexrgb.el'.
(defun icicle-read-color (&optional arg prompt)
  "Read a color name or hex RGB color value #RRRRGGGGBBBB.
A string value is returned.
Interactively, optional argument ARG is the prefix arg.
Optional argument PROMPT is the prompt to use (default \"Color: \").

With plain `C-u', use `hexrgb-read-color', which lets the user
complete a color name or input any valid RGB hex value (without
completion).  

With no prefix arg, return a string with both the color name and the
RGB value, separated by `icicle-list-nth-parts-join-string'.

With a numeric prefix arg of 0 or 1, return the color name.  With any
other numeric prefix arg, return the RGB value.

In the plain `C-u' case, user input is checked to ensure that it
represents a valid color.

In all other cases:

- A user can complete input against the color name or the RGB value,
  or both.

- If a user enters input without completing or cycling, the input is
  not checked: whatever is entered is returned as the string value.

From Emacs Lisp, you can use ARG to control the behavior, or you can
use `icicle-list-use-nth-parts' if ARG is nil.

Note: Duplicate color names are removed by downcasing and removing
whitespace.  For example, \"AliceBlue\" and \"alice blue\" are both
treated as \"aliceblue\".  Otherwise, candidates with different names
but the same RGB values are not considered duplicates, so, for
example, users can match either \"darkred\" or \"red4\", which both
have RGB #8b8b00000000.  Users can toggle duplicate removal at any
time using `C-$'.

This command is intended only for use in Icicle mode (but it can be
used with `C-u', with Icicle mode turned off)."
  (interactive "P")
  (unless (featurep 'hexrgb) (error "`icicle-read-color' requires library `hexrgb.el'"))
  (let (color)
    (if (consp arg)                     ; Plain `C-u': complete against color name only, 
        (let ((icicle-transform-function 'icicle-remove-color-duplicates))
          (setq color (hexrgb-read-color t))) ; and be able to input any valid RGB string.

      ;; Complete against name+RGB pairs, but user can enter invalid value without completing.
      (let ((completion-ignore-case t)
            (icicle-transform-function 'icicle-remove-color-duplicates)
            (icicle-sort-functions-alist
             '(("by color name" . icicle-part-1-lessp)
               ("by color hue" . (lambda (s1 s2) (not (icicle-color-hue-lessp s1 s2))))
               ("by color purity (saturation)" .
                (lambda (s1 s2) (not (icicle-color-saturation-lessp s1 s2))))
               ("by color brightness (value)" .
                (lambda (s1 s2) (not (icicle-color-value-lessp s1 s2))))
               ("by amount of red" . (lambda (s1 s2) (not (icicle-color-red-lessp s1 s2))))
               ("by amount of green" . (lambda (s1 s2) (not (icicle-color-green-lessp s1 s2))))
               ("by amount of blue" . (lambda (s1 s2) (not (icicle-color-blue-lessp s1 s2))))
               ("by color rgb" . (lambda (s1 s2) (not (icicle-part-2-lessp s1 s2))))
               ("turned OFF")))
            ;; Make the two `*-join-string' variables the same, so past inputs are recognized.
            ;; Do not use " " as the value, because color names such as "white smoke" would be
            ;; split, and "smoke" would not be recognized as a color name when trying to list
            ;; candidates in *Completions*.
            (icicle-list-nth-parts-join-string ": ")
            (icicle-list-join-string ": ")
            (icicle-list-end-string ""))
        (let ((icicle-list-use-nth-parts
               (or (and arg (list arg)) ; 1 or 2, by program or via `C-1' or `C-2'.
                   icicle-list-use-nth-parts ; Bound externally by program.
                   '(1 2))))            ; Both parts, by default.
          (setq color (completing-read
                       (or prompt "Color: ")
                       (mapcar #'icicle-make-color-candidate (x-defined-colors)))))))
    (when (interactive-p) (message "Color: `%s'" color))
    color))

(defun icicle-make-color-candidate (color-name)
  "Return multi-completion candidate of COLOR-NAME and its hex RGB string.
If `icicle-WYSIWYG-Completions-flag' is non-nil, then the hex RGB
string has the color as its background text property."
  (unless (featurep 'hexrgb) (error "`icicle-make-color-candidate' requires library `hexrgb.el'"))
  (let ((rgb-string (hexrgb-color-name-to-hex color-name)))
    (when icicle-WYSIWYG-Completions-flag
      (put-text-property 0 (length rgb-string) 'face (cons 'background-color rgb-string)
                         rgb-string))
    (list (list color-name rgb-string))))

(defun icicle-remove-color-duplicates (list)
  "Copy of LIST with duplicate color candidates removed.
Candidates are considered duplicates if they have the same color name,
abstracting from whitespace and letter case."
  (let ((tail list)
        new)
    (save-match-data
      (while tail
        (let ((this (car tail)))
          (string-match ": " this)
          (setq this (icicle-delete-whitespace-from-string (downcase this) 0 (match-beginning 0)))
          (unless (member this new) (push this new)))
        (pop tail)))
    (nreverse new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-cmd.el ends here

0

