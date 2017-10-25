19;;; icicles-mcmd.el --- Minibuffer commands for Icicles
;;
;; Filename: icicles-mcmd.el
;; Description: Minibuffer commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Tue Sep 04 13:39:31 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 9743
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mcmd.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `cl', `color-theme', `cus-face',
;;   `easymenu', `ffap', `ffap-', `help-mode', `hexrgb',
;;   `icicles-fn', `icicles-opt', `icicles-var', `pp', `pp+',
;;   `thingatpt', `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  commands to be used mainly in the minibuffer or buffer
;;  *Completions* (and a few non-interactive functions used in those
;;  commands).  For top-level commands, see `icicles-cmd.el'.  For
;;  Icicles documentation, see `icicles.el'.
;;
;;  Commands defined here:
;;
;;    `icicle-abort-minibuffer-input',
;;