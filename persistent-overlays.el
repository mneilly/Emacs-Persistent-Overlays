;;; persistent-overlays.el --- Minor mode to store selected overlays to be loaded later -*- coding: utf-8 -*-

;; Copyright (C) 2016 Michael R. Neilly

;; Author: Michael Neilly <mneilly@yahoo.com>
;; Package-Version: 0.9
;; Keywords: overlays persistent
;; URL: https://github.com/mneilly/Emacs-Persistent-Overlays

;;; Commentary:

;; persistent-overlays is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; persistent-overlays is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; About
;;
;; Bugs should be reported on the github issues page:
;;   https://github.com/mneilly/Emacs-Persistent-Overlays/issues
;; 
;; The goal for persistent-overlays is to maintain persistent overlays
;; between Emacs sessions.  It has been tested with hideshow and
;; outline modes on Linux, Mac OS X and Windows.  However, this version
;; should be considered beta software and it has not been previously
;; released.
;; 
;; Overlays are stored in ~/.emacs-pov by default.  There are several
;; customizable variables which allow a user to change the file naming
;; convention and the storage location of the overlay files.
;;
;; Please use describe-mode for a full description.
;;
;; To enable this mode add the following to your ~/.emacs or
;; ~/emacs.d/init.el file.  This assumes that you have placed
;; persistent-overlays.el somewhere in your load-path.
;;
;; (load-library "persistent-overlays")
;;
;; Enjoy
;;

;;; Code:
(defgroup persistent-overlays nil
  "Minor mode for storing and loading overlays for a buffers."
  :prefix "persistent-overlays-"
  :group 'languages)

;; ===== Customization variables ==============================================

(defcustom persistent-overlays-property-names '(invisible)
  "Indicates a list of overlay property names.
If any of these properties exist in an overlay it will be handled
by this mode.\n\nBy default it is set to `invisible'."
  :type '(repeat symbol)
  :group 'persistent-overlays)
;; make it buffer local because different buffers may be using different
;; modes and minor modes
(make-variable-buffer-local 'persistent-overlays-property-names)

(defcustom persistent-overlays-disable-on-major-mode-change nil
  "When t switching major modes will disable persistent-overlays-minor-mode.
By default it nil so that persistent-overlays-minor-mode remains enabled when
switching major mods."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom persistent-overlays-directory "~/.emacs-persistent-overlays"
  "Specifies the full path to the directory in which to store overlays.
\n\nBy default it is set to ~/.emacs-persistent-overlays"
  :type 'directory
  :group 'persistent-overlays)

(defcustom persistent-overlays-use-path-name nil
  "By default overlay files are named as the buffer name plus a SHA1 hash.
If this variable is set to t, the overlay file will be named
using the full path of the file by replacing the file separators
with underscores."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom persistent-overlays-store-with-user-file nil
  "By default overlay files are stored in `persistent-overlays-directory'.
If this variable is t, files will be stored alongside the user
file instead."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom persistent-overlays-store-as-hidden nil
  "If this variable is t, overlay file names will start with a '.'.
By default overlay files are named as per `persistent-overlays-use-path-name'."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom persistent-overlays-auto-load nil
  "When t overlays are automatically loaded.
Overlays are loaded from the corresponding overlay file in
`persistent-overlays-directory' when a buffer is loaded.  By default this is nil."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom persistent-overlays-auto-merge t
  "When t overlays are automatically loaded and merged.
Overlays are loaded from the corresponding overlay file in
`persistent-overlays-directory' when a buffer is loaded.  This is
useful when you have added overlays to a file before enabling
persistent-overlays-minor-mode but you already having overlays
saved that you don't want to lose.  If
`persistent-overlays-auto-merge' is t
`persistent-overlays-auto-load' is ignored.\n\nBy default this is
t."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom persistent-overlays-auto-save t
  "When t overlays are automatically saved.
Overlays are saved to the corresponding overlay file in
`persistent-overlays-directory' when a buffer is saved.\n\nBy default this is t."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom persistent-overlays-minor-mode-hook nil
  "Hook called when `persistent-overlays-minor-mode' is activated or deactivated."
  :type 'hook
  :group 'persistent-overlays
  :version "24.5")

;; ===== variables ============================================================

(defvar persistent-overlays-minor-mode nil
  "Indicates if this mode is active.
Use the command `persistent-overlays-minor-mode' to toggle or set this
variable.")

;; ===== key map ==============================================================

(defvar persistent-overlays-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; default binding for explicitly saving and loading overlays
    (define-key map "\C-c*\C-s"	'persistent-overlays-save-overlays)
    (define-key map "\C-c*\M-s"	'persistent-overlays-savex-overlays)
    (define-key map "\C-c*\C-m"	'persistent-overlays-merge-overlays)
    (define-key map "\C-c*\M-m"	'persistent-overlays-mergex-overlays)
    (define-key map "\C-c*\C-l"	'persistent-overlays-load-overlays)
    (define-key map "\C-c*\M-l"	'persistent-overlays-loadx-overlays)
    map)
  "Keymap for persistent-overlays minor mode.")

;; ===== internal functions ===================================================

(defun persistent-overlays-read-overlays (&optional file)
  "This is an internal function to read overlays from FILE.
Overlays are read into a temporary buffer which is used for
loading and merging."
  (let ((tbuf (generate-new-buffer "**persistent-overlays**")))
    (if (file-exists-p file)
	(with-current-buffer tbuf
	  ;; only keep elisp code that is in the expected format
	  (insert-file-contents file)
	  (keep-lines "(let ((tovly (make-overlay [[:digit:]]+ [[:digit:]]+)) (tplist '(.*?))) (while tplist (let ((tp (car tplist)) (tpv (cadr tplist))) (overlay-put tovly tp tpv)) (setq tplist (cddr tplist))))")
	  )
      (when (char-or-string-p file)
	(progn
	  (kill-buffer tbuf)
	  (setq tbuf nil))))
    tbuf))

(defun persistent-overlays-remove-overlays (&optional start end name value)
  "This is an internal function which wraps `remove-overlays'.
The START, END and NAME argments match those passed to `remove-overlays'.
If VALUE is set to 'ANY, then all values are matched.  This allows
removal of overlays by matching only the name."
  (if (eq value 'ANY)
      (let ((ovlys (overlays-in start end)))
	(while ovlys
	  (let ((ovly (car ovlys)))
	    (when (overlay-get ovly name) (delete-overlay ovly)))
	  (setq ovlys (cdr ovlys))))
    (remove-overlays start end name value)))

(defun persistent-overlays-get-existing-overlays ()
  "This is an internal function to read existing overlays.
Elisp code to recreate them is stored in a temporary buffer for
use when saving or merging overlays."
  (let ((tbuf (generate-new-buffer "**persistent-overlays**")))
    (save-excursion
      (let ((ovlys (overlays-in (point-min) (point-max))))
	(while ovlys
	  (let ((ovly (car ovlys)) (props persistent-overlays-property-names))
	    (while props
	      (let ((prop (car props)) (oprops nil))
		(when (overlay-get ovly prop)
		  (setq oprops (overlay-properties ovly))
		  (princ (format "(let ((tovly (make-overlay %d %d)) (tplist '%s)) (while tplist (let ((tp (car tplist)) (tpv (cadr tplist))) (overlay-put tovly tp tpv)) (setq tplist (cddr tplist))))\n" (overlay-start ovly) (overlay-end ovly) oprops) tbuf)))
	      (setq props (cdr props))))
	  (setq ovlys (cdr ovlys)))
	(set-buffer tbuf)
	(goto-char (point-min))
	))
    tbuf))

(defun persistent-overlays-delete-duplicate-lines (buf)
  "This is an internal function to remove duplicate lines from BUF.
It is only used if `delete-duplicate-lines' is not available.  It
is used to ensure that dupliate overlays are not created when
merging overlays."
  (with-current-buffer buf
    (let ((end (copy-marker (point-max))))
      (while
	  (progn
	    (goto-char (point-min))
	    (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
	(replace-match "\\1\n\\2")))))

(defun persistent-overlays-get-ovly-fname (full fname)
  "Internal function to generate the file name for overlay files.
FULL is the full path name of the file being visited.  FNAME is just
the filename being visited."
  ; strip unique identifier off of fname
  (setq fname (replace-regexp-in-string "<.*?>$" "" fname))
  (let ((name (if persistent-overlays-use-path-name (replace-regexp-in-string "[:/\\]" "_" full) (concat fname "-" (sha1 full) ".povly")))
	(dir (if persistent-overlays-store-with-user-file (replace-regexp-in-string "[:/\\][^/\\]*$" "" full) persistent-overlays-directory)))
    (when persistent-overlays-store-as-hidden (setq name (concat "." name)))
    (concat dir "/" name)))

(defun persistent-overlays-disable ()
  "Internal function to disable persistent-overlays-minor-mode upon a major mode change."
  (persistent-overlays-minor-mode -1))

;; ===== merge functions ======================================================

(defun persistent-overlays-mergex-overlays (&optional ovfile)
  "Explicitly merge in overlays.
OVFILE must be explicity provided.  The overlay file OVFILE
specifies the path to the overlay file to be loaded and merged.

NOTE: Loaded overlays are merged with existing overlays.  See
`persistent-overlays-load-overlays' and
`persistent-overlays-loadx-overlays' if you want to load overlays
without merging."
  (interactive "f")
  (when (persistent-overlays-merge-overlays ovfile)
      (message "Overlays were loaded and merged from %s" ovfile)))

;;;###autoload
(defun persistent-overlays-merge-overlays (&optional ovfile)
  "Merge overlays.
If OVFILE is not provided, the overlay file corresponding to the
current buffer is loaded from `persistent-overlays-directory' and
merged with existing overlays.  If OVFILE is provided, it
specifies the path to the overlay file to be merged.

NOTE: If a loaded overlay has exactly the same properties and
values as an existing overlay only a single overlay is retained."
  (interactive)
  (when (buffer-file-name)
    (let ((file (if (char-or-string-p ovfile)
		    ovfile
		  (persistent-overlays-get-ovly-fname (buffer-file-name) (buffer-name))))
	  (tbuf nil)
	  (xbuf nil)
	  (curbuf nil)
	  (merged nil))
      (with-temp-message (format "Merging in overlays from %s..." (buffer-name))
	(setq curbuf (buffer-name))
	(save-excursion
	  (setq tbuf (persistent-overlays-read-overlays file))
	  (if tbuf
	      (progn
		(setq xbuf (persistent-overlays-get-existing-overlays))
		(set-buffer xbuf)
		(append-to-buffer tbuf (point-min) (point-max))
		(set-buffer tbuf)
		(when (fboundp 'delete-duplicate-lines)
		  (delete-duplicate-lines (point-min) (point-max))
		  (persistent-overlays-delete-duplicate-lines tbuf))
		(set-buffer curbuf)
		(let ((props persistent-overlays-property-names))
		  (while props
		    (let ((prop (car props)))
		      (persistent-overlays-remove-overlays (point-min) (point-max) prop 'ANY))
		    (setq props (cdr props))
		    ))
		(eval-buffer tbuf)
		(kill-buffer tbuf)
		(kill-buffer xbuf)
		(setq merged t))
	    (when (char-or-string-p ovfile) (message "File %s does not exist." ovfile)))))
      merged)))
  

;; ===== load functions =======================================================

(defun persistent-overlays-loadx-overlays (&optional ovfile)
  "Explicitly load overalys.
OVFILE must be explicity provided.  The overlay file OVFILE
specifies the path to the overlay file to be loaded.

NOTE: All overlays that contain properties in
`persistent-overlays-property-names' are removed before loading
overlays.  Loaded overlays are not merged with existing overlays.
See `persistent-overlays-merge-overlays' and
`persistent-overlays-mergex-overlays' if you want to merge
overlays."
  (interactive "f")
  (when (persistent-overlays-load-overlays ovfile)
      (message "Overlays were loaded from %s" ovfile)))

;;;###autoload
(defun persistent-overlays-load-overlays (&optional ovfile)
  "Load overalys.
If OVFILE is not provided the overlay file corresponding to
the current buffer is loaded from `persistent-overlays-directory'.  If OVFILE is
provided it specifies the path to the overlay file to be loaded.

NOTE: All overlays that contain properties in
`persistent-overlays-property-names' are removed before loading overlays.  Loaded
overlays are not merged with existing overlays.  A future version
may provide an `persistent-overlays-merge-overlays' function."
  (interactive)
  (when (buffer-file-name)
    (let ((file (if (char-or-string-p ovfile) ovfile (persistent-overlays-get-ovly-fname (buffer-file-name) (buffer-name))))
	  (tbuf nil)
	  (loaded nil))
      (with-temp-message (format "Loading overlays for %s..." (buffer-name))
	(setq tbuf (persistent-overlays-read-overlays file))
	(if tbuf
	    (progn
	      (let ((props persistent-overlays-property-names))
		(while props
		  (let ((prop (car props)))
		    (persistent-overlays-remove-overlays (point-min) (point-max) prop 'ANY))
		  (setq props (cdr props))
		  ))
	      (eval-buffer tbuf)
	      (kill-buffer tbuf)
	      (setq loaded t))
	  (when (char-or-string-p ovfile) (message "File %s does not exist." ovfile))))
      loaded)))

;; ===== save functions =======================================================

(defun persistent-overlays-savex-overlays (&optional ovfile)
  "Explicitly save overlays.
OVFILE must be explicity provided.  The overlay file
corresponding to the current buffer is saved in OVFILE.  Usually,
`persistent-overlays-save-overlays' should be used instead.

NOTE: Only overlays that contain properties in
`persistent-overlays-property-names' are saved.  If the overlay
already exists it is overwritten."
  (interactive "F")
  (when (persistent-overlays-save-overlays ovfile)
      (message "Overlays were saved to %s" ovfile)))

;;;###autoload
(defun persistent-overlays-save-overlays (&optional ovfile)
  "Save overlays.
If OVFILE is not provided, the overlay file corresponding to the
current buffer is saved in the directory pointed to by
`persistent-overlays-directory'.  If OVFILE is provided, it
specifies the path of the overlay file to be saved.

NOTE: Only overlays that contain properties in
`persistent-overlays-property-names' are saved.  If the overlay
already exists it is overwritten."
  (interactive)
  (let ((file (if (char-or-string-p ovfile) ovfile (persistent-overlays-get-ovly-fname (buffer-file-name) (buffer-name))))
	(tbuf nil)
	(saved nil))
    (with-temp-message (format "Saving overlays for %s..." (buffer-name))
      (save-excursion
	(setq tbuf (persistent-overlays-get-existing-overlays))
	(set-buffer tbuf)
	(if (> (buffer-size) 0)
	    (progn
	      (goto-char (point-min))
	      (insert (concat ";; " (buffer-file-name) "\n;; " (current-time-string) "\n"))
	      (write-region (point-min) (point-max) file nil 'quietly))
	  (when (file-exists-p file) (delete-file file)))
	(kill-buffer tbuf))
      (setq saved t))
    saved))

;; ===== Define the mode ======================================================

;;;###autoload
(define-minor-mode persistent-overlays-minor-mode
  "This is a minor mode to make overlays persistent by saving
them to a file and subsequently loading them. By default overlays
are stored in ~/.emacs-persistent-overlays with a file name that
is created as a combination of the buffer name and a SHA1 hash of
the file name to which they correspond.
`persistent-overlays-directory' may be set to indicate a
different directory in which to store overlays. If
`persistent-overlays-use-path-name' is set to t, the overlay file
is named using the underscore delimited full path of the file
instead of using a SHA1 hash. Overlay file names always end with
.povly.

`persistent-overlays-property-names' specifies a list of property
symbols that must exist in an overlay for it to be handled by
this mode. By default it is set to a list containing the symbol
'invisible so that overlays which hide sections of a buffer are
stored. This was the primary motivation for this mode.

If `persistent-overlays-auto-save' is t all overlays containing
properties in `persistent-overlays-property-names' will be saved
in the directory indicated by `persistent-overlays-directory'
when the buffer is saved.

If `persistent-overlays-auto-load' is t existing overlays for the
buffer will be loaded from `persistent-overlays-directory' when
the buffer is loaded or when `persistent-overlays-minor-mode' is
enabled.

If `persistent-overlays-auto-merge' is t existing overlays for
the buffer will be loaded and merged from
`persistent-overlays-directory' when the buffer is loaded and
when `persistent-overlays-minor-mode' is enabled. This is the
default behavior which is useful when
persistent-overlays-minor-mode is enabled after having created
additional overlays.

Other custom variables are
`persistent-overlays-store-with-user-file',
`persistent-overlays-store-as-hidden' and
`persistent-overlays-disable-on-major-mode-change'.  Please use
describe-variable for details on those variables.

NOTE: There is currently no mechanism for binding overlay files
with the corresponding user files when files are moved. If you
wish to retain existing stored overlays you can manually load or
merge the overlays from the original overlay file after renaming
the user file. You may also want to delete the original overlay
file after merging to avoid having unused overlay files lying
around. Each overlay file contains the full path name of the file
to which it applies as its first line.

Key bindings:
\\{persistent-overlays-minor-mode-map}"
  :group 'persistent-overlays
  :lighter " povly"
  :keymap persistent-overlays-minor-mode-map
  (if persistent-overlays-minor-mode
      (progn ; persistent-overlays-minor-mode was just enabled
	(format-mode-line mode-name)
        (when persistent-overlays-disable-on-major-mode-change (add-hook 'change-major-mode-hook (persistent-overlays-disable) nil t))
	(when persistent-overlays-auto-save (add-hook 'after-save-hook 'persistent-overlays-save-overlays nil t))
	(if persistent-overlays-auto-merge (persistent-overlays-merge-overlays) (when persistent-overlays-auto-load (persistent-overlays-load-overlays)))
	)
    (progn ; persistent-overlays-minor-mode was just disabled
      (remove-hook 'after-save-hook 'persistent-overlays-save-overlays t)
      (remove-hook 'change-major-mode-hook 'persistent-overlays-disable t))))

(provide 'persistent-overlays)

;;; persistent-overlays.el ends here
