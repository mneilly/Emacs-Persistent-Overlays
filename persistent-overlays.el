;;; persistent-overlays.el --- minor mode to store selected overlays to be loaded later -*- coding: utf-8 -*-

;; Copyright (C) 2016 Michael R. Neilly

;; Author: Michael Neilly <mneilly@yahoo.com>
;; Keywords: C C++ java lisp tools editing comments blocks hiding outlines
;; Maintainer-Version: 0.9
;; URL: https://github.com/mneilly/Emacs-Persistent-Overlays

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
;; The goal for persistent-overlays is to maintain persistent overlays
;; between emacs sessions. It has been tested with hideshow and
;; outline modes on Linux, Mac OS X and Windows. However, this version
;; should be considered beta software and it has not been previously
;; released.
;; 
;; Overlays are stored in ~/.emacs-pov by default. There are several
;; customizable variables which allow a user to change the file naming
;; convention and the storage location of the overlay files.
;;
;; Please use describe-mode for a full description.
;;
;; To enable this mode add the following to your ~/.emacs or
;; ~/emacs.d/init.el file. This assumes that you have placed
;; persistent-overlays.el somewhere in your load-path.
;;
;; (load-library "persistent-overlays")
;;
;; Enjoy
;;

(defgroup persistent-overlays nil
  "Minor mode for storing and loading overlays for a buffers."
  :prefix "pov-"
  :group 'languages)

;; ===== Customization variables ==============================================

(defcustom pov-property-name 'invisible
  "Indicates the name of the property that must exist in an
overlay for it to be handled by this mode.\n\nBy default it is set
to `invisible'."
  :type 'symbol
  :group 'persistent-overlays)
;; make it buffer local because different buffers may be using different
;; modes and minor modes
(make-variable-buffer-local 'pov-property-name)

(defcustom pov-disable-on-major-mode-change nil
  "When t switching major modes will disable pov-minor-mode. By
default it nil so that pov-minor-mode remains enabled when
switching major mods."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom pov-directory "~/.emacs-pov"
  "Specifies the full path to the directory in which to store
overlays.\n\nBy default it is set to ~/.emacs-pov"
  :type 'directory
  :group 'persistent-overlays)

(defcustom pov-use-path-name nil
  "By default overlay files are named as the buffer name
appended with a SHA1 hash. If this variable is set to t, the
overlay file will be named using the full path of the file by
replacing the file separators with underscores."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom pov-store-with-user-file nil
  "By default overlay files are stored in the directory specified
by `pov-directory'. If this variable is t, files will be stored
alongside the user file instead."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom pov-store-as-hidden nil
  "By default overlay files are named as per `pov-use-path-name'.
If this variable is t, overlay file names will start with a .
which makes them hidden files on most systems."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom pov-auto-load nil
  "When t overlays are automatically loaded from the corresponding
overlay file in `pov-directory' when a buffer is loaded. By default
this is nil"
  :type 'boolean
  :group 'persistent-overlays)

(defcustom pov-auto-merge t
  "When t overlays are automatically loaded and merged from the
corresponding overlay file in `pov-directory' when a buffer is
loaded. This is useful when you have added overlays to a file
before enabling pov-minor-mode but you already having overlays
saved that you don't want to lose. If `pov-auto-merge' is t
`pov-auto-load' is ignored.\n\nBy default this is t."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom pov-auto-save t
  "When t overlays are automatically saved to the corresponding
overlay file in `pov-directory' when a buffer is saved.\n\nBy
default this is t."
  :type 'boolean
  :group 'persistent-overlays)

(defcustom pov-minor-mode-hook nil
  "Hook called when `pov-minor-mode' is activated or deactivated."
  :type 'hook
  :group 'persistent-overlays
  :version "24.5")

;; ===== variables ============================================================

(defvar pov-minor-mode nil
  "t indicates that this mode is active. Use the command
`pov-minor-mode' to toggle or set this variable.")

;; ===== key map ==============================================================

(defvar pov-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; default binding for explicitly saving and loading overlays
    (define-key map "\C-c*\C-s"	'pov-save-overlays)
    (define-key map "\C-c*\M-s"	'pov-savex-overlays)
    (define-key map "\C-c*\C-m"	'pov-merge-overlays)
    (define-key map "\C-c*\M-m"	'pov-mergex-overlays)
    (define-key map "\C-c*\C-l"	'pov-load-overlays)
    (define-key map "\C-c*\M-l"	'pov-loadx-overlays)
    map)
  "Keymap for persistent-overlays minor mode.")

;; ===== internal functions ===================================================

(defun pov-read-overlays (&optional file)
  "This is an internal function which reads overlays from a file
into a temporary buffer which is used to loading and merging."
  (let ((tbuf (generate-new-buffer "**persistent-overlays**")))
    (if (file-exists-p file)
	(save-excursion
	  ;; only execute the expected code
	  (set-buffer tbuf)
	  (insert-file-contents file)
	  (keep-lines "(let ((tovly (make-overlay [[:digit:]]+ [[:digit:]]+)) (tplist '(.*?))) (while tplist (let ((tp (car tplist)) (tpv (cadr tplist))) (overlay-put tovly tp tpv)) (setq tplist (cddr tplist))))")
	  )
      (when (char-or-string-p ovfile) (setq tbuf nil)))
    tbuf))

(defun pov-remove-overlays (&optional start end name value)
  "This is an internal function which wraps functionality around
remove-overlays so that VALUE can be any value. This is
designated by setting VALUE to symbol 'ANY."
  (if (eq value 'ANY)
      (progn
	(let ((ovlys (overlays-in start end)))
	  (while ovlys
	    (let ((ovly (car ovlys)))
	      (when (overlay-get ovly name)
		(delete-overlay ovly)
		))
	    (setq ovlys (cdr ovlys)))))
    (remove-overlays start end name value)))

(defun pov-get-existing-overlays ()
  "This is an internal function which reads the existing overlays
into a temporary buffer for use when saving or merging overlays."
  (let ((tbuf (generate-new-buffer "**persistent-overlays**")))
    (save-excursion
      (princ (concat ";; " (buffer-file-name) "\n;; " (current-time-string) "\n") tbuf)
      (let ((ovlys (overlays-in (point-min) (point-max))))
	(while ovlys
	  (let ((ovly (car ovlys)))
	    (when (overlay-get ovly pov-property-name)
	      (setq oprops (overlay-properties ovly))
	      (princ (format "(let ((tovly (make-overlay %d %d)) (tplist '%s)) (while tplist (let ((tp (car tplist)) (tpv (cadr tplist))) (overlay-put tovly tp tpv)) (setq tplist (cddr tplist))))\n" (overlay-start ovly) (overlay-end ovly) oprops) tbuf)
	      ))
	  (setq ovlys (cdr ovlys)))
	(set-buffer tbuf)
	(goto-char (point-min))
	))
    tbuf))

(defun pov-delete-duplicate-lines (buf)
  "This is an internal function to remove duplicate lines from a buffer.
It is only used if `delete-duplicate-lines' is not available. It
is used to ensure that dupliate overlays are not created when
merging overlays."
  (save-excursion
    (set-buffer buf)
    (let ((end (copy-marker (point-max))))
      (while
	  (progn
	    (goto-char (point-min))
	    (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
	(replace-match "\\1\n\\2")))))

(defun pov-get-ovly-fname (full fname)
  "Internal function that generates the file name for overlay files."
  (let ((name (if pov-use-path-name (replace-regexp-in-string "[:/\\]" "_" full) (concat fname "-" (secure-hash 'sha1 full) ".pov")))
	(dir (if pov-store-with-user-file (replace-regexp-in-string "[:/\\][^/\\]*$" "" full) pov-directory)))
    (when pov-store-as-hidden (setq name (concat "." name)))
    (concat dir "/" name)))

(defun pov-disable ()
  "Internal function to disable pov-minor-mode on major mode changes."
  (pov-minor-mode -1))

;; ===== merge functions ======================================================

(defun pov-mergex-overlays (&optional ovfile)
  "OVFILE must be explicity provided. The overlay file OVFILE
specifies the path to the overlay file to be loaded and merged.

NOTE: Loaded overlays are merged with existing overlays. See
`pov-load-overlays' and `pov-loadx-overlays' if you want to load
overlays without merging.
"
  (interactive "f")
  (when (pov-merge-overlays ovfile)
      (message "Overlays were loaded and merged from %s" ovfile)))

(defun pov-merge-overlays (&optional ovfile)
  "If OVFILE is not provided, the overlay file corresponding to
the current buffer is loaded from `pov-directory' and merged with
existing overlays. If OVFILE is provided, it specifies the path to
the overlay file to be merged.

NOTE: If a loaded overlay has exactly the same properties and
values as an existing overlay only a single overlay is retained.
"
  (interactive)
  (when (buffer-file-name)
    (let ((file (if (char-or-string-p ovfile) ovfile (pov-get-ovly-fname (buffer-file-name) (buffer-name))))
	  (merged nil))
      (with-temp-message (format "Merging in overlays from %s..." (buffer-name))
	(setq curbuf (buffer-name))
	(save-excursion
	  (setq tbuf (pov-read-overlays file))
	  (if tbuf
	      (progn
		(setq xbuf (pov-get-existing-overlays))
		(set-buffer xbuf)
		(append-to-buffer tbuf (point-min) (point-max))
		(set-buffer tbuf)
		(when (fboundp 'delete-duplicate-lines) (delete-duplicate-lines (point-min) (point-max)) (pov-delete-duplicate-lines tbuf))
		(set-buffer curbuf)
		(pov-remove-overlays (point-min) (point-max) pov-property-name 'ANY)
		(eval-buffer tbuf)
		(kill-buffer tbuf)
		(kill-buffer xbuf)
		(setq loaded t))
	    (when (char-or-string-p ovfile) (message "File %s does not exist." ovfile)))))
      merged)))
  

;; ===== load functions =======================================================

(defun pov-loadx-overlays (&optional ovfile)
  "OVFILE must be explicity provided. The overlay file OVFILE
specifies the path to the overlay file to be loaded.

NOTE: All overlays that contain property `pov-property-name' are
removed before loading overlays. Loaded overlays are not merged
with existing overlays. See `pov-merge-overlays' and
`pov-mergex-overlays' if you want to merge overlays.
"
  (interactive "f")
  (when (pov-load-overlays ovfile)
      (message "Overlays were loaded from %s" ovfile)))

(defun pov-load-overlays (&optional ovfile)
  "If OVFILE is not provided the overlay file corresponding to
the current buffer is loaded from `pov-directory'. If OVFILE is
provided it specifies the path to the overlay file to be loaded.

NOTE: All overlays that contain property `pov-property-name' are
removed before loading overlays. Loaded overlays are not merged
with existing overlays. A future version may provide an
`pov-merge-overlays' function.
"
  (interactive)
  (when (buffer-file-name)
    (let ((file (if (char-or-string-p ovfile) ovfile (pov-get-ovly-fname (buffer-file-name) (buffer-name))))
	  (loaded nil))
      (with-temp-message (format "Loading overlays for %s..." (buffer-name))
	(setq tbuf (pov-read-overlays file))
	(if tbuf
	    (progn
	      (pov-remove-overlays (point-min) (point-max) pov-property-name 'ANY)
	      (eval-buffer tbuf)
	      (kill-buffer tbuf)
	      (setq loaded t))
	  (when (char-or-string-p ovfile) (message "File %s does not exist." ovfile))))
      loaded)))

;; ===== save functions =======================================================

(defun pov-savex-overlays (&optional ovfile)
  "OVFILE must be explicity provided. The overlay file
corresponding to the current buffer is saved in OVFILE. Usually,
`pov-save-overlays' should be used instead.

NOTE: Only overlays that contain property `pov-property-name' are
saved. If the overlay already exists it is overwritten.
"
  (interactive "F")
  (when (pov-save-overlays ovfile)
      (message "Overlays were saved to %s" ovfile)))

(defun pov-save-overlays (&optional ovfile)
  "If OVFILE is not provided, the overlay file corresponding to
the current buffer is saved in the directory pointed to by
`pov-directory'. If OVFILE is provided, it specifies the path of
the overlay file to be saved.

NOTE: Only overlays that contain property `pov-property-name' are
saved. If the overlay already exists it is overwritten.
"
  (interactive)
  (let ((file (if (char-or-string-p ovfile) ovfile (pov-get-ovly-fname (buffer-file-name) (buffer-name))))
	(saved nil))
    (with-temp-message (format "Saving overlays for %s..." (buffer-name))
      (save-excursion
	(setq tbuf (pov-get-existing-overlays))
	(set-buffer tbuf)
	(write-region (point-min) (point-max) file nil 'quietly)
	(kill-buffer tbuf))
      (setq saved t))
    saved))

;; ===== Define the mode ======================================================

(define-minor-mode pov-minor-mode
  "This is a minor mode to make overlays persistent by saving
them to a file and subsequently loading them. By default overlays
are stored in ~/.emacs-pov with a file name that is created as a
combination of the buffer name and a SHA1 hash of the file name
to which they correspond.  `pov-directory' may be set to indicate
a different directory in which to store overlays. If
`pov-use-path-name' is set to t, the overlay file is named using
the underscore delimited full path of the file instead of using a
SHA1 hash. Overlay file names always end with .pov.

`pov-property-name' specifics a property symbol that must exist
in an overlay for it to be handled by this mode. By default it is
set to the symbol 'invisible so that overlays which hide sections
of a buffer are stored. This was the primary motivation for this
mode.

If `pov-auto-save' is t all overlays containing
`pov-property-name' will be saved in the directory indicated by
`pov-directory' when the buffer is saved.

If `pov-auto-load' is t existing overlays for the buffer will be
loaded from `pov-directory' when the buffer is loaded or when
`pov-minor-mode' is enabled.

If `pov-auto-merge' is t existing overlays for the buffer will be
loaded and merged from `pov-directory' when the buffer is loaded
and when `pov-minor-mode' is enabled. This is the default
behavior which is useful when pov-minor-mode is enabled after
having created additional overlays.

Other custom variables are `pov-store-with-user-file',
`pov-store-as-hidden' and `pov-disable-on-major-mode-change'.
Please use describe-variable for details on those variables.

NOTE: There is currently no mechanism for binding overlay files
with the corresponding user files when files are moved. If you
wish to retain existing stored overlays you can manually load or
merge the overlays from the original overlay file after renaming
the user file. You may also want to delete the original overlay
file after merging to avoid having unused overlay files lying
around. Each overlay file contains the full path name of the file
to which it applies as its first line.

Key bindings:
\\{pov-minor-mode-map}"
  :group 'persistent-overlays
  :lighter " pov"
  :keymap pov-minor-mode-map
  (setq pov-headline nil)
  (if pov-minor-mode 
      (progn ; pov-minor-mode was just enabled
	(format-mode-line mode-name)
        (when pov-disable-on-major-mode-change (add-hook 'change-major-mode-hook (pov-disable) nil t))
	(when pov-auto-save (add-hook 'after-save-hook 'pov-save-overlays nil t))
	(if pov-auto-merge (pov-merge-overlays) (when (pov-auto-load (pov-load-overlays))))
	)
    ( ; pov-minor-mode was just disabled
    (remove-hook 'after-save-hook 'pov-save-overlays t)
    (remove-hook 'change-major-mode-hook 'pov-disable t))
    ))

(provide 'persistent-overlays)

;; ============================================================================
;; ============================================================================
