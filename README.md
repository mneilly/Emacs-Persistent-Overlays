# Emacs-Persistent-Overlays
This is an Emacs mode that allows you to store overlays between sessions. This is useful for storing overlays with the invisible property in hideshow and outline modes.

## About

The goal for persistent-overlays is to maintain persistent overlays
between emacs sessions. It has been tested with hideshow and
outline modes on Linux, Mac OS X and Windows. However, this version
should be considered beta software and it has not been previously
released.

Overlays are stored in ~/.emacs-persistent-overlays by default. There
are several customizable variables which allow a user to change the
file naming convention and the storage location of the overlay files.

Please use describe-mode for a full description.

To enable this mode add the following to your ~/.emacs or
~/emacs.d/init.el file. This assumes that you have placed
persistent-overlays.el somewhere in your load-path.

    (load-library "persistent-overlays")

FWIW - this is what I use in my .emacs:

    ;; minor modes on by default for all programming modes
    (add-to-list 'load-path "~/commonenv/emacs/Emacs-Persistent-Overlays/")
    (load-library "persistent-overlays")

    (when (not (< emacs-major-version 24))
      (add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1) (setq hs-allow-nesting t) (persistent-overlays-minor-mode 1))))

    (when (not (> emacs-major-version 23))
      (add-hook 'c-mode-hook (lambda () (hs-minor-mode 1) (setq hs-allow-nesting t) (persistent-overlays-minor-mode 1)))
      (add-hook 'c++-mode-hook (lambda () (hs-minor-mode 1) (setq hs-allow-nesting t) (persistent-overlays-minor-mode 1)))
      (add-hook 'emacs-lisp-mode-hook (lambda () (hs-minor-mode 1) (setq hs-allow-nesting t) (persistent-overlays-minor-mode 1)))
    )

Enjoy

## Full Description

This is a minor mode to make overlays persistent by saving
them to a file and subsequently loading them. By default overlays
are stored in ~/.emacs-persistent-overlays with a file name that
is created as a combination of the buffer name and a SHA1 hash of
the file name to which they correspond. **NOTE**: you need to create
the directory ~/.emacs-persistent-overlays.

*persistent-overlays-directory* may be set to indicate a
different directory in which to store overlays. If
*persistent-overlays-use-path-name* is set to *t*, the overlay file
is named using the underscore delimited full path of the file
instead of using a SHA1 hash. Overlay file names always end with
*.povly*.

*persistent-overlays-property-names* specifies a list of property
symbols that must exist in an overlay for it to be handled by
this mode. By default it is set to a list containing the symbol
*'invisible* so that overlays which hide sections of a buffer are
stored. This was the primary motivation for this mode.

If *persistent-overlays-auto-save* is *t* all overlays containing
properties in *persistent-overlays-property-names* will be saved
in the directory indicated by *persistent-overlays-directory*
when the buffer is saved.

If *persistent-overlays-auto-load* is *t* existing overlays for the
buffer will be loaded from *persistent-overlays-directory* when
the buffer is loaded or when *persistent-overlays-minor-mode* is
enabled.

If *persistent-overlays-auto-merge* is *t* existing overlays for
the buffer will be loaded and merged from
*persistent-overlays-directory* when the buffer is loaded and
when *persistent-overlays-minor-mode* is enabled. This is the
default behavior which is useful when
*persistent-overlays-minor-mode* is enabled after having created
additional overlays.

Other custom variables are
*persistent-overlays-store-with-user-file*,
*persistent-overlays-store-as-hidden* and
*persistent-overlays-disable-on-major-mode-change*.  Please use
describe-variable for details on those variables.

## Potential Issues

**NOTE: There is currently no mechanism for binding overlay files
with the corresponding user files when files are moved. If you
wish to retain existing stored overlays you can manually load or
merge the overlays from the original overlay file after renaming
the user file. You may also want to delete the original overlay
file after merging to avoid having unused overlay files lying
around. Each overlay file contains the full path name of the file
to which it applies as its first line.**

If you edit a file outside of Emacs, any stored overlays are potentially
invalid due to repositioning of the text. For example, performing a *git checkout*
of a file to restore the master version over an edited version. In this scenario,
you may need to expand all hidden overlays and save to overwrite the existing 
overlays (or exit Emacs, delete the overlay file and restart Emacs).

 [![Analytics](https://ga-beacon.appspot.com/UA-63342536-2/Emacs-Persistent-Overlays)](https://github.com/mneilly/Emacs-Persistent-Overlays)
 
