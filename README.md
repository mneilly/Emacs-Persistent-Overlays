# Emacs-Persistent-Overlays
This is an Emacs mode that allows you to store overlays between sessions. This is useful for storing overlays with the invisible property in hideshow and outline modes.

## About

The goal for persistent-overlays is to maintain persistent overlays
between emacs sessions. It has been tested with hideshow and
outline modes on Linux, Mac OS X and Windows. However, this version
should be considered beta software and it has not been previously
released.

Overlays are stored in ~/.emacs-pov by default. There are several
customizable variables which allow a user to change the file naming
convention and the storage location of the overlay files.

Please use describe-mode for a full description.

To enable this mode add the following to your ~/.emacs or
~/emacs.d/init.el file. This assumes that you have placed
persistent-overlays.el somewhere in your load-path.

(load-library "persistent-overlays")

Enjoy

## Full Description

The following is a copy of the docstring that can be viewed using
the describe-mode command.

This is a minor mode to make overlays persistent by saving
them to a file and subsequently loading them. By default overlays
are stored in *~/.emacs-pov* with a file name that is created as a
combination of the buffer name and a SHA1 hash of the file name
to which they correspond.  **pov-directory** may be set to indicate
a different directory in which to store overlays. If
**pov-use-path-name** is set to t, the overlay file is named using
the underscore delimited full path of the file instead of using a
SHA1 hash. Overlay file names always end with .pov.

**pov-property-names** specifies a list of property symbols that
must exist in an overlay for it to be handled by this mode. By
default it is set to a list containing the symbol **invisible** so
that overlays which hide sections of a buffer are stored. This
was the primary motivation for this mode.

If **pov-auto-save** is t all overlays containing properties in
**pov-property-names** will be saved in the directory indicated by
**pov-directory** when the buffer is saved.

If **pov-auto-load** is t existing overlays for the buffer will be
loaded from **pov-directory** when the buffer is loaded or when
**pov-minor-mode** is enabled.

If **pov-auto-merge** is t existing overlays for the buffer will be
loaded and merged from **pov-directory** when the buffer is loaded
and when **pov-minor-mode** is enabled. This is the default
behavior which is useful when pov-minor-mode is enabled after
having created additional overlays.

Other custom variables are **pov-store-with-user-file**,
**pov-store-as-hidden** and **pov-disable-on-major-mode-change**.
Please use describe-variable for details on those variables.

NOTE: There is currently no mechanism for binding overlay files
with the corresponding user files when files are moved. If you
wish to retain existing stored overlays you can manually load or
merge the overlays from the original overlay file after renaming
the user file. You may also want to delete the original overlay
file after merging to avoid having unused overlay files lying
around. Each overlay file contains the full path name of the file
to which it applies as its first line.
