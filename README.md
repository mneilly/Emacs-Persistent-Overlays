# Emacs-Persistent-Overlays
This is an Emacs mode that allows you to store overlays between sessions. This is useful for storing overlays with the invisible property in sideshow and outline modes.

## About

The goal for persistent-overlays is to maintain persistent overlays
between emacs sessions. It has been tested with hideshow and
outline on Linux, Mac OS X and Windows. However, this version
should be considered beta software.

Overlays are stored in ~/.emacs-pov by default. There are several
customizable variables which allow a user to change the file naming
convention and the storage location of the overlay files.

Please use describe-mode for a full description.

To enable this mode add the following to your ~/.emacs or
~/emacs.d/init.el file. This assumes that you have placed
persistent-overlays.el somewhere in your load-path.

(load-library "persistent-overlays")

Enjoy

