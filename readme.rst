###############
Undo Fu Session
###############

Save & recover undo steps between Emacs sessions.

This package writes undo/redo information upon file save which is restored
where possible when the file is loaded again.

The following features are supported:

- Store full undo/redo history, allowing ``undo-only`` to work as expected.
- Uses a minor-mode which can be enabled globally or buffer-local.
- Option to store compressed.
- Option to limit number of undo-session files stored
  *(avoid adding files indefinitely until the user manually removes them).*
- Fast early-exit on restore
  *(only decode undo data if buffer length & checksum match).*

Available via `melpa <https://melpa.org/#/undo-fu-session>`__.


Motivation
==========

To have a stand-alone package that stores Emacs built-in undo information for re-use,
without imposing any changes to undo functionality.

While this package is intended for use with `undo-fu <https://codeberg.org/ideasman42/emacs-undo-fu>`__,
there are no inter-dependencies.


Usage
=====

Typically this package is enabled globally,
afterwards it runs automatically when saving & loading files.


Enable (Global)
---------------

Enabling globally is most straightforward, you can then
disable storing undo information by mode and file-name.

.. code-block:: elisp

   (undo-fu-session-global-mode)


Enable (Buffer)
---------------

If you want to isolate this functionality,
you can enable this using a mode hook for e.g.

.. code-block:: elisp

   ;; Only use undo-fu-session for org mode.
   (add-hook 'org-mode-hook #'undo-fu-session-mode)


Customization
-------------

``undo-fu-session-directory``: ``"undo-fu-session"`` in emacs user directory
   The location of stored files.

``undo-fu-session-linear``: ``nil``
   Write linear undo history, omitting branches which were themselves undone.

   Note that this only writes undo steps which would be used by ``undo-only``.
``undo-fu-session-compression``: ``'gz``
   Compression setting for stored undo data.

   Valid options:

   - ``'bz2`` BZip2.
   - ``'gz`` ZLib.
   - ``'xz`` XZ.
   - ``'zst`` Z-standard.
   - ``nil`` (no compression).

   After changing, run ``undo-fu-session-compression-update`` to convert existing files to the newly selected format.

``undo-fu-session-file-limit``: ``nil``
   Number of files to store, nil to disable limiting entirely.

   Enforcing removes the oldest files.


Customization (Exclusions)
--------------------------

There are various methods of excluding undo-session storage,
this may be done to prevent storing sensitive information
or to prevent unnecessary storage of undo-session for temporary files (such as commit messages).

These exclusion options allow ``undo-fu-session-global-mode`` to be enabled.
With exceptions added for cases where undo-session should not be stored.

``undo-fu-session-incompatible-files`` (``'()``)
   List of regexps or functions for matching file names to ignore saving/recovering undo session.
``undo-fu-session-incompatible-major-modes`` (``nil``)
   List of major-modes in which saving undo session should not be performed.

   This matches derived modes so you may include a mode that other modes are derived from
   to avoid having to explicitly list all modes.
``undo-fu-session-ignore-encrypted-files`` (``t``)
   Ignore saving/recovering undo session for encrypted files (matching ``epa-file-name-regexp``).
``undo-fu-session-ignore-temp-files`` (``t``)
   Ignore saving/recovering undo session for files under
   ``temporary-file-directory`` & ``undo-fu-session-temp-directories``.
``undo-fu-session-temp-directories`` (``'("/tmp" "/dev/shm")``)
   Directories to consider temporary when ``undo-fu-session-ignore-temp-files`` is enabled.


Customization (Data Storage)
----------------------------

This is advanced functionality for users who prefer to define their own method
of defining the file-system location of undo-fu-session data.

``undo-fu-session-make-file-name-function``: ``'undo-fu-session-make-file-name``
   This function takes two arguments ``(filepath extension)`` and must return
   a the path where undo data is stored, ending with ``extension``.

If you wish to store undo data next in the same directory as the file being edited
this example shows how it can be achieved.

Take care, if you define paths outside of ``undo-fu-session-directory`` the following functionality

- ``undo-fu-session-file-limit`` isn't applied.
- ``undo-fu-session-compression-update`` won't update compression on existing undo data.

This example function stores undo data for ``~/file.txt`` as ``~/.#file#.undo-fu-session.gz``.

.. code-block:: elisp

   (setq undo-fu-session-make-file-name-function
         (lambda (filepath extension)
           (let* ((filename (file-name-nondirectory filepath))
                  (dirpath (substring filepath 0 (- (length filepath) (length filename)))))
             (concat dirpath ".#" filename "#.undo-fu-session" extension))))


Details
=======

- Undo session information is stored in a directory,
  with a corresponding file for each undo session.
- Each file stores a length and checksum which is validated
  before restoring the undo-session.
- A mismatch with the file length or checksum will skip loading the undo session
  with a message.


Installation
============

The package is available in melpa as ``undo-fu-session``, here is an example with ``use-package``:

.. code-block:: elisp

   (use-package undo-fu-session
     :config
     (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

   (undo-fu-session-global-mode)


Undo Tree
---------

``undo-tree`` defines it's own undo data-structures and is not compatible with ``undo-fu-session``.

These packages cannot be used together.


Other Packages
==============

As there are multiple packages which deal with undo, it's worth mentioning how this interacts with other packages.

`Undo Fu <https://codeberg.org/ideasman42/emacs-undo-fu>`__
   This package is intended for use with undo-fu-session,
   as a convenience wrapper over Emacs built-in undo.

`Undohist <https://github.com/emacsorphanage/undohist>`__
   This package also stores undo data between sessions,
   however it doesn't store redo data making undo-only and redo-only
   operations fail to skip branches of undo history.

`Vundo <https://github.com/casouri/vundo>`__
   This package supports navigating undo as a tree structure,
   and works with undo/redo history loaded by ``undo-fu-session``
   although there are no inter-dependencies as both packages operate on Emacs built-in undo.
