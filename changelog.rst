
##########
Change Log
##########

- In development

  - Add ``undo-fu-session-ignore-encrypted-files`` to ignore encrypted files.
  - Fix bug when ``undo-fu-session-linear`` was enabled,
    causing an empty undo history to disable undo entirely on reload.

- Version 0.2 (2020-05-17)

  - Add ``undo-fu-session-linear`` to optionally write linear undo history.
  - Fix ``global-undo-fu-session`` activating with the mini-buffer.
  - Fix loading the undo session clearing the ``undo-equiv-table`` for other buffers.

- Version 0.1 (2020-01-27)

  Initial release.
