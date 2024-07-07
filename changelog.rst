
##########
Change Log
##########

- Version 0.7 (2024-07-07)

  - Support custom file-system locations for undo data via ``undo-fu-session-make-file-name-function``.
  - Support derived major-modes when checking ``undo-fu-session-incompatible-major-modes``.
  - Add ``undo-fu-session-temp-directories`` so additional directories can be considered temporary.

- Version 0.6 (2023-05-20)

  - Add ``undo-fu-session-ignore-temp-files`` to ignore files in temporary directories.
  - The ``undo-fu-session-incompatible-files`` match is now case insensitive for case insensitive file-systems.

- Version 0.5 (2023-04-05)

  - Fix bug when ``undo-fu-session-linear`` was disabled,
    saving when all undo steps were undone would cause an error & fail to save undo data.

- Version 0.4 (2023-01-07)

  - Rename ``global-undo-fu-session-mode`` to ``undo-fu-session-global-mode``.

- Version 0.3 (2022-07-08)

  - Support conversion of existing undo data to other formats via ``undo-fu-session-compression-update``.
  - Support different compression formats (``bzip2``, ``gzip``, ``xz`` & ``zstd``)
  - Set permissions on the directory only only be readable by the owner (to avoid potential security issues).
  - Disable ``global-undo-fu-session`` in ``special-mode`` and it's derived modes (such as ``package-menu-mode``).
  - Add ``undo-fu-session-ignore-encrypted-files`` to ignore encrypted files.
  - Fix bug when ``undo-fu-session-linear`` was enabled,
    causing an empty undo history to disable undo entirely on reload.

- Version 0.2 (2020-05-17)

  - Add ``undo-fu-session-linear`` to optionally write linear undo history.
  - Fix ``global-undo-fu-session`` activating with the mini-buffer.
  - Fix loading the undo session clearing the ``undo-equiv-table`` for other buffers.

- Version 0.1 (2020-01-27)

  Initial release.
