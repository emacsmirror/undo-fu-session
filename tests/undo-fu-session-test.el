;;; undo-fu-session-test.el --- Undo-fu session test -*- lexical-binding: t -*-

;; Copyright (C) 2020  Campbell Barton
;; Copyright (C) 2009-2015  Tomohiro Matsuyama

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://gitlab.com/ideasman42/emacs-undo-fu-session
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; This is a test for `undo-fu-session'.
;;

;;; Usage

;;
;; To test this file run:
;;
;;     `emacs -batch -l tests/undo-fu-session-test.el -f undo-fu-session-test-run-all'
;;

;;; Code:

;; ---------------------------------------------------------------------------
;; Setup Environment

(add-to-list 'load-path (concat (file-name-directory load-file-name) ".."))
(require 'undo-fu-session)


;; ---------------------------------------------------------------------------
;; Internal Macros

(defmacro undo-fu-session-test--with-temp-dir (temp-dir &rest body)
  "Run BODY with TEMP-DIR directory."
  `
  (let ((,temp-dir (make-temp-file "" t)))
    (unwind-protect
      (progn
        ,@body)
      (delete-directory ,temp-dir t))))


;; ---------------------------------------------------------------------------
;; Tests

(defun undo-fu-session-test-run-all-impl ()
  "Run each test and exit."

  (undo-fu-session-test--with-temp-dir
    ;; Don't touch the users home directory.
    undo-fu-session-directory

    (dotimes (f 100)
      (let*
        ( ;; While the session file wouldn't typically
          ;; be in the same directory as the undo session data, it's harmless.
          (filename (concat undo-fu-session-directory "/undo-fu-session-test"))
          (filename-session (undo-fu-session--make-file-name filename)))
        (when (file-exists-p filename)
          (delete-file filename))
        (when (file-exists-p filename-session)
          (delete-file filename-session))
        (with-current-buffer (find-file-literally filename)
          (dotimes (_i 1000)
            (ignore-errors
              (pcase (random 3)
                (`0
                  (dotimes (_j 10)
                    (insert (make-string (1+ (random 20)) (+ (random 26) 65)))))
                (`1 (newline))
                (`2 (insert "\t"))
                (`3 (forward-line))
                (`4 (forward-line -1))
                (`5 (kill-line))
                (`6 (kill-paragraph -1))
                (`7 (yank))
                (`8
                  (kill-region
                    (+ (point-min) (random (point-max)))
                    (+ (point-min) (random (point-max))))))))
          (save-buffer)
          (undo-fu-session-save)
          (kill-buffer (current-buffer)))
        (with-current-buffer (find-file-literally filename)
          (undo-fu-session-recover)
          (ignore-errors
            (while
              (prog1 t
                (undo))))
          (let ((contents (buffer-string)))
            (set-buffer-modified-p nil)
            (kill-buffer (current-buffer))
            (cond
              ((string-equal contents "")
                (message "Test succeeded #%s" f))
              (t
                (error "Test failed #%s" f))))))))

  (message "Done"))

(defun undo-fu-session-test-run-all ()
  "Run every test."

  (global-undo-fu-session-mode)

  (message "Running with `undo-fu-session-linear' enabled:")
  (setq undo-fu-session-linear t)
  (undo-fu-session-test-run-all-impl)

  (message "Running with `undo-fu-session-linear' disabled:")
  (setq undo-fu-session-linear nil)
  (undo-fu-session-test-run-all-impl))

(provide 'undo-fu-session-test)
;;; undo-fu-session-test.el ends here
