;; To test this file run:
;; emacs --eval '(progn (add-to-list \'load-path ".") (load "undo-fu-session.el") (load "undo-fu-session-test.el"))' --batch

(global-undo-fu-session-mode)

(defmacro with-temp-dir (temp-dir &rest body)
  `
  (let ((,temp-dir (make-temp-file "" t)))
    (unwind-protect
      (progn
        ,@body)
      (delete-directory ,temp-dir t))))

(with-temp-dir
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
            (cl-case
              (random 3)
              (0
                (dotimes (_j 10)
                  (insert (make-string (1+ (random 20)) (+ (random 26) 65)))))
              (1 (newline))
              (2 (insert "\t"))
              (3 (forward-line))
              (4 (forward-line -1))
              (5 (kill-line))
              (6 (kill-paragraph -1))
              (7 (yank))
              (8
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
          (if (string-equal contents "")
            (message "Test succeeded #%s" f)
            (error "Test failed #%s" f)))))))

(message "Done")
