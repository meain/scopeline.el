(defvar context--overlays '())
(defvar context-overlay-prefix "  # ")
(defvar context-targets '((go-mode . ("function_declaration" "func_literal" "if_statement" "for_statement"))))
(defface context-face
  '((default :inherit font-lock-comment-face))
  "Face for showing context info.")

(defun context--add-overlay (pos text)
  "Adds overlay at `POS' with the specified `TEXT'."
  (let ((ov (make-overlay pos pos)))
    (overlay-put ov 'after-string
                 (propertize (format "%s%s" context-overlay-prefix text)
                             'face 'context-face))
    ;; FIXME: If we have overlays at the same point, it does not get
    ;; added multiple times to the list but does get shown multiple
    ;; times in the buffer
    (add-to-list 'context--overlays ov)))

(defun context--delete-all-overlays ()
  "Delete all context related overlays."
  (dolist (ov context--overlays)
    (delete-overlay ov))
  (setq context--overlays '()))

(defun context--show ()
  "Show all the context items in buffer."
  (when-let* ((context-targets-for-mode (cdr (assq major-mode context-targets)))
              (query-s (string-join
                        (seq-map (lambda (ct)
                                   (format "(%s) @entity" ct))
                                 context-targets-for-mode)
                        "\n"))
              (query (tsc-make-query tree-sitter-language query-s))
              (root-node (tsc-root-node tree-sitter-tree))
              (matches (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties)))
    (seq-map (lambda (x)
               (let* ((entity (seq-elt (cdr x) 0))
                      (pos (tsc-node-byte-range (cdr entity))))
                 (context--add-overlay
                  (cdr pos)
                  (save-excursion
                    (goto-char (cl-callf byte-to-position (car pos)))
                    (s-trim (thing-at-point 'line))))))
             matches)))

(defun context--redisplay (&rest _)
  "Redisplay all the context entries."
  (if context-mode
      (progn
        (context--delete-all-overlays)
        (context--show))))

(define-minor-mode context-mode
  "Show context of first line on last line."
  :lighter " context"
  ;; FIXME: Don't always remove. This is done so as to not add the
  ;; command to hook multiple times
  (remove-hook 'post-command-hook #'context--redisplay)
  (if context-mode
      (add-hook 'post-command-hook #'context--redisplay t)
    (context--delete-all-overlays)))

(provide 'context)
;;; context.el ends here
