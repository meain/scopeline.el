;;; context.el --- Show context info of blocks in buffer -*- lexical-binding: t; -*-

;; URL: https://github.com/meain/context.el
;; Keywords: context, tree-sitter, convenience
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "25.1") (tree-sitter "0.15.0"))
;; Version: 0.1

;;; Commentary:
;; This package lets you show the context of blocks like function
;; definitions, loops, conditions etc.  It does this by adding the
;; first line of these blocks at the end of the last char of that
;; block.  It makes use of `tree-sitter' to figure out block start and
;; end.
;;
;; The package exposes a single minor mode `context-mode' which you
;; can use to enable or disable the functionality.
;;
;; Here is a sample `use-package' configuration
;;
;; (use-package context
;;   :after tree-sitter
;;   :config (add-hook 'tree-sitter-mode-hook #'context-mode))
;;
;; You can find more info in the README for the project at
;; https://github.com/meain/context.el

;;; Code:

(defvar context--overlays '())
(defvar context-overlay-prefix "  # ")
(defface context-face
  '((default :inherit font-lock-comment-face))
  "Face for showing context info.")
(defvar context-targets ;; TODO: Add more language modes
  '((go-mode . ("function_declaration" "func_literal" "if_statement" "for_statement"))
    (rust-mode . ("function_item" "for_expression" "if_expression"))))

(defun context--add-overlay (pos text)
  "Add overlay at `POS' with the specified `TEXT'."
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
  "Re-display all the context entries."
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
