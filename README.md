# context.el

![Screenshot of Emacs frame with context.el enabled](https://user-images.githubusercontent.com/14259816/207393093-bef655ad-172c-48b3-aca5-cf20ee3a5894.png)

This package lets you show the context of blocks like function
definitions, loops, conditions etc. It does this by adding the
first line of these blocks at the end of the last char of that
block. It makes use of `tree-sitter' to figure out block start and
end.

The package exposes a single minor mode `context-mode' which you
can use to enable or disable the functionality.

Here is a sample `use-package' configuration:

```emacs-lisp
(use-package context
  :after tree-sitter
  :config (add-hook 'tree-sitter-mode-hook #'context-mode))
```

## Configuration

- `context-overlay-prefix`: Change this to another string to change
  the prefix. Defaults to <code>&nbsp;&nbsp;#&nbsp;</code>.
- `context-face`: This is the face applied for the context overlay,
  change this to change how it appears. Defaults to whatever comments
  are highlighted with.
- `context-targets`: Update this alist which maps `major-mode` to list
  of tree-sitter entries to control what all blocks get context
  overlays


*Thanks to [haringsrob/nvim_context_vt](https://github.com/haringsrob/nvim_context_vt) for the idea.*
