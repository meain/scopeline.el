# scopeline.el

![Screenshot of Emacs frame with scopeline.el enabled](https://user-images.githubusercontent.com/14259816/208631769-052ac0ab-44df-4949-8f2f-3ef43e249f65.png)

This package lets you show the scope info of blocks like function
definitions, loops, conditions etc. It does this by adding the first
line of these blocks at the end of the last char of that block. In the
screenshot you can see the light grey things after the closing
brackets `}`. It makes use of `tree-sitter' to figure out block start
and end.

The package exposes a single minor mode `scopeline-mode' which you
can use to enable or disable the functionality.

Here is a sample `use-package' configuration:

```emacs-lisp
(use-package scopeline
  :after tree-sitter
  :config (add-hook 'tree-sitter-mode-hook #'scopeline-mode))
```

## Configuration

- `scopeline-overlay-prefix`: Change this to another string to change
  the prefix. Defaults to <code>&nbsp;&nbsp;¤&nbsp;</code>.
- `scopeline-face`: This is the face applied for the scopeline
  overlay, change this to change how it appears. Defaults to whatever
  comments are highlighted with.
- `scopeline-targets`: Update this alist which maps `major-mode` to
  list of tree-sitter entries to control what all blocks get scopeline
  overlays
- `scopeline-min-lines`: Minimum number of lines before we start
  showing scopeline information. Default is set to 5. Set it to 0 to
  always show.


*Thanks to [haringsrob/nvim_context_vt](https://github.com/haringsrob/nvim_context_vt) for the idea.*
