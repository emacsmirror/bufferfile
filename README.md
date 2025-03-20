# bufferfile.el
![Build Status](https://github.com/jamescherti/bufferfile.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/bufferfile.el)
![](https://raw.githubusercontent.com/jamescherti/bufferfile.el/main/.images/made-for-gnu-emacs.svg)

This package provides helper functions to delete and rename buffer files:
- `(bufferwizard-rename-file)`: Renames the file that the current buffer is visiting. This command renames the file name on disk, adjusts the buffer name, and updates any indirect buffers or other buffers associated with the old file.
- `(bufferwizard-delete-file)`: Delete the file associated with a buffer and kill all buffers visiting the file, including indirect buffers.

## Installation

### Install with straight (Emacs version < 30)

To install `bufferfile` with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package bufferfile
  :ensure t
  :straight (bufferfile
             :type git
             :host github
             :repo "jamescherti/bufferfile.el"))
```

### Installing with use-package and :vc (Built-in feature in Emacs version >= 30)

To install `bufferfile` with `use-package` and `:vc` (Emacs >= 30):

``` emacs-lisp
(use-package bufferfile
  :ensure t
  :vc (:url "https://github.com/jamescherti/bufferfile.el"
       :rev :newest))
```

## Customizations

### How to make bufferwizard use version control (VC), such as Git, when renaming or deleting files?

To make *bufferwizard* use version control (VC) when renaming or deleting files, you can set the variable `bufferfile-use-vc` to `t`. This ensures that file operations within *bufferwizard* interact with the version control system, preserving history and tracking changes properly.

``` emacs-lisp
(setq bufferfile-use-vc t)
```

### Hook functions

The *bufferwizard* package provides customizable hook variables that allow users to execute functions before and after renaming or deleting files. These hooks can be used to integrate additional logic, such as logging, or updating dependent buffers.

#### Hooks for Renaming Files

- **`bufferfile-before-rename-functions`**
  A list of functions executed before renaming a file.
  Each function receives three arguments:
  - `list-buffers`: The list of buffers associated with the file.
  - `previous-path`: The original file path.
  - `new-path`: The new file path.

- **`bufferfile-after-rename-functions`**
  A list of functions executed after a file has been renamed.
  Each function receives the same three arguments as `bufferfile-before-rename-functions`.

#### Hooks for Deleting Files

- **`bufferfile-before-delete-functions`**
  A list of functions executed before a file is deleted.
  Each function receives two arguments:
  - `list-buffers`: The list of buffers associated with the file.
  - `path`: The file path to be deleted.

- **`bufferfile-after-delete-functions`**
  A list of functions executed after a file has been deleted.
  Each function receives the same two arguments as `bufferfile-before-delete-functions`.

## Author and License

The *bufferfile* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [bufferfile.el @GitHub](https://github.com/jamescherti/bufferfile.el)
