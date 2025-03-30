# bufferfile.el - Delete or rename buffer file names with their associated buffers
![Build Status](https://github.com/jamescherti/bufferfile.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/bufferfile.el)
![](https://raw.githubusercontent.com/jamescherti/bufferfile.el/main/.images/made-for-gnu-emacs.svg)

This package provides helper functions to delete and rename buffer files:
- `bufferwizard-rename-file`: Renames the file that the current buffer is visiting. This command renames the file name on disk, adjusts the buffer name, and updates any indirect buffers or other buffers associated with the old file.
- `bufferwizard-delete-file`: Delete the file associated with a buffer and kill all buffers visiting the file, including indirect buffers.

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

### Making bufferwizard use version control (VC), such as Git, when renaming or deleting files?

To make *bufferwizard* use version control (VC) when renaming or deleting files, you can set the variable `bufferfile-use-vc` to `t`. This ensures that file operations within *bufferwizard* interact with the version control system, preserving history and tracking changes properly.

``` emacs-lisp
(setq bufferfile-use-vc t)
```

### Usage

### Hook functions

The *bufferwizard* package provides customizable hook variables that allow users to execute functions before and after renaming or deleting files. These hooks can be used to integrate additional logic, such as logging, or updating dependent buffers.

#### Hooks for Renaming Files

- **`bufferfile-pre-rename-functions`**
  A list of functions executed before renaming a file.
  Each function receives three arguments:
  - `list-buffers`: The list of buffers associated with the file.
  - `previous-path`: The original file path.
  - `new-path`: The new file path.

- **`bufferfile-post-rename-functions`**
  A list of functions executed after a file has been renamed.
  Each function receives the same three arguments as `bufferfile-pre-rename-functions`.

#### Hooks for Deleting Files

- **`bufferfile-pre-delete-functions`**
  A list of functions executed before a file is deleted.
  Each function receives two arguments:
  - `list-buffers`: The list of buffers associated with the file.
  - `path`: The file path to be deleted.

- **`bufferfile-post-delete-functions`**
  A list of functions executed after a file has been deleted.
  Each function receives the same two arguments as `bufferfile-pre-delete-functions`.

## Frequently asked questions

### What is the difference between bufferfile and the built-in C-x C-j R and C-x C-j D?

The `C-x C-j R` and `C-x C-j D` key bindings do not support renaming associated buffers, including indirect buffers (clones).

This limitation of the built-in functions was one of the motivations for developing the *bufferfile* package, which extends support for renaming and deleting buffers, including indirect buffers.

## Author and License

The *bufferfile* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [bufferfile.el @GitHub](https://github.com/jamescherti/bufferfile.el)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
