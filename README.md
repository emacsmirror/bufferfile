# bufferfile.el
![Build Status](https://github.com/jamescherti/bufferfile.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/bufferfile.el)
![](https://raw.githubusercontent.com/jamescherti/bufferfile.el/main/.images/made-for-gnu-emacs.svg)

A suite of helper functions to delete and rename buffer files.

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

## Author and License

The *bufferfile* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [bufferfile.el @GitHub](https://github.com/jamescherti/bufferfile.el)
