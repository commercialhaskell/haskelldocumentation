---
title: IDEs and Linters
author: Kostiantyn Rybnikov <k-bx@k-bx.com>
description: Preparing your editor
first-written: 2015-03-05
last-updated: 2015-03-05
---

Incomplete. Need to add:

* Emacs:
  * ghc-mod
  * flycheck
  * tags-search (hasktags)
  * haskell-mode + hlint
  * stylish-haskell
  * Additional:
    * multi-tags search
    * projectile for projects
* Vim: pretty much everything
* fpcomplete IDE
* other IDEs not listed here

## Emacs

Emacs is one of most popular Haskell editors out there. If you are
able to use it, you can get a very nice development environment.

### Prerequisites

#### Cask

It's assumed you have modern package manager Cask installed. You can
install it by following the
[cask user guide](http://cask.readthedocs.org/en/latest/index.html). For
older Emacs versions (< 24), please figure out other method of
package-installation.

Make sure a `Cask` file in your `.emacs.d` directory has marmelade
repo, ensure it looks something like this:

```elisp
(source gnu)
(source melpa)
(source marmalade)
```

And check that your `~/.emacs` file has:

```elisp
(require 'cask "~/.cask/cask.el")
(cask-initialize)
```

#### exec-path-from-shell

On MacOS X you will also need
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
in order to get your environment variables from your shell (bash or
zsh) into your emacs, so that it inherits your `$PATH` and sees proper
`~/.cabal/bin/cabal`, for example. To install, add following to your
`Cask` file:

```elisp
(depends-on "exec-path-from-shell")
```

and run `cask install` from `Cask`-file directory. Add following to your `~/.emacs`:

```elisp
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
```

### haskell-mode

First of all, you should use
[haskell-mode](https://github.com/haskell/haskell-mode) for editing
`.hs` and `.cabal` files. Add this line to your `Cask`:

```elisp
(depends-on "haskell-mode")
```

and run `cask install` in that file's directory. This should install a
fresh `haskell-mode` for you. Restart your emacs and you should be
ready to open any `.cabal` or `.hs` file for editing. You will see
syntax highlighted, you'll be able to run commands like `M-x
haskell-process-load-file`.

However, you can't yet use `haskell-mode` in full because you haven't
configured indentation mode and interaction. Go read
[haskell-mode tutorial on setting the indentation-mode you like](https://github.com/haskell/haskell-mode/wiki/Indentation)
and
[Haskell Interactive Mode Setup](https://github.com/haskell/haskell-mode/wiki/Haskell-Interactive-Mode-Setup)
pages, or just put something like this to your `.emacs` (if you
already have `custom-set-variables` in your `.emacs` -- extend it):

```elisp
(defun turn-on-subword-mode ()
  (interactive)
  (subword-mode 1))
(defun my-haskell-mode-hook ()
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space))
(custom-set-variables
 '(haskell-mode-hook
   (quote
    (interactive-haskell-mode 
     turn-on-haskell-indent 
     turn-on-subword-mode
     turn-on-haskell-decl-scan 
     my-haskell-mode-hook)))
 '(haskell-process-type 'cabal-repl))
```

Now you're more or less ready, but this is just a beginning. Go ahead,
open some `.hs` file and press `C-h ? b`. You will see list of
bindings available. Search for `haskell-` to see all haskell-related
bindings. Also, run `M-x haskell-` and press `TAB` for list of
available commands starting with `haskell-`. There's plenty of them!

Press `C-c C-l` to open a `cabal repl` shell with current module
loaded. You can easily play with your code like this. Also, pressing
`C-c C-i` will inspect symbol under cursor and show you some info. Neat!
