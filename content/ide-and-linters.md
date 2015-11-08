---
title: IDEs and Linters
author: Kostiantyn Rybnikov <k-bx@k-bx.com>
description: Preparing your editor
first-written: 2015-03-05
last-updated: 2015-03-05
---

Incomplete. Need to add:

* Emacs:
  * haskell-mode + hlint
  * stylish-haskell
  * Additional:
    * multi-tags search
    * projectile for projects
* Vim - expand, show simple usage of each without redoing the libraries' own documentation
* other IDEs not listed here

## Emacs

Emacs is one of most popular Haskell editors out there. If you are
able to use it, you can get a very nice development environment.

### Prerequisites

#### Cask (or use-package)

It's assumed in this tutorial you have modern package manager Cask installed.
You can install it by following the
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

After adding dependencies to your `Cask` file, run `cask install` in
that directory.

If instead of `Cask` you are using [use-package](https://github.com/jwiegley/use-package)
to manage your packages, then make sure that you have this in your `init.el` file:

```elisp
(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(require 'use-package)
```

Make sure that you install `use-package` from the package manager. Following tutorial
should be straightforward.

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

### ghc-mod

[Ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/) is a tool that
helps your editor become more close to an IDE. It lets you do things
like blazingly-fast error-reporting upon save, autocompletion and more.

First, install ghc-mod itself:

```
cabal install ghc-mod
```

Then, add this to your `Cask` file:

```elisp
(depends-on "ghc")
```

And this to your `~/.emacs`:

```
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
```

and extend your `my-haskell-mode-hook` with call to `(ghc-init)` like
this:

```elisp
(defun my-haskell-mode-hook ()
  (ghc-init)
  ; ... all previous contents
  )
```

After this is done, relaunch your Emacs (or re-eval config)
and open some `.hs`-file. Make a mistake and save the file, mistake
should now become underlined with red, and pressing `M-S-?` while
keeping curstor on top of it will show the error, and `M-n` `M-p`
should navigate between next/prev errors.

Check out
[ghc-mod homepage](http://www.mew.org/~kazu/proj/ghc-mod/en/) and
other resources to know more.

### TAGS

In order to get a "goto by name" functionality you can use standard
[Emacs TAGS support](https://www.gnu.org/software/emacs/manual/html_node/emacs/Tags.html). Haskell
has a special program, called `hasktags`:

```
cabal install hasktags
```

In order to generate tags, you can use `haskell-mode`'s `M-x
haskell-process-generate-tags`, or you can manually run `hasktags -e.`
in your project's root.

`haskell-mode` replaces standard `M-.` tag-search with it's own,
trying to search via ghci first, and only then via standard TAGS
mechanism.

### Auto-completion via company-ghc

Auto-completion via [company-ghc](https://github.com/iquiw/company-ghc) is pretty simple. Put this to your `Cask`:

```elisp
(depends-on "company")
(depends-on "company-ghc")
```

Then put this to your `.emacs`:

```elisp
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-ghc)
```

Voila! Beautiful auto-complete from `ghc-mod` works like a charm.

## Vim

Suggested plugins

* [Vimproc](https://github.com/Shougo/vimproc.vim) (required when installing ghcmod-vim)
* [ghcmod-vim](https://github.com/eagletmt/ghcmod-vim) - a wrapper for the multifaceted [ghc-mod](https://github.com/kazu-yamamoto/ghc-mod) command-line tool
* [vim2hs](https://github.com/dag/vim2hs) - A configurable collection of syntax highlighting and keyword shortcuts tailored for Haskell development
* [Syntastic](https://github.com/scrooloose/syntastic) - syntax checking and linting
* [neco-ghc](https://github.com/eagletmt/neco-ghc) - Haskell/GHC completion plugin
* [neocomplete](https://github.com/Shougo/neocomplete.vim) - General-purpose keyword completion engine, fed by neco-ghc
* [Cabal Context](https://github.com/timmytofu/vim-cabal-context) - Sets pwd to the directory of the nearest cabal file, useful for bouncing between different libraries
