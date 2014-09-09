# SICP CS61A #

Exercises from Brian Harvey's [CS61A Volume 1][1] for
[SICP Support Group][2].

[1]: https://inst.eecs.berkeley.edu/~cs61a/reader/vol1.html
[2]: https://groups.google.com/forum/#!topic/sicp-support


## Install Dependencies ##

You will need Scheme 48:

    brew install scheme48 rust

and Rust:

    open http://www.rust-lang.org/install.html

and Haskell:

    brew install ghc cabal-install
    cabal install

## Set Up Emacs ##

Grab some custom modes:

    curl http://www.emacswiki.org/wiki/download/scheme48.el > ~/.emacs.d/scheme48.el

In emacs:

    M-x package-install rust-mode
    M-x package-install paredit

Add to `.emacs`:

    (autoload 'paredit-mode "paredit"
      "Minor mode for pseudo-structurally editing Lisp code."
      t)
