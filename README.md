# Features
TLA+ is known by programmers for having some strange syntax and that syntax can also be quite difficult to parse correctly.
So we rely on tree-sitter to provide top-notch information about the TLA+ file.

This major-mode for TLA+ has a combination of features from earlier versions made by others.
  * [Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based font-locking and indentation.
  * Comint-supported REPL development for interactive and iterative development of a specification.
  * A configuration & dispatch interface for working with TLA+'s model checking and formal verification tools

# Dependencies
  * Emacs 29+ for support from Emacs 29's **built-in** `treesit` package.
  * [tree-sitter-tlaplus](https://github.com/tlaplus-community/tree-sitter-tlaplus)
    - NOTE: The author uses tree-sitter 0.20.10 and tree-sitter-tlaplus 1.0.8.
      There are known ABI compatibility problems between tree-sitter versions.
      The primary author uses [Guix](https://guix.gnu.org/) as their primary operating system and package manager, and the versions of the tree-sitter packages currently in their repository is known to work.

# Previous Versions
There have been a variety of previous major-modes created for TLA+ in Emacs.
But, none of them seemed maintained and did not have all the features I wanted.
So, I put this major-mode together in the hopes that it is useful to me and to others.

I want to thank these previous pioneers in working on this field, in no particular order:
  * [bch](https://git.sdf.org/bch/tlamode)
  * [carlthuringer](https://github.com/carlthuringer/tla-mode)
  * [Davidbrcz](https://github.com/Davidbrcz/tla-ts-mode)
