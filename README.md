[![Build Status](https://travis-ci.com/twlz0ne/nerd-fonts.el.svg?branch=master)](https://travis-ci.com/twlz0ne/nerd-fonts.el)

# nerd-fonts.el

Emacs [nerd-fonts](https://github.com/ryanoasis/nerd-fonts) utilities.

This project was inspired by [emacs-fontawesome](https://github.com/syohex/emacs-fontawesome).

## Requirements

- nerd-fonts (See [here](https://github.com/ryanoasis/nerd-fonts#font-installation) how to install)

## Installation

Clone this repository to `~/.emacs.d/site-lisp/nerd-fonts`. Add the following to your `.emacs`:

```elisp
(require 'nerd-fonts)
```

## Usage

Return a icon by giving name:

```elisp
(nerd-fonts "mdi-lambda")
;; => "ﬦ"
```

Insert a icon at point in interactive way:

<p float="left" align="center">
  <img src="/images/ido-1.png" />
</p>

More features with helm/ivy:

<p float="left" align="center">
  <img src="/images/helm-1.png" />
  <img src="/images/helm-2.png" />
</p>
