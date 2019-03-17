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

### `(nerd-fonts icon-name)`

Return code point of `icon-name`, or insert it into buffer while called interactivelly.

### `M-x helm-nerd-fonts`

Insert & copy nerd-fonts icon with helm interface.

<p float="left" align="center">
  <img src="/helm-nerd-fonts_1.png" />
  <img src="/helm-nerd-fonts_2.png" />
</p>

### `M-x ivy-nerd-fonts`

Same as `helm-nerd-fonts` but with ivy interface.
