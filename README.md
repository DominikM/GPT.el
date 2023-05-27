# GPT.el

An Emacs package for interacting with the GPT API. The default system message forces GPT to respond with Emacs lisp code which is evaluated by emacs.

## Demo
![demo of GPT.el](demo.gif)

## Installation

1. Download `gpt.el` and place it in your Emacs load-path.
2. Add `(require 'gpt)` to your Emacs configuration.

## Usage

1. Set your OpenAI API key with `(setq gpt-api-key "your-api-key")`.
2. Use `M-x gpt-message` to send a message to GPT and receive a response.
3. Use `M-x gpt-message-buffer` to send a message to GPT along with the current buffer's content.
4. Use `M-x gpt-reset` to reset the conversation with GPT.

## Customization

You can customize the following variables:
- `gpt-model`: The GPT model to use (default is "gpt-4").
- `gpt-temperature`: The sampling temperature (default is 0.2).
- `gpt-api-key`: Your OpenAI API key.

## Warning!!!
This package runs code directly from GPT without any safety checking or verification. IT SHOULD PROBABLY NOT BE USED FOR ANYTHING EVER. Of course, this isn't in the sense that a misaligned AI will take over the world through your emacs client, but rather at worse it'll do something stupid like `rm -rf /` and that's no one's fault but your own.

## License

This program is free software under the terms of the GNU General Public License version 3 or later.
