# GPT.el #

This is a simple package to aid in communicating with GPT-4 from emacs. There are 3 commands to use:
* `gpt-message`: send a message to GPT-4, and it will respond with an emacs lisp form which will be evaluated.
* `gpt-message-buffer`: send a message to GPT-4 along with the text of the current buffer.
* `gpt-reset`: reset the current conversation log.

This package runs code provided from GPT-4 without any confirmation or safety checking, so in reality it shouldn't be used for anything ever.
