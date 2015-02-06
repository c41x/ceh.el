# ceh.el
Emacs C-Edit-Helper

Mode inspired by Paredit but for C-like languages.
This mode is highly experimental, there is some hardcoded stuff like keybindings, integration with key-chord.el and yasnippet.el so use at your own risk.

#Installation / Usage
You must have key-chord.el and yasnippet.el installed, then just add to init.el:
```
(add-to-list 'load-path "~/.emacs.d/ceh-folder")
(require 'ceh)

;; add hooks helper
(defun add-hooks (function hooks)
  "runs [function] for given [hooks]"
  (mapc (lambda (hook) (add-hook hook function)) hooks))

;; set inserting '{' behaviour, when ceh-brace-newline is nil open brace char
;; is inserted after newline
(setq ceh-brace-newline t)

;; add hooks
(add-hooks 'ceh-mode
	   '(cg-mode-hook
	     c-mode-hook
	     c++-mode-hook
	     js-mode-hook
	     js2-mode-hook
	     csharp-mode-hook))
```
... or just delete mode code (define-minor-mode... at the end of ceh.el) and bind keys by yourself.

#CEH prototype in action:
[![CEH Video](http://img.youtube.com/vi/IuTeucOo0cM/0.jpg)](http://www.youtube.com/watch?v=IuTeucOo0cM)

#Commands
* ceh-parametrize
* ceh-unparametrize
* ceh-stringize-line
* ceh-finish-expression
* ceh-new-brace
* ceh-transpose-atoms
* ceh-next-argument
* ceh-previous-argument
* ceh-leave-atom
* ceh-step-out-of-args
* ceh-step-in-args
* ceh-kill-line
* ceh-include-block
* ceh-exclude-block
* ceh-create-block
* ceh-expand
