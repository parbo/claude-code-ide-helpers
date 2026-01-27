# claude-code-ide-helpers

Emacs extensions for [claude-code-ide](https://github.com/manzaltu/claude-code-ide.el) that help manage multiple Claude instances.

## Features

- **Window layout**: Arrange multiple Claude buffers with one main window and smaller stacked side windows
- **Modeline indicator**: Shows `[Ready]` or `[Working...]` status based on output activity
- **Buffer cycling**: Rotate which buffer is the main (large) window

## Installation

With `use-package` and `:vc` (Emacs 29+):

```elisp
(use-package claude-code-ide-helpers
  :vc (:url "https://github.com/parbo/claude-code-ide-helpers" :rev :newest)
  :after claude-code-ide
  :bind (("C-c C-l" . claude-code-ide-helpers-arrange-windows)
         ("C-c C-n" . claude-code-ide-helpers-cycle-main))
  :config
  (add-hook 'vterm-mode-hook #'claude-code-ide-helpers-enable-status-mode))
```

## Usage

- `C-c C-l` (`claude-code-ide-helpers-arrange-windows`): Create a layout with one main Claude buffer and smaller ones stacked on the right
- `C-c C-n` (`claude-code-ide-helpers-cycle-main`): Rotate which buffer is the main window

The modeline indicator automatically shows:
- `[Ready]` (green) - Claude is waiting for input
- `[Working...]` (orange) - Claude is generating output

## Customization

```elisp
;; Seconds of no output before showing [Ready] (default: 2.0)
(setq claude-code-ide-helpers-idle-threshold 2.0)
```

## License

MIT
