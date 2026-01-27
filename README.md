# claude-code-ide-helpers

Emacs extensions for [claude-code-ide](https://github.com/manzaltu/claude-code-ide.el) that help manage multiple Claude instances.

## Features

- **Window layout**: Arrange multiple Claude buffers with one main window and smaller stacked side windows
- **Buffer switcher**: Fuzzy-select which project to show as main (integrates with vertico/ivy)
- **Modeline indicator**: Shows `✓` or `✱` status based on output activity
- **Buffer cycling**: Rotate which buffer is the main (large) window
- **Session persistence**: Save and restore Claude sessions across Emacs restarts

## Installation

With `use-package` and `:vc` (Emacs 29+):

```elisp
(use-package claude-code-ide-helpers
  :vc (:url "https://github.com/parbo/claude-code-ide-helpers" :rev :newest)
  :demand t
  :bind (("C-c a l" . claude-code-ide-helpers-arrange-windows)
         ("C-c a n" . claude-code-ide-helpers-cycle-main)
         ("C-c a s" . claude-code-ide-helpers-switch-main)
         ("C-c a r" . claude-code-ide-helpers-restore-sessions))
  :config
  (add-hook 'vterm-mode-hook #'claude-code-ide-helpers-enable-status-mode))
```

### doom-modeline setup

If using doom-modeline, add a custom segment:

```elisp
(use-package doom-modeline
  :config
  (doom-modeline-def-segment claude-status
    "Display Claude Code status."
    (when (bound-and-true-p claude-code-ide-helpers-status-mode)
      (claude-code-ide-helpers--modeline-indicator)))
  (doom-modeline-def-modeline 'vterm
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process claude-status time))
  (add-hook 'vterm-mode-hook (lambda () (doom-modeline-set-modeline 'vterm))))
```

## Usage

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c a l` | `claude-code-ide-helpers-arrange-windows` | Layout with main + stacked side windows |
| `C-c a n` | `claude-code-ide-helpers-cycle-main` | Rotate which buffer is main |
| `C-c a s` | `claude-code-ide-helpers-switch-main` | Fuzzy-select main buffer by project name |
| `C-c a r` | `claude-code-ide-helpers-restore-sessions` | Restore sessions from last Emacs session |

The modeline indicator shows:
- `✓` (green) - Claude is waiting for input
- `✱` (orange) - Claude is generating output

## Customization

```elisp
;; Seconds of no output before showing ready (default: 5.0)
(setq claude-code-ide-helpers-idle-threshold 5.0)

;; File to store session directories (default: ~/.emacs.d/claude-sessions)
(setq claude-code-ide-helpers-session-file "~/.emacs.d/claude-sessions")
```

## License

MIT
