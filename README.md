# Emacs Configuration

Personal Emacs configuration focused on simplicity and modern tooling while respecting default Emacs keybindings.

## Philosophy

- Clean and minimal configuration
- Prefer built-in Emacs functionality when possible
- Use modern, actively maintained packages
- Respect default Emacs keybindings
- Single `init.el` file for simplicity

## Structure

```
~/.config/emacs/
├── init.el              # Main configuration file
├── elisp/               # Custom configuration modules
│   ├── c-init.el        # C/C++ configuration
│   └── hoersten-c-style.el  # Custom C style
├── custom.el            # Custom-set variables (gitignored)
└── README.md            # This file
```

## Key Packages

### Completion Framework
- **vertico** - Vertical minibuffer completion UI
- **orderless** - Flexible completion style (space-separated patterns)
- **consult** - Enhanced commands (buffer switching, search, yank-pop)
- **marginalia** - Helpful annotations in completion candidates
- **embark** - Contextual actions on completion candidates
- **corfu** - In-buffer completion popup (manual trigger)
- **cape** - completion-at-point extensions (dabbrev, file paths)

### Development
- **eglot** - Built-in LSP client (Python, JS/TS, C/C++, YAML, JSON, CSS/SCSS, HTML, Haskell)
- **flymake** - Built-in syntax checking (enabled in all `prog-mode` buffers)
- **jinx** - Fast spell-checking globally enabled in `text-mode`, `prog-mode`, and `conf-mode` (skips code identifiers, only checks comments/strings in code)
- **treesit-auto** - Tree-sitter modes with automatic grammar installation
- **magit** - Git interface
- **forge** - GitHub/GitLab integration for magit
- **magit-todos** - Show TODO/FIXME comments in magit status
- **diff-hl** - Inline git diff indicators in fringe (synced with magit)

### Editing
- **paredit** - Structured editing for Lisp
- **expand-region** - Smart region expansion
- **move-text** - Move lines/regions up and down
- **visual-regexp** - Visual feedback for regexp replace
- **emmet-mode** - HTML/CSS abbreviation expansion
- **rainbow-delimiters** - Colorize nested delimiters

### Search & Navigation
- **avy** - Jump to visible text by typing target chars
- **rg** - ripgrep results buffer
- **wgrep** - Editable grep/rg results buffers

### Modes
- **markdown-mode** - Markdown editing
- **terraform-mode** - Terraform/HCL configuration
- **json-reformat** - JSON pretty-printing
- **ansible-doc** - Ansible module documentation lookup
- **jinja2-mode** - Jinja2 template syntax
- **haskell-mode** - Haskell editing
- Built-in `css-mode` handles `.scss` files (auto-upgraded to `css-ts-mode` via treesit-auto)

### UI
- **solarized-theme** - Color theme
- **auto-dark** - Switch theme with macOS dark mode
- **helpful** - Better Help buffers with source links
- **which-key** - Built-in popup of key completions

### Utilities
- **ibuffer-project** - Group buffers by project
- **exec-path-from-shell** - Import shell environment variables
- **ghostel** - libghostty terminal emulator
- **claude-code-context** - Send Emacs buffer context to Claude Code (loaded from `~/Dev/code/git/elisp/claude-code-context`)

## Key Bindings

### Custom Bindings
- `C-c b` - Browse URL at point
- `C-c c` - Compile
- `C-c r` - Recompile
- `C-c a` - Align regexp
- `C-c g` - Consult ripgrep (search with preview)
- `C-c s` - Launch eshell
- `C-c C-l` - claude-code-context prefix (`u` update, `d` diagnostics, `c` clear, `m` toggle mode)
- `C-x g` - Magit status
- `C-x C-b` - ibuffer (better buffer list)
- `C-=` - Expand region
- `C-:` - Avy jump (2 chars)
- `C-,` - Embark act (contextual actions)
- `C-.` - Embark dwim (default action)
- `M-p` / `M-n` - Move text up/down
- `M-/` - Completion at point (corfu)

### Enhanced Default Bindings
- `M-x` - Command execution (enhanced with vertico)
- `C-x b` / `C-x 4 b` / `C-x 5 b` - Switch buffer (enhanced with consult)
- `M-y` - Yank from kill ring (enhanced with consult)
- `C-h f` / `C-h v` / `C-h k` / `C-h x` - Help (enhanced with helpful)

## Installation

### Prerequisites

**macOS:**
```bash
brew install enchant pkg-config
```

**Ubuntu/Debian:**
```bash
sudo apt install libenchant-2-dev pkg-config
```

### Setup

1. Clone this repository to `~/.config/emacs`:
   ```bash
   git clone git@github.com:lukehoersten/emacs.d.git ~/.config/emacs
   ```

2. Launch Emacs - packages will auto-install on first run

3. Jinx spell-checker will compile its native module on first launch (requires enchant)

4. Tree-sitter grammars install automatically on first use

## Tree-sitter Support

Tree-sitter provides faster, more accurate syntax highlighting and parsing. Enabled automatically via `treesit-auto` for JavaScript/TypeScript, JSON, Python, YAML, CSS, HTML, C/C++, and many more. Grammars install on first use.

## Customizations

Emacs customizations are redirected to `custom.el` (gitignored) to keep `init.el` clean.

## Updating Packages

```elisp
M-x package-refresh-contents
M-x package-update-all
```

## Notes

- Theme: Solarized, switches between light/dark with macOS dark mode (via `auto-dark`)
- Font: Inconsolata-12 (GUI only)
- Shell: eshell (launched automatically on startup)
- Server: emacs server starts automatically for `emacsclient`
- Whitespace cleanup on save enabled globally
- Line numbers and rainbow delimiters enabled in all `prog-mode` and `text-mode` buffers
- Auto-fill (line wrap at column 120) enabled in all `text-mode` buffers
