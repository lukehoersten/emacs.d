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
~/.emacs.d/
├── init.el           # Main configuration file
├── elisp/            # Custom configuration modules
│   ├── c-init.el     # C/C++ configuration
│   └── ansible-init.el # Ansible/YAML configuration
└── README.md         # This file
```

## Key Packages

### Completion Framework
- **vertico** - Vertical completion UI
- **orderless** - Flexible completion style (space-separated patterns)
- **consult** - Enhanced commands (buffer switching, search, etc.)
- **marginalia** - Helpful annotations in completion candidates

### Development
- **company** - Auto-completion
- **flycheck** - Syntax checking
- **jinx** - Fast spell-checking (requires enchant)
- **treesit-auto** - Tree-sitter modes with automatic grammar installation
- **magit** - Git interface
- **forge** - GitHub/GitLab integration for magit
- **magit-todos** - Show TODO/FIXME comments in magit status

### Editing
- **paredit** - Structured editing for Lisp
- **expand-region** - Smart region expansion
- **move-text** - Move lines/regions up and down
- **visual-regexp** - Visual feedback for regexp replace
- **yasnippet** - Snippet expansion
- **emmet-mode** - HTML/CSS abbreviation expansion
- **rainbow-delimiters** - Colorize nested delimiters

### Modes
- **markdown-mode** - Markdown editing
- **terraform-mode** - Terraform configuration files
- **yaml-mode** - YAML files
- **json-mode** - JSON files
- **hgignore-mode** - Mercurial ignore files

### Utilities
- **ibuffer-project** - Group buffers by project
- **exec-path-from-shell** - Import shell environment variables
- **rg** - ripgrep integration

## Key Bindings

### Custom Bindings
- `C-c c` - Compile
- `C-c r` - Recompile
- `C-c a` - Align regexp
- `C-c g` - Consult ripgrep (search with preview)
- `C-c s` - Launch eshell
- `C-x g` - Magit status
- `C-x C-b` - ibuffer (better buffer list)
- `C-=` - Expand region
- `M-p` / `M-n` - Move text up/down
- `M-%` - Visual regexp replace
- `C-M-%` - Visual regexp query replace

### Enhanced Default Bindings
- `M-x` - Command execution (enhanced with vertico)
- `C-x b` - Switch buffer (enhanced with consult)
- `M-y` - Yank from kill ring (enhanced with consult)
- `M-/` - Company completion

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

1. Clone this repository:
   ```bash
   git clone git@github.com:lukehoersten/emacs.d.git ~/.emacs.d
   ```

2. Launch Emacs - packages will auto-install on first run

3. Jinx spell-checker will compile its native module on first launch (requires enchant)

4. For tree-sitter modes, grammars install automatically when first opening a file

## Tree-sitter Support

Tree-sitter provides faster, more accurate syntax highlighting and parsing. Enabled automatically for:
- JavaScript/TypeScript
- JSON
- Python
- And many more (via `treesit-auto`)

Grammars install automatically on first use.

## Customizations

Emacs customizations are redirected to `custom.el` (gitignored) to keep `init.el` clean.
The configuration uses `package-require` for explicit package management.

## Updating Packages

```elisp
M-x package-refresh-contents
M-x package-update-all
```

## Notes

- Theme: Solarized Light (GUI only)
- Font: Inconsolata-12 (GUI only)
- Shell: eshell (launched automatically on startup)
- Whitespace cleanup on save enabled globally
