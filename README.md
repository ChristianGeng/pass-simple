# pass-simple

Simple Emacs utilities for managing passwords with [pass](https://www.passwordstore.org/) (password-store).

## Features

- **List, insert, remove entries** - Basic pass operations from Emacs
- **Idempotent upsert** - Only updates if secret differs
- **Bulk operations** - Import secrets from elisp files
- **Environment variables** - Set env vars from pass or auth-source
- **Fuzzy completion** - Integrates with consult/vertico if available

## Installation

### With straight.el

```elisp
(straight-use-package
 '(pass-simple :host github :repo "cgeng/pass-simple"))
```

### With Doom Emacs

In `packages.el`:

```elisp
(package! pass-simple
  :recipe (:host github :repo "cgeng/pass-simple"))
```

### Manual

Clone this repo and add to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/pass-simple")
(require 'pass-simple)
```

## Requirements

- Emacs 25.1+
- [pass](https://www.passwordstore.org/) CLI installed and initialized
- GPG configured for pass

### Optional

- `password-store.el` - Enhanced pass integration
- `consult` - Fuzzy completion for entry selection

## Usage

### Basic Operations

```elisp
;; List all entries
(pass-simple-list)

;; Insert a secret (prompts interactively)
(pass-simple-insert "path/to/secret" "mysecret")

;; Idempotent insert - skips if already set to same value
(pass-simple-upsert "path/to/secret" "mysecret")

;; Remove an entry
(pass-simple-remove "path/to/secret")
```

### Environment Variables

Set environment variables from pass entries:

```elisp
;; Set a single env var (tries pass, then auth-source, then existing value)
(pass-simple-set-env "OPENAI_API_KEY" "code/openai_api_key" "openai.com")

;; Define specs for bulk export
(setq pass-simple-secret-specs
  '((openai    :pass "code/openai_api_key"    :env "OPENAI_API_KEY")
    (anthropic :pass "code/anthropic_api_key" :env "ANTHROPIC_API_KEY")
    (xai       :pass "code/xai_api_key"       :env "XAI_API_KEY")))

;; Export all at once
(pass-simple-export-env)

;; Or run at startup
(add-hook 'after-init-hook #'pass-simple-export-env)
```

### Bulk Import

Import secrets from an encrypted elisp file:

```elisp
;; File: ~/.config/secrets.el.gpg
;; Contains: (setq my-secrets '(("path/one" . "secret1") ("path/two" . "secret2")))

(pass-simple-bulk-insert-from-file "~/.config/secrets.el.gpg" nil 'my-secrets)
```

## Interactive Commands

| Command | Description |
|---------|-------------|
| `M-x pass-simple-list` | List all pass entries |
| `M-x pass-simple-insert` | Insert a new secret |
| `M-x pass-simple-upsert` | Insert or update (idempotent) |
| `M-x pass-simple-remove` | Remove an entry |
| `M-x pass-simple-export-env` | Export secrets to env vars |
| `M-x pass-simple-bulk-insert-from-file` | Bulk import from file |

## License

MIT
