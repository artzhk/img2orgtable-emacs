# img2orgtable

Make a screenshot of a table, and get detected table right to your org notes.  
Works not perfect, but at least you get a structure to start.

![](./assets/demo.mp4)


## Table of Contents

- [img2orgtable](#img2orgtable)
  - [Installation](#installation)
  - [Basic usage](#basic-usage)
  - [Customization](#customization)
    - [`img2orgtable-python`](#img2orgtable-python)
    - [`img2orgtable-python-script`](#img2orgtable-python-script)
    - [`img2orgtable--screenshot-cmd`](#img2orgtable--screenshot-cmd)
    - [`img2orgtable-timeout`](#img2orgtable-timeout)
  - [Summary](#summary)

## Installation

_Prerequisites_:

- Python 3.7+
- Tesseract OCR – [Installation Guide](https://tesseract-ocr.github.io/tessdoc/Installation.html)

_Installation steps_:

1. Clone the repository
2. Install deps:
    ```bash
    make install
    ```
3. In your init file, add:
    ```emacs-lisp
    (add-to-list 'load-path "/path/to/img2orgtable")
    (require 'img2orgtable)
    ```

## Basic usage

The main entry point is the interactive command:

```emacs-lisp
(img2orgtable-screenshot-to-org)
```

Typical setup with a keybinding:

```emacs-lisp
(require 'img2orgtable)

(global-set-key (kbd "C-c t s") #'img2orgtable-screenshot-to-org)
```

Workflow:

1. Place point where you want the Org table.
2. Call `M-x img2orgtable-screenshot-to-org` (or your keybinding).
3. Use the screenshot UI that pops up to select a region with a table.
4. The screenshot is saved to a temporary PNG.
5. A Python script (`img2orgtable.py`) is run on that PNG.
6. The detected Org table is inserted at point.

On failure, a `user-error` is raised with the underlying command output.

## Customization

All options are under the `img2orgtable` customization group:

```emacs-lisp
M-x customize-group RET img2orgtable RET
```

### `img2orgtable-python`

Python interpreter used by `img2orgtable`.

```emacs-lisp
;; Use project-local venv:
(setq img2orgtable-python "/path/to/project/.venv/bin/python3")

;; Or rely on auto-detection (default):
(setq img2orgtable-python nil)
```

If `nil`, auto-detection order:

1. `.venv/bin/python3` under the `img2orgtable` install directory, if executable.
2. `python3` found in `PATH`.
3. `python` found in `PATH`.

If none are found, a `user-error` is signaled: `No suitable python found for img2orgtable`.

### `img2orgtable-python-script`

Path to the Python extractor script.

```emacs-lisp
;; Default (relative to img2orgtable.el location):
(setq img2orgtable-python-script "img2orgtable.py")

;; Custom absolute path:
(setq img2orgtable-python-script "/opt/img2org/bin/img2orgtable.py")
```

- If the value is relative, it is resolved relative to the directory containing `img2orgtable.el`.
- If absolute, it is used as-is.

### `img2orgtable--screenshot-cmd`

Override the screenshot command used to capture the region.

```emacs-lisp
;; Example: use Flameshot region and pipe PNG to file
(setq img2orgtable--screenshot-cmd "flameshot gui -r > %s")
```

- Default: `nil` → use built-in per-platform logic:

    - Linux + Wayland: `grim -g "$(slurp)" OUTFILE` via `sh -c`.
    - Linux + X11: `maim -s OUTFILE`.
    - Windows: PowerShell/.NET full-screen capture to `OUTFILE`.

- When non-nil, must be a shell command string.
- If the string contains `%s`, it is replaced with the shell-quoted output filename.
- If it does not contain `%s`, the shell-quoted filename is appended.

Examples:

```emacs-lisp
;; Custom script that writes PNG to a given path
(setq img2orgtable--screenshot-cmd "~/bin/my-screenshot-script %s")

;; Hypothetical macOS setup using screencapture:
(setq img2orgtable--screenshot-cmd "screencapture -i %s")
```

If the screenshot command exits non-zero, the command output is shown via:

```text
Screenshot failed: <output>
```

### `img2orgtable-timeout`

Maximum time (in seconds) to wait for external commands.

```emacs-lisp
(setq img2orgtable-timeout 20)  ;; default
```

Currently exposed as a defcustom for clamping misbehaving screenshot tools or Python calls; can be wired into timeout handling where needed.

## Summary

- Command: `img2orgtable-screenshot-to-org`
- Uses:

    - Screenshot backend (`img2orgtable--screenshot-cmd` or platform defaults)
    - Python interpreter (`img2orgtable-python` / auto-detection)
    - Extractor script (`img2orgtable-python-script`)

- Output: Org table inserted at point, suitable as a starting structure for manual cleanup.
