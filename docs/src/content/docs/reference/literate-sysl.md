---
title: Literate Sysl (`.lsysl`)
description: Literate programming format mixing Markdown prose with indented Sysl source.
---

`.lsysl` files are **literate programming** sources — full Markdown documents that contain Sysl code as indented blocks. The compiler extracts (tangles) the code and discards the prose; the documentation toolchain renders (weaves) the prose with syntax-highlighted code.

## Format

- **Prose** starts at column 0 — it is standard Markdown.
- **Code** is indented (4+ spaces or 1+ tabs) — extracted as Sysl source.
- Fenced code blocks (triple backticks) in the prose are **not** extracted as code — they're documentation-only examples.

```lsysl
This is prose explaining the module.

    module std.mem

    copy(dst: []byte, src: []byte) -> int
        // ... implementation ...

More prose describing the next function.

    set(dst: []byte, val: byte)
        // ...
```

## Markdown features

The `.lsysl` renderer supports:

- **Headings** (`#` through `######`)
- **Paragraphs**, **bold** (`**text**`), **italic** (`*text*`)
- **Inline code** (backtick-delimited)
- **Fenced code blocks** with syntax highlighting (15+ languages including `sysl`, `python`, `javascript`, `rust`, `c`, `bash`, `json`)
- **Indented code blocks** (default to `sysl` highlighting)
- **Unordered and ordered lists** (with nesting)
- **Block quotes** (`>`)
- **Tables** (GFM-style pipe tables)
- **Horizontal rules** (`---`)
- **Links** (`[text](url)`)
- **HTML comments** (`<!-- -->`)
- **LaTeX math** via KaTeX — inline `\(x^2\)` and display `\[equation\]`

## Commands

```bash
sysl doc <file.lsysl>                   # render one file to HTML
sysl doc <directory>                    # render all .lsysl in directory + index
sysl doc --output <dir> <file.lsysl>    # specify output directory
```

## Tangling

When compiling, the `.lsysl` parser extracts all indented blocks as Sysl source, concatenating them in order. The extracted code is compiled identically to a `.sysl` file. Module declarations, imports, functions, and `#test` annotations all work inside `.lsysl` code blocks.

## See also

- [Modules and Imports](/reference/modules-and-imports/)
- [Attributes and Tests](/reference/attributes-and-testing/)
